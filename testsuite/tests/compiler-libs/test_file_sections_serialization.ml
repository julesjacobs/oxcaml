(* TEST
 include ocamlcommon;
 flags = "-I ${ocamlsrcdir}/utils";
 native;
*)

module S = File_sections

type node = { value : int; mutable next : node option }
type graph = { root : node; shared : node * node; payload : int array }

let graph n =
  let root = { value = n; next = None } in
  root.next <- Some root;
  { root; shared = (root, root); payload = [| n; n + 1; n + 2 |] }

let add_prefix builder =
  let first = S.Builder.add builder (Obj.repr (graph 41)) in
  let table = Hashtbl.create ~random:false 7 in
  List.iter (fun n -> Hashtbl.add table n (graph n)) [ 3; 17; 41; 64 ];
  let second = S.Builder.add builder (Obj.repr table) in
  (first, second)

let raw_sections sections =
  let bytes, offsets, length = S.serialize sections in
  let running = ref 0 in
  Array.iteri
    (fun i data ->
      assert (offsets.(i) = !running);
      running := !running + String.length data)
    bytes;
  assert (!running = length);
  (bytes, offsets, length)

let () =
  let baseline = S.Builder.create 0 in
  let baseline_first, baseline_second = add_prefix baseline in
  let baseline_static = S.Builder.add baseline (Obj.repr [| 7; 9; 11 |]) in
  let expected = raw_sections (S.Builder.build baseline) in
  let early = S.Builder.create 0 in
  let first, second = add_prefix early in
  assert (first = baseline_first && second = baseline_second);
  let earlier_snapshot = S.Builder.build early in
  S.Builder.serialize early;
  S.Builder.serialize early;
  let static = S.Builder.add early (Obj.repr [| 7; 9; 11 |]) in
  assert (static = baseline_static);
  let sections = S.Builder.build early in
  assert (S.length earlier_snapshot = 2);
  assert (S.length sections = 3);
  assert (raw_sections sections = expected);
  let decoded : graph = Obj.obj (S.get sections first) in
  let previous : graph = Obj.obj (S.get earlier_snapshot first) in
  assert (decoded == previous);
  assert (S.get sections first == S.get sections first);
  let a, b = decoded.shared in
  assert (a == b && a == decoded.root);
  (match decoded.root.next with
  | Some next -> assert (next == decoded.root)
  | None -> assert false);
  decoded.payload.(0) <- 1234;
  let changed, _, _ = raw_sections sections in
  assert (changed.(0) = Marshal.to_string decoded []);
  assert (
    changed.(0)
    <>
    let bytes, _, _ = expected in
    bytes.(0));
  S.Builder.serialize early;
  assert (raw_sections (S.Builder.build early) = raw_sections sections);
  let table : (int, graph) Hashtbl.t = Obj.obj (S.get sections second) in
  assert ((Hashtbl.find table 64).payload.(0) = 64);
  let raw = S.get sections static in
  let array : int array = Obj.obj raw in
  assert (array = [| 7; 9; 11 |]);
  S.Builder.clear early;
  let restarted = S.Builder.add early (Obj.repr "new") in
  assert (restarted = first);
  assert (S.length (S.Builder.build early) = 1);
  assert (S.length sections = 3);
  assert ((Obj.obj (S.get sections first) : graph).payload.(0) = 1234);
  ()

let () =
  let builder = S.Builder.create 0 in
  let value = graph 77 in
  let index = S.Builder.add builder (Obj.repr value) in
  let first = S.Builder.build builder in
  let second = S.Builder.build builder in
  assert (S.get first index == Obj.repr value);
  assert (S.get first index == S.get second index);
  value.payload.(0) <- 999;
  assert (fst value.shared == snd value.shared);
  let encoded, _, _ = raw_sections first in
  assert (encoded.(0) = Marshal.to_string value []);
  let input = [| Obj.repr value |] in
  let copied = S.from_array input in
  input.(0) <- Obj.repr "changed";
  assert (S.get copied index == Obj.repr value);
  ()

let[@inline never] add_weak_graph builder n =
  let value = graph (Sys.opaque_identity n) in
  let weak = Weak.create 1 in
  Weak.set weak 0 (Some value);
  let index = S.Builder.add builder (Obj.repr value) in
  (index, weak)

let () =
  let builder = S.Builder.create 0 in
  let index, weak =
    (Sys.opaque_identity add_weak_graph) builder (Sys.opaque_identity 314159)
  in
  let snapshot = S.Builder.build builder in
  Gc.full_major ();
  assert (Weak.check weak 0);
  S.Builder.serialize builder;
  Gc.full_major ();
  assert (not (Weak.check weak 0));
  let later = S.Builder.add builder (Obj.repr 271828) in
  let final = S.Builder.build builder in
  let decoded : graph = Obj.obj (S.get final index) in
  assert (decoded.payload.(0) = 314159);
  assert (S.get snapshot index == S.get final index);
  assert ((Obj.obj (S.get final later) : int) = 271828);
  ()

let () =
  let builder = S.Builder.create 0 in
  let first, second = add_prefix builder in
  S.Builder.serialize builder;
  let final = S.Builder.build builder in
  let expected, offsets, _ = raw_sections final in
  let file = Filename.temp_file "file-sections-early-serialization" ".bin" in
  let oc = open_out_bin file in
  output_string oc "prefix";
  Array.iter (output_string oc) expected;
  close_out oc;
  let ic = open_in_bin file in
  let disk = S.create offsets file ic ~first_section_offset:6 in
  let a : graph = Obj.obj (S.get disk first) in
  assert (a.payload.(0) = 41);
  assert (S.get disk first == S.get disk first);
  let table : (int, graph) Hashtbl.t = Obj.obj (S.get disk second) in
  assert ((Hashtbl.find table 17).root.value = 17);
  let actual, _, _ = raw_sections disk in
  assert (actual = expected);
  a.payload.(0) <- 606;
  let changed, _, _ = raw_sections disk in
  assert (changed.(0) = Marshal.to_string a []);
  Sys.remove file;
  ()

let () =
  let builder = S.Builder.create 0 in
  let shared = [| 10; 20 |] in
  let first = S.Builder.add builder (Obj.repr shared) in
  let second = S.Builder.add builder (Obj.repr shared) in
  let snapshot = S.Builder.build builder in
  assert (S.get snapshot first == S.get snapshot second);
  let before = S.get snapshot first in
  S.Builder.serialize builder;
  let a : int array = Obj.obj (S.get snapshot first) in
  let b : int array = Obj.obj (S.get snapshot second) in
  assert (Obj.repr a != before);
  assert (a != b);
  assert (a = b);
  assert (S.get snapshot first == Obj.repr a);
  ()

let () =
  let builder = S.Builder.create 0 in
  let first = S.Builder.add builder (Obj.repr [| 101 |]) in
  let closure = S.Builder.add builder (Obj.repr (fun n -> n + 1)) in
  let last = S.Builder.add builder (Obj.repr [| 303 |]) in
  let snapshot = S.Builder.build builder in
  let first_before = S.get snapshot first in
  let last_before = S.get snapshot last in
  (match S.Builder.serialize builder with
  | exception Invalid_argument _ -> ()
  | _ -> assert false);
  (match S.Builder.serialize builder with
  | exception Invalid_argument _ -> ()
  | _ -> assert false);
  assert ((Obj.obj (S.get snapshot first) : int array) = [| 101 |]);
  assert (S.get snapshot first != first_before);
  assert (S.get snapshot last == last_before);
  assert ((Obj.obj (S.get snapshot closure) : int -> int) 9 = 10);
  let appended = S.Builder.add builder (Obj.repr [| 404 |]) in
  let appended_value : int array =
    Obj.obj (S.get (S.Builder.build builder) appended)
  in
  assert (appended_value = [| 404 |]);
  ()

let () = print_endline "ok"
