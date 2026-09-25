(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml pref_records.ml";
 { bytecode; }
 { native; }
*)

type node : immutable_data = {
  value : int;
  left : node option Pref.t;
  right : node option Pref.t;
}

type callback = {
  f : unit -> unit @@ many forkable unyielding total immutable
}

let () =
  let empty : node option = None in
  let t = Pref.empty () in
  let l = Pref.alloc empty t in
  let left = l.value in
  let t = l.state in
  let r = Pref.alloc empty t in
  let right = r.value in
  let t = r.state in
  let node = {value = 42; left; right} in
  let result = Pref.alloc node t in
  let p = result.value in
  let t = result.state in
  let t : {t : Pref.token | Pref.Heap.mem (Pref.own t) p} = t in
  let actual = Pref.read p t in
  assert (actual.value = 42)

let () =
  let callback = { f = fun () -> () } in
  let t = Pref.empty () in
  let result = Pref.alloc callback t in
  let p = result.value in
  let t = result.state in
  let t : {t : Pref.token | Pref.Heap.mem (Pref.own t) p} = t in
  let callback = Pref.read p t in
  callback.f ()
