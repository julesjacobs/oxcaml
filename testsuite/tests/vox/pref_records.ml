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
  let refine_ t = Pref.empty () in
  let refine_ l = Pref.alloc empty t in
  let left = l.value in
  let t = l.state in
  let refine_ r = Pref.alloc empty t in
  let right = r.value in
  let t = r.state in
  let node = {value = 42; left; right} in
  let refine_ result = Pref.alloc node t in
  let p = result.value in
  let t = result.state in
  let t : {t : Pref.token | Pref.Heap.mem (Pref.own t) p} = refine_ t in
  let refine_ actual = Pref.read p t in
  assert (actual.value = 42)

let () =
  let callback = { f = fun () -> () } in
  let refine_ t = Pref.empty () in
  let refine_ result = Pref.alloc callback t in
  let p = result.value in
  let t = result.state in
  let t : {t : Pref.token | Pref.Heap.mem (Pref.own t) p} = refine_ t in
  let refine_ callback = Pref.read p t in
  callback.f ()
