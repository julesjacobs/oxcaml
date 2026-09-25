(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml mode_solver_atomic.ml mode_solver_atomic_store.ml";
 { bytecode; }
 { native; }
*)

open Mode_solver_atomic
module H = Pref.Heap

let () =
  let before = { lower = 0; upper = 2; self_edge = false } in
  ghost_ (valid_def before);
  let before : {s : state | valid s} = before in
  let empty = Pref.empty () in
  let allocated = Pref.alloc before empty in
  let p = allocated.value in
  let t = allocated.state in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let observed : {s : state | H.at h p === Some s && valid s} =
    let borrowed = borrow_ t in
    let borrowed : {b : Pref.token | H.mem (Pref.own b) p} =
      borrowed in
    let value = Pref.read p borrowed in
    value
  in
  let restored =
    match add_self_edge observed with
    | Failure _ ->
      let restored : {r : Pref.token | Pref.own r === h} @ unique =
        t in
      restored
    | Success (_, after) ->
      let t : {t : Pref.token |
        Pref.own t === h && H.mem (Pref.own t) p} = t in
      let changed = Pref.write p after t in
      let changed : {t : Pref.token |
        Pref.own t === H.put h p after && H.mem (Pref.own t) p} =
        changed in
      let restored = Pref.write p observed changed in
      let proof = ghost_ (
        let law = H.put_law h p after observed in
        let identity = H.put_law h p observed observed in
        let u = () in
        let proof : {u : unit |
          H.put (H.put h p after) p observed === h} = u in
        proof) in
      let proof = proof in
      let restored : {r : Pref.token | Pref.own r === h} @ unique =
        restored in
      restored
  in
  let restored : {r : Pref.token | Pref.own r === h} @ unique =
    restored in
  let borrowed = borrow_ restored in
  let borrowed : {b : Pref.token | H.mem (Pref.own b) p} =
    borrowed in
  let observed = Pref.read p borrowed in
  if observed <> before then failwith "rollback changed the solver state"
