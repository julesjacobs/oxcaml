(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml pref_equal.ml";
 { bytecode; }
 { native; }
*)

let equal_alias (p : int Pref.t @ immutable) : {b : bool | b} =
  let alias = p in
  let refine_ b = Pref.equal p alias in refine_ b

let () =
  let value = 42 in
  let refine_ t = Pref.empty () in
  let refine_ a = Pref.alloc value t in
  let p = a.value in let t = a.state in
  let refine_ b = Pref.alloc value t in
  let q = b.value in let t = b.state in
  let refine_ different = Pref.equal p q in
  let proof : {u : unit | not different} = let u = () in refine_ u in
  let refine_ proof = proof in
  assert (not different);
  let refine_ same = equal_alias p in
  assert same;
  let t : {t : Pref.token | Pref.Heap.mem (Pref.own t) p} = refine_ t in
  let other = 7 in
  let refine_ t = Pref.write p other t in
  let refine_ different = Pref.equal p q in
  assert (not different);
  let t : {t : Pref.token | Pref.Heap.mem (Pref.own t) q} = refine_ t in
  let refine_ contents = Pref.read q t in
  assert (contents = 42)
