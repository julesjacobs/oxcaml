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
  let b = Pref.equal p alias in b

let () =
  let value = 42 in
  let t = Pref.empty () in
  let a = Pref.alloc value t in
  let p = a.value in let t = a.state in
  let b = Pref.alloc value t in
  let q = b.value in let t = b.state in
  let different = Pref.equal p q in
  let proof : {u : unit | not different} = () in
  let _ = proof in
  assert (not different);
  let same = equal_alias p in
  assert same;
  let t : {t : Pref.token | Pref.Heap.mem (Pref.own t) p} = t in
  let other = 7 in
  let t = Pref.write p other t in
  let different = Pref.equal p q in
  assert (not different);
  let t : {t : Pref.token | Pref.Heap.mem (Pref.own t) q} = t in
  let contents = Pref.read q t in
  assert (contents = 42)
