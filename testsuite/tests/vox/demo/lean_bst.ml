(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "bst.mli bst.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Client of the specced BST: the model and its theorems arrive
   through Bst's .cmi (no prelude flag anywhere).  A tree built by
   inserts PROVABLY contains what was inserted -- and the miss below
   is exactly as proved as the hits: [mem_insert] characterizes each
   insertion completely, down to [mem 5 empty = False]. *)

open Bst

let demo : bool{ _ } * bool{ not _ } =
  let e = empty in
  let t1 = insert 2 e in
  let t2 = insert 1 t1 in
  let t3 = insert 3 t2 in
  let hit = member 2 t3 in
  let miss = member 5 t3 in
  (hit, miss)
