(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/htbl.mli ../lib/htbl.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Client of the specced hash table: the model and its theorems arrive
   through Htbl's .cmi (no prelude flag anywhere).  A table built by
   [add]s PROVABLY binds what was inserted, and a miss is Missing --
   both facts flow from the exported [tfind_madd_eq] / [tfind_madd_ne],
   with [find] itself equal to the whole-table scan [tfind] by the
   one-bucket theorem.  Keys 3 and 11 COLLIDE (both hash to bucket 3),
   so the hit below exercises a real bucket with two entries. *)

open Htbl

let demo : opt{ _ = Found 7 } * opt{ _ = Found 5 } * opt{ _ = Missing } =
  let t1 = add 3 7 empty in
  let t2 = add 11 5 t1 in
  let hit3 = find 3 t2 in
  let hit11 = find 11 t2 in
  let miss = find 4 t2 in
  (hit3, hit11, miss)
