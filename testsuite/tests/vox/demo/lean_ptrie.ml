(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/ptrie.mli ../lib/ptrie.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Client of the specced Patricia trie: the model and its theorems
   arrive through Ptrie's .cmi.  A trie built by inserts provably
   contains what was inserted, and the miss is exactly as proved as
   the hits -- [mem_insert] characterizes each insertion completely,
   with no bit in sight. *)

open Ptrie

let demo : bool{ _ } * bool{ not _ } =
  let e = empty in
  let t1 = insert 5 e in
  let t2 = insert 4 t1 in
  let t3 = insert 1 t2 in
  let hit = mem 4 t3 in
  let miss = mem 7 t3 in
  (hit, miss)
