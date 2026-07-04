(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/ptrie_packed.mli ../lib/ptrie_packed.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Client of the PACKED Patricia trie -- the compiler's own
   representation, one [prefix_and_bit] int per branch.  The API is
   the same model story as the little-endian toy: [mem_insert]
   characterizes every insertion, with neither a bit nor a pack in
   sight. *)

open Ptrie_packed

let demo : bool{ _ } * bool{ not _ } =
  let e = empty in
  let t1 = insert 5 e in
  let t2 = insert 4 t1 in
  let t3 = insert 1 t2 in
  let hit = mem 4 t3 in
  let miss = mem 7 t3 in
  (hit, miss)
