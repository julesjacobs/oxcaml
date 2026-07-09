(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/htbl.mli ../lib/htbl.ml ../lib/bslice.mli ../lib/bslice.ml ../lib/mhtbl.mli ../lib/mhtbl.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Client of the IMPERATIVE hash table (lib/mhtbl): a uniquely owned
   mutable bucket array whose ghost is the immutable model, mutated
   IN PLACE by [add].  The reasoning is word-for-word the immutable
   client's (demo/lean_htbl.ml) -- the same [tfind_madd_eq] /
   [tfind_madd_ne] / [tfind_eq_jump] theorems arrive through Htbl's
   .cmi and apply to the mutable table's ghost -- only here every
   [add] rewrites one bucket of one array, and ownership threads
   through each call [@ unique] so no alias can observe a write.
   Keys 3 and 11 COLLIDE (both hash to bucket 3), so the hits
   exercise a real bucket with two entries, in-place. *)

open Htbl
open Mhtbl

let demo : opt{ _ = Found 7 } * opt{ _ = Found 5 } * opt{ _ = Missing } =
  let h = create () in
  let h = add 3 7 h in
  let h = add 11 5 h in
  let (hit3, h) = find 3 h in
  let (hit11, h) = find 11 h in
  let (miss, h) = find 4 h in
  ignore h;
  (hit3, hit11, miss)
