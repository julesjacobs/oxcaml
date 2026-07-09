(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/pset.mli ../lib/pset.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* STAGE 4: PARAMETERIZED [via] across a module boundary.  [pset]
   declares [type 'a t : value refines ('a iset)] over a parameterized
   ghost sort and PROVES [add] honestly (no [assume_unchecked_]) at the
   generic element sort.  A client instantiates at [int]: it binds
   [int Pset.t] at the concrete set sort [(ISet Int)] (the argument sort
   [Int] flows through the [refines] kind), imports [ISet]/[mem]/[ins]
   from the unit's VoxSig, and proves a membership fact THROUGH the
   abstraction with no view of the tree.  [add x s = ins x s], and the
   exported [mem_ins] lemma closes [mem x (ins x s)]. *)

open Pset

let member_after_add : (x : int) -> (s : int t) -> int t{ mem x _ } =
  fun x s -> add x s
