(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/oset.mli ../lib/oset.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The abstraction is real: this claim is TRUE of the concrete
   representation (a fresh element is never in a one-element tree, so
   [member 5] returns false and [_ = true] fails -- but even [_ =
   false] would need the axioms), yet the goal below is not decided by
   the interface's axioms, so the client cannot establish it.  What
   the .mli hides, no client proof can see. *)

open Oset

let cheat : bool{ _ = true } =
  let t1 = insert 2 empty in
  member 5 t1
