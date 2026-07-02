(* TEST
 flags = "-vox-dry-run";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A refined and an unrefined branch do not unify: refinements never
   flow through unification (soundness: which branch's refinement
   "won" would be an implementation accident).  DESIGN.md's required
   counterexample. *)

let f (b : {v:int | v > 0}) (c : bool) = if c then 0 else b
