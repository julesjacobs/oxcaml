(* TEST
 flags = "-vox-dry-run";
 modules = "collide_a.ml collide_b.ml";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Negative side of stamp_collide.ml: the two units' SECOND dependent
   signatures have different predicates (p + q vs r * s) and must not
   unify.  Since imported binders are now freshened disjoint at
   signature load, the historical stamp COLLISION this pair was shaped
   to force no longer reaches the consuming unit; the rejection stands
   on the differing operators (Add vs Mul).  The message renders the
   expected codomain faithfully as [p + q]: because imported binders are
   now freshened DISJOINT from every stamp already in the codomain
   (Subst), the binder-alignment rename on a FAILED arrow unify no longer
   aliases the two parameters (the historical [p + p] artifact is gone),
   leaving the operator mismatch as the sole, honest driver. *)

let bad (b : bool) = if b then Collide_a.dep2 else Collide_b.dep2
