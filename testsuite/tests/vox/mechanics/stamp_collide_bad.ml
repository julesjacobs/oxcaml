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
   expected codomain as [p + p]: on a FAILED arrow unify the binder-
   alignment rename (ctype [unify]) has already substituted one arrow's
   binder into the other's codomain, so the printer shows the aligned
   name -- cosmetic, and orthogonal to the operator mismatch that
   drives the error. *)

let bad (b : bool) = if b then Collide_a.dep2 else Collide_b.dep2
