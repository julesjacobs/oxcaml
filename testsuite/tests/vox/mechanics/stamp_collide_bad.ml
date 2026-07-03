(* TEST
 flags = "-vox-dry-run";
 modules = "collide_a.ml collide_b.ml";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Negative side of stamp_collide.ml: the two units' SECOND dependent
   signatures have colliding binder stamps but different predicates
   (p + q vs r * s); colliding stamps must not make them unify. *)

let bad (b : bool) = if b then Collide_a.dep2 else Collide_b.dep2
