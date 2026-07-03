(* TEST
 flags = "-dump-vc -vox-dry-run";
 modules = "collide_a.ml collide_b.ml";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Cross-unit dependent arrows whose .cmi-marshalled binder stamps
   COLLIDE (the aux modules are shaped identically so their separate
   compiler runs mint the same stamps; see collide_a.ml).  This must
   be invisible: binder references are alpha-compared under the arrow
   pairing, never by raw stamp across units.  Exercised here:

   - unifying the two units' alpha-equivalent signatures;
   - opening each unit's binder at a dependent application, where the
     substitution must hit exactly its own arrow's references. *)

let pick (b : bool) = if b then Collide_a.dep else Collide_b.dep

let use (n : {v:int | v = 5}) : {v:int | v = 5} * {v:int | v = 25} =
  let refine_ a = Collide_a.dep n in
  let refine_ m = Collide_b.dep2 n n in
  ((refine_ a : {v:int | v = 5}), (refine_ m : {v:int | v = 25}))
