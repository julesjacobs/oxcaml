(* TEST
 modules = "vox_spec.ml";
 setup-ocamlc.byte-build-env;
 module = "vox_spec.ml";
 ocamlc.byte;
 module = "";
 flags += " -I ${test_build_directory}/ocamlc.byte";
 expect;
*)

(* This avoids the unresolved bare-implementation direction.  The
   implementation result [_ = x * x] and interface result [Vox_spec.int_ge _ 0]
   are both refined; FINAL sealing proves the directed implication.  CURRENT: the
   interface predicate is written through the prelude wrapper [Vox_spec.int_ge],
   an ordinary (partial) user function -- not one of the comparison primitives
   admitted inside a predicate.  A predicate is checked at [total], so forming
   the interface refinement type calls the partial wrapper and is rejected at
   totality, before the seal VC engages.  When total comparisons make the wrapper
   total-annotatable the seal's directed-implication VC engages again; the
   [unlocks] tag records that dependency. *)

#load "vox_spec.cmo";;

(* @ex id=seal_square_nonnegative final=ACCEPT today=REJECT stable=no unlocks=total-comparisons+verification *)
module Square : sig
  val square : int -> int{ Vox_spec.int_ge _ 0 }
end = struct
  let square (x : int) = (x * x : int{ _ = x * x })
end

[%%expect {|
Line 2, characters 27-42:
2 |   val square : int -> int{ Vox_spec.int_ge _ 0 }
                               ^^^^^^^^^^^^^^^
Error: The value "Vox_spec.int_ge" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 2, characters 22-48).
|}]
