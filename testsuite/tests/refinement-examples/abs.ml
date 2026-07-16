(* TEST
 modules = "vox_spec.ml";
 setup-ocamlc.byte-build-env;
 module = "vox_spec.ml";
 ocamlc.byte;
 module = "";
 flags += " -I ${test_build_directory}/ocamlc.byte";
 expect;
*)

(* FINAL: verification proves that either branch is nonnegative.
   CURRENT: the predicate is written through the prelude wrapper
   [Vox_spec.int_ge], an ordinary (partial) user function -- not one of the
   comparison primitives admitted inside a predicate.  A predicate is checked at
   [total], so calling the partial wrapper is rejected at totality, before any
   verification obligation is generated.  When total comparisons make the
   wrapper total-annotatable the predicate flows through to verification again;
   the [unlocks] tag records that dependency. *)

#load "vox_spec.cmo";;

(* @ex id=abs_nonnegative final=ACCEPT today=REJECT stable=no unlocks=total-comparisons+verification *)
let abs (x : int) =
  (if Vox_spec.int_ge x 0 then x else 0 - x
    : int{ Vox_spec.int_ge _ 0 })

[%%expect {|
Line 3, characters 11-26:
3 |     : int{ Vox_spec.int_ge _ 0 })
               ^^^^^^^^^^^^^^^
Error: The value "Vox_spec.int_ge" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 3, characters 6-32).
|}]
