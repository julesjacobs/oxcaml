(* TEST
 modules = "vox_spec.ml";
 setup-ocamlc.byte-build-env;
 module = "vox_spec.ml";
 ocamlc.byte;
 module = "";
 flags += " -I ${test_build_directory}/ocamlc.byte";
 expect;
*)

(* FINAL: recursive results provide induction hypotheses, while the argument
   contract proves recursive calls stay nonnegative.  CURRENT: the refined
   parameter/result types are written through the prelude wrappers
   [Vox_spec.int_le]/[Vox_spec.int_ge], ordinary (partial) user functions -- not
   the comparison primitives admitted inside a predicate.  A predicate is checked
   at [total], so forming the refinement type calls the partial wrapper and is
   rejected at totality, before any verification obligation is generated.  When
   total comparisons make the wrappers total-annotatable the predicate flows
   through to verification again; the [unlocks] tag records that dependency. *)

#load "vox_spec.cmo";;

(* @ex id=fib_nonnegative final=ACCEPT today=REJECT stable=no unlocks=total-comparisons+verification *)
let rec fib (n : int{ Vox_spec.int_ge _ 0 })
    : int{ Vox_spec.int_ge _ 0 }
  =
  if Vox_spec.int_le n 0
  then 0
  else if n = 1
  then 1
  else fib (n - 1) + fib (n - 2)

[%%expect {|
Line 1, characters 22-37:
1 | let rec fib (n : int{ Vox_spec.int_ge _ 0 })
                          ^^^^^^^^^^^^^^^
Error: The value "Vox_spec.int_ge" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 1, characters 17-43).
|}]
