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
   contract proves recursive calls stay nonnegative.  CURRENT: the verification
   pass generates the result obligation, but the comparison wrappers
   [Vox_spec.int_le]/[Vox_spec.int_ge] are partial and therefore opaque to the
   solver, so the guard and the nonnegativity goal cannot be related; the
   obligation is not proved until total comparisons land. *)

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
Lines 4-8, characters 2-32:
4 | ..if Vox_spec.int_le n 0
5 |   then 0
6 |   else if n = 1
7 |   then 1
8 |   else fib (n - 1) + fib (n - 2)
Error: Refinement verification failed (not-proved)
|}]
