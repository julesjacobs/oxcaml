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
   contract proves recursive calls stay nonnegative.  CURRENT: typing reaches
   the refined result but rejects a bare branch result; no verification pass
   exists to discharge it. *)

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
Line 5, characters 7-8:
5 |   then 0
           ^
Error: The constant "0" has type "int" but an expression was expected of type
         "int{ (app[Vox_spec!.int_ge] _ 0) }"
|}]
