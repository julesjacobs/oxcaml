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
   CURRENT: the body is checked against the refinement's skeleton [int] and the
   binding is assigned the refined type; no verification-condition obligation is
   stored at this tip (the discharged VC arrives with the verification pass). *)

#load "vox_spec.cmo";;

(* @ex id=abs_nonnegative final=ACCEPT today=ACCEPT stable=no unlocks=total-comparisons+verification *)
let abs (x : int) =
  (if Vox_spec.int_ge x 0 then x else 0 - x
    : int{ Vox_spec.int_ge _ 0 })

[%%expect {|
val abs : int -> int{ (app[Vox_spec!.int_ge] _ 0) } = <fun>
|}]
