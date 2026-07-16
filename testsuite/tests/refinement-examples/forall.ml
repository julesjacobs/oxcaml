(* TEST
 modules = "vox_spec.ml";
 setup-ocamlc.byte-build-env;
 module = "vox_spec.ml";
 ocamlc.byte;
 module = "";
 flags += " -I ${test_build_directory}/ocamlc.byte";
 expect;
*)

(* [forall_] is an ordinary function applied to an ordinary single-parameter
   lambda.  Equality on known integers is total today, so this example does
   not depend on the deferred ordering comparisons.

   FINAL: the backend recognizes [forall_] and proves the quantified result.
   CURRENT: predicate modes are stubbed, so the refined-value hole remains
   partial while the ordinary [forall_] argument requires a total lambda. *)

#load "vox_spec.cmo";;

(* @ex id=forall_unique_identity final=ACCEPT today=REJECT stable=no unlocks=modes+verification *)
let unique_identity (x : int)
    : int{
        Vox_spec.forall_
          (fun z -> Vox_spec.implies (z = x) (_ = z))
      }
  =
  x

[%%expect {|
Line 4, characters 46-47:
4 |           (fun z -> Vox_spec.implies (z = x) (_ = z))
                                                  ^
Error: The value "_" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 4, characters 10-53
         which is expected to be "total".
|}]
