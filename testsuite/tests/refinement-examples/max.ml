(* TEST
 modules = "vox_spec.ml";
 setup-ocamlc.byte-build-env;
 module = "vox_spec.ml";
 ocamlc.byte;
 module = "";
 flags += " -I ${test_build_directory}/ocamlc.byte";
 expect;
*)

(* FINAL: verification uses the branch condition to prove that the result is
   at least both arguments.  CURRENT: the marked result obligation is not yet
   discharged.  The conjunction is the ordinary boolean [&&]. *)

#load "vox_spec.cmo";;

(* @ex id=max_upper_bound final=ACCEPT today=ACCEPT stable=no unlocks=total-comparisons+verification *)
let max (x : int) (y : int) =
  (if Vox_spec.int_ge x y then x else y
    : int{
        Vox_spec.int_ge _ x
        && Vox_spec.int_ge _ y
      })

[%%expect {|
val max :
  int ->
  int ->
  int{
   (app[Stdlib!.&&] (app[Vox_spec!.int_ge] _ global[x/290])
   (app[Vox_spec!.int_ge] _ global[y/291]))
   } =
  <fun>
|}]
