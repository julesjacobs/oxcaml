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
   at least both arguments.  CURRENT: the verification pass now generates the
   conjunction obligation, but the comparison wrapper [Vox_spec.int_ge] is
   partial and therefore opaque to the solver, so it cannot prove [int_ge _ x]
   holds in the branch that returns [y].  The conjunction is the ordinary
   boolean [&&], which the solver does interpret; discharge waits on total
   comparisons giving the wrapper a logical meaning. *)

#load "vox_spec.cmo";;

(* @ex id=max_upper_bound final=ACCEPT today=REJECT stable=no unlocks=total-comparisons+verification *)
let max (x : int) (y : int) =
  (if Vox_spec.int_ge x y then x else y
    : int{
        Vox_spec.int_ge _ x
        && Vox_spec.int_ge _ y
      })

[%%expect {|
Lines 2-6, characters 2-8:
2 | ..(if Vox_spec.int_ge x y then x else y
3 |     : int{
4 |         Vox_spec.int_ge _ x
5 |         && Vox_spec.int_ge _ y
6 |       })
Error: Refinement verification failed (not-proved)
|}]
