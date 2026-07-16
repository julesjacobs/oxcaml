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
   CURRENT: the verification pass now generates the result obligation, but the
   comparison wrapper [Vox_spec.int_ge] is partial and therefore opaque to the
   solver, so it cannot relate the negated branch [0 - x] to the guard.  The
   obligation is honestly not proved until total comparisons give the wrapper a
   logical meaning. *)

#load "vox_spec.cmo";;

(* @ex id=abs_nonnegative final=ACCEPT today=REJECT stable=no unlocks=total-comparisons+verification *)
let abs (x : int) =
  (if Vox_spec.int_ge x 0 then x else 0 - x
    : int{ Vox_spec.int_ge _ 0 })

[%%expect {|
Lines 2-3, characters 2-33:
2 | ..(if Vox_spec.int_ge x 0 then x else 0 - x
3 |     : int{ Vox_spec.int_ge _ 0 })
Error: Refinement verification failed (not-proved)
|}]
