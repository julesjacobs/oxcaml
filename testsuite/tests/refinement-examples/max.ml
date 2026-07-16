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
   at least both arguments.  CURRENT: the predicate is written through the
   prelude wrapper [Vox_spec.int_ge], an ordinary (partial) user function -- not
   one of the comparison primitives admitted inside a predicate.  A predicate is
   checked at [total], so calling the partial wrapper is rejected at totality,
   before the conjunction obligation is generated.  (The connective [&&] IS an
   admitted primitive; it is the wrapper that is partial.)  When total
   comparisons make the wrapper total-annotatable the predicate flows through to
   verification again; the [unlocks] tag records that dependency. *)

#load "vox_spec.cmo";;

(* @ex id=max_upper_bound final=ACCEPT today=REJECT stable=no unlocks=total-comparisons+verification *)
let max (x : int) (y : int) =
  (if Vox_spec.int_ge x y then x else y
    : int{
        Vox_spec.int_ge _ x
        && Vox_spec.int_ge _ y
      })

[%%expect {|
Line 4, characters 8-23:
4 |         Vox_spec.int_ge _ x
            ^^^^^^^^^^^^^^^
Error: The value "Vox_spec.int_ge" is "partial"
       but is expected to be "total"
         because it is used in an expression (at lines 3-6, characters 6-7).
|}]
