(* TEST
 modules = "vox_spec.ml";
 setup-ocamlc.byte-build-env;
 module = "vox_spec.ml";
 ocamlc.byte;
 module = "";
 flags += " -I ${test_build_directory}/ocamlc.byte";
 expect;
*)

(* The predicate uses identifiers, equality, and application of the prelude
   measure [Vox_spec.list_length].

   FINAL: once recursive measures can be total, the recursive call's refined
   result is the induction hypothesis for the cons case.  CURRENT: the measure
   [Vox_spec.list_length] is an ordinary (partial) user function -- not one of
   the comparison primitives admitted inside a predicate.  A predicate is checked
   at [total], so forming the refinement type calls the partial measure and is
   rejected at totality, before any verification obligation is generated.  The
   [unlocks] tag keeps [modes] alongside recursive-totality and verification:
   the mode discipline is what now rejects the measure, and reaching the final
   ACCEPT additionally needs the measure to become total (recursive-totality)
   and the obligation to be discharged (verification). *)

#load "vox_spec.cmo";;

(* @ex id=list_length_measure final=ACCEPT today=REJECT stable=no unlocks=recursive-totality+modes+verification *)
let rec length (values : int list)
    : int{ _ = Vox_spec.list_length values }
  =
  match values with
  | [] -> 0
  | _head :: tail -> 1 + length tail

[%%expect {|
Line 2, characters 15-35:
2 |     : int{ _ = Vox_spec.list_length values }
                   ^^^^^^^^^^^^^^^^^^^^
Error: The value "Vox_spec.list_length" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 2, characters 6-44).
|}]
