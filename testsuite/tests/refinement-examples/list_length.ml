(* TEST
 modules = "vox_spec.ml";
 setup-ocamlc.byte-build-env;
 module = "vox_spec.ml";
 ocamlc.byte;
 module = "";
 flags += " -I ${test_build_directory}/ocamlc.byte";
 expect;
*)

(* The program may pattern-match; only the predicate must stay inside the
   currently supported predicate subset.  Its predicate uses identifiers,
   equality, and application of the prelude measure.

   FINAL: once recursive measures can be total, the recursive call's refined
   result is the induction hypothesis for the cons case.  CURRENT: the
   verification pass reaches the obligation but cannot yet represent the [match]
   expression form in a verification condition, so it rejects the body. *)

#load "vox_spec.cmo";;

(* @ex id=list_length_measure final=ACCEPT today=REJECT stable=no unlocks=recursive-totality+modes+verification *)
let rec length (values : int list)
    : int{ _ = Vox_spec.list_length values }
  =
  match values with
  | [] -> 0
  | _head :: tail -> 1 + length tail

[%%expect {|
Lines 4-6, characters 2-36:
4 | ..match values with
5 |   | [] -> 0
6 |   | _head :: tail -> 1 + length tail
Error: Refinement verification failed: this expression form cannot yet be represented in a verification condition
|}]
