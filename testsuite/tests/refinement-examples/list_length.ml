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
   result is the induction hypothesis for the cons case.  CURRENT: the bare
   nil-case result is rejected against the refined result type. *)

#load "vox_spec.cmo";;

(* @ex id=list_length_measure final=ACCEPT today=REJECT stable=no unlocks=recursive-totality+modes+verification *)
let rec length (values : int list)
    : int{ _ = Vox_spec.list_length values }
  =
  match values with
  | [] -> 0
  | _head :: tail -> 1 + length tail

[%%expect {|
Line 5, characters 10-11:
5 |   | [] -> 0
              ^
Error: The constant "0" has type "int" but an expression was expected of type
         "int{
          (app[Stdlib!.=] _ (app[Vox_spec!.list_length] global[values/290]))
          }"
|}]
