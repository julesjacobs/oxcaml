(* TEST
 readonly_files = "has_z3.sh vc_import.ml vc_import.mli";
 setup-ocamlc.opt-build-env;
 module = "vc_import.mli";
 ocamlc.opt;
 module = "vc_import.ml";
 ocamlc.opt;
 flags = "-drefinements -vox-backend z3";
 flags += " -I ocamlc.opt ocamlc.opt/vc_import.cmo";
 script = "sh ${test_source_directory}/has_z3.sh";
 script;
 expect;
*)

(* A refined contract crossing a REAL compilation-unit boundary: vc_import
   is compiled by its own ocamlc.opt action above, so [Vc_import.positive]
   reaches this unit only through the cmi — the predicate round-trips
   through marshalling, unlike vc-z3.ml's same-unit module shapes, which
   read the in-memory declaration.  Nothing in this unit checked that
   contract, so it deposits as a fact at the occurrence and the verdict is
   conditional (same protocol as vc-z3.ml's admission-report block). *)

let import_use : int{ _ >= 1 } = Vc_import.positive;;
[%%expect{|
Line 1, characters 4-14: refined environment entry: import_use :
  int{ _ >= 1 }
Line 1, characters 33-51: refinement obligation: int{ _ >= 1 }
Refinement verdicts are conditional on 1 assumed contract:
  Vc_import.positive : int{ _ > 0 }
val import_use : int{ _ >= 1 } = 1
|}]

(* The same imported value read inside a predicate: the predicate-side
   occurrence rides the same cmi-fed deposit route, and the phrase still
   reports exactly one assumed contract. *)

let import_in_predicate : int{ _ * Vc_import.positive >= 0 } = 1;;
[%%expect{|
Line 1, characters 4-23: refined environment entry: import_in_predicate :
  int{ (_ * Vc_import.positive) >= 0 }
Line 1, characters 63-64: refinement obligation:
  int{ (_ * Vc_import.positive) >= 0 }
Refinement verdicts are conditional on 1 assumed contract:
  Vc_import.positive : int{ _ > 0 }
val import_in_predicate : int{ (_ * Vc_import.positive) >= 0 } = 1
|}]
