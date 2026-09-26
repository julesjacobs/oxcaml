(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_egraph_language_spec.ml vox_egraph_rule_spec.ml vox_egraph_derivation_spec.ml";
 readonly_files = "egraph_rule_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.opt-build-env;
   ocamlc.opt;
   run-expect;
   check-program-output;
 }
*)

module L = Vox_egraph_language_spec;;
module R = Vox_egraph_rule_spec;;
module E = Vox_egraph_derivation_spec;;
[%%expect{|
module L = Vox_egraph_language_spec
module R = Vox_egraph_rule_spec
module E = Vox_egraph_derivation_spec
|}]

let (foreign_rule @ total) () :
    {u : unit | E.valid R.No_rules
      (E.Rule (0, {R.vars = []; lhs = R.Int_lit 0;
        rhs = R.Int_lit 1}, []))} @ ghost = ghost_ (
  let rule = {R.vars = []; lhs = R.Int_lit 0; rhs = R.Int_lit 1} in
  E.valid_def R.No_rules (E.Rule (0, rule, []));
  R.lookup_rule_def R.No_rules 0;
  ());;
[%%expect{|
Line 8, characters 2-4:
8 |   ());;
      ^^
Error: Refinement could not be proved (counterexample)
|}]

let (wrong_endpoint @ total) () :
    {u : unit | E.valid R.No_rules
      (E.Trans (E.Refl (L.Int_lit 0), E.Refl (L.Int_lit 1)))}
    @ ghost = ghost_ (
  let a = E.Refl (L.Int_lit 0) in
  let b = E.Refl (L.Int_lit 1) in
  E.valid_def R.No_rules (E.Trans (a, b));
  E.right_def a;
  E.left_def b;
  E.endpoints_def a;
  E.endpoints_def b;
  ());;
[%%expect{|
Line 12, characters 2-4:
12 |   ());;
       ^^
Error: Refinement could not be proved (counterexample)
|}]
