(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_egraph_language_spec.ml vox_egraph_language_proof.ml vox_egraph_rule_spec.ml vox_egraph_rules.ml egraph_rules.ml";
 { native; }
*)

module L = Vox_egraph_language_spec
module LP = Vox_egraph_language_proof
module R = Vox_egraph_rule_spec
module RP = Vox_egraph_rules

let () =
  let rule = { R.vars = [L.Integer];
    lhs = R.Add (R.Var 0, R.Int_lit 0); rhs = R.Var 0 } in
  assert (R.rule_valid rule);
  assert (not (R.rule_valid {rule with rhs = R.Bool_lit true}));
  assert (not (R.rule_valid {rule with rhs = R.Var 1}));
  assert (not (R.rule_valid {rule with lhs = R.Var (-1)}));
  assert (not (R.rule_valid {rule with vars = [L.Integer; L.Integer];
    rhs = R.Var 1}));
  assert (not (R.rule_valid {rule with
    lhs = R.Int_if (R.Var 0, R.Var 0, R.Var 0)}));
  assert (R.subst_valid rule.vars [L.Int_input]);
  assert (not (R.subst_valid rule.vars [L.Bool_input]));
  assert (not (R.subst_valid rule.vars []));
  assert (not (R.subst_valid rule.vars [L.Int_input; L.Int_input]));
  assert (R.instantiate rule.lhs [L.Int_input] =
    L.Add (L.Int_input, L.Int_lit 0));
  let arbitrary = {R.vars = []; lhs = R.Int_lit 0; rhs = R.Int_lit 1} in
  assert (R.rule_valid arbitrary);
  ghost_ (
    R.rule_valid_def arbitrary;
    R.pat_sort_def [] arbitrary.lhs;
    R.pat_sort_def [] arbitrary.rhs;
    R.vars_in_def arbitrary.rhs arbitrary.lhs;
    R.subst_valid_def [] [];
    RP.instance_sorted arbitrary [] ());
  let boolean = {R.vars = [L.Boolean];
    lhs = R.Bool_if (R.Var 0, R.Bool_lit true, R.Bool_lit false);
    rhs = R.Var 0} in
  assert (R.rule_valid boolean);
  assert (R.valid (R.Rule_cons (rule,
    R.Rule_cons (arbitrary, R.Rule_cons (boolean, R.No_rules)))));
  assert (R.instantiate boolean.rhs [L.Bool_input] = L.Bool_input)
