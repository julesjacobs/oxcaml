(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_egraph_language_spec.ml vox_egraph_language_proof.ml vox_egraph_rule_spec.ml vox_egraph_rules.ml vox_egraph_derivation_spec.ml vox_egraph_derivation.ml vox_egraph_interpret_wrapping.mli vox_egraph_interpret_wrapping.ml egraph_derivation.ml";
 { bytecode; }
*)

module L = Vox_egraph_language_spec
module LP = Vox_egraph_language_proof
module R = Vox_egraph_rule_spec
module RP = Vox_egraph_rules
module E = Vox_egraph_derivation_spec
module EP = Vox_egraph_derivation

let () = ghost_ (
  let rule = {R.vars = []; lhs = R.Int_lit 0; rhs = R.Int_lit 1} in
  let rules = R.Rule_cons (rule, R.No_rules) in
  R.lookup_rule_def rules 0;
  R.rule_valid_def rule;
  R.pat_sort_def [] rule.lhs;
  R.pat_sort_def [] rule.rhs;
  R.vars_in_def rule.rhs rule.lhs;
  R.subst_valid_def [] [];
  let proof = EP.rule_instance rules 0 rule [] () in
  EP.sort_sound rules proof ();
  let reverse = EP.symmetric rules proof in
  let chain = EP.transitive rules proof reverse () in
  EP.sort_sound rules chain ();
  let absent = E.Rule (1, rule, []) in
  R.lookup_rule_def rules 1;
  R.lookup_rule_def R.No_rules 0;
  E.valid_def rules absent;
  let _ : {u : unit | not (E.valid rules absent)} = () in
  let negative = E.Rule (-1, rule, []) in
  R.lookup_rule_def rules (-1);
  E.valid_def rules negative;
  let _ : {u : unit | not (E.valid rules negative)} = () in
  let bad_subst = E.Rule (0, rule, [L.Bool_input]) in
  R.subst_valid_def [] [L.Bool_input];
  E.valid_def rules bad_subst;
  let _ : {u : unit | not (E.valid rules bad_subst)} = () in
  let other = {rule with rhs = R.Int_lit 2} in
  let wrong_rule = E.Rule (0, other, []) in
  E.valid_def rules wrong_rule;
  let _ : {u : unit | not (E.valid rules wrong_rule)} = () in
  let mismatched = E.Trans (E.Refl (L.Int_lit 0), E.Refl (L.Int_lit 1)) in
  E.valid_def rules mismatched;
  E.right_def (E.Refl (L.Int_lit 0));
  E.left_def (E.Refl (L.Int_lit 1));
  E.endpoints_def (E.Refl (L.Int_lit 0));
  E.endpoints_def (E.Refl (L.Int_lit 1));
  let _ : {u : unit | not (E.valid rules mismatched)} = () in
  let foreign = E.Rule (0, rule, []) in
  E.valid_def R.No_rules foreign;
  R.lookup_rule_def R.No_rules 0;
  let _ : {u : unit | not (E.valid R.No_rules foreign)} = () in
  ());
  ()

let[@def] (zero_rule @ total) (_unit : unit) =
  {R.vars = [L.Integer];
   lhs = R.Add (R.Var 0, R.Int_lit 0); rhs = R.Var 0}

let (zero_holds @ total) :
    (rules : {rs : R.t | rs === R.Rule_cons (zero_rule (), R.No_rules)}) @ immutable ->
    (env : L.env) @ immutable -> (index : int) ->
    (rule : R.rule) @ immutable -> (subst : R.subst) @ immutable ->
    {u : unit | R.lookup_rule rules index === Some rule &&
      R.rule_valid rule && R.subst_valid rule.vars subst} ->
    {u : unit | L.eval (R.instantiate rule.lhs subst) env ===
      L.eval (R.instantiate rule.rhs subst) env} @ ghost =
    fun rules env index rule subst premise -> ghost_ (
  zero_rule_def ();
  R.lookup_rule_def rules index;
  R.lookup_rule_def R.No_rules (index - 1);
  R.subst_valid_def rule.vars subst;
  R.instantiate_def rule.lhs subst;
  R.instantiate_def rule.rhs subst;
  R.instantiate_def (R.Int_lit 0) subst;
  R.lookup_expr_def subst 0;
  match subst with
  | [] -> ()
  | expr :: _ ->
    LP.typed_eval expr env;
    match L.eval expr env with
    | L.Int_value value -> LP.add_zero_right expr env value ()
    | L.Bool_value _ -> ())

let () =
  let env = {L.int_input = 17; bool_input = false} in
  let rule = zero_rule () in
  let rules = R.Rule_cons (rule, R.No_rules) in
  ghost_ (
    zero_rule_def ();
    R.lookup_rule_def rules 0;
    R.rule_valid_def rule;
    R.pat_sort_def rule.vars rule.lhs;
    R.pat_sort_def rule.vars rule.rhs;
    R.pat_sort_def rule.vars (R.Int_lit 0);
    R.lookup_sort_def rule.vars 0;
    R.vars_in_def rule.rhs rule.lhs;
    R.occurs_def 0 rule.lhs;
    R.occurs_def 0 (R.Var 0);
    R.subst_valid_def rule.vars [L.Int_input];
    R.subst_valid_def [] [];
    L.sort_def L.Int_input;
    let proof = EP.rule_instance rules 0 rule [L.Int_input] () in
    Vox_egraph_interpret_wrapping.sound rules proof env
      (zero_holds rules env) ());
  ()
