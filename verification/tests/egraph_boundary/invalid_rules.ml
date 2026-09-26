module R = Vox_egraph_rule_spec
let make () =
  let rule = {R.vars = []; lhs = R.Int_lit 0; rhs = R.Bool_lit true} in
  let rules = R.Rule_cons (rule, R.No_rules) in
  ghost_ (R.valid_def rules; R.rule_valid_def rule;
    R.pat_sort_def [] rule.lhs; R.pat_sort_def [] rule.rhs);
  Vox_egraph_rule_handle.create rules
