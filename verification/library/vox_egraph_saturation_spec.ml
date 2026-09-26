module R = Vox_egraph_rule_spec
module Q = Vox_egraph_match_spec
module B = Vox_egraph_quantifier

let[@def] (closed_rule @ total) (graph : Q.graph @ immutable) (rule : R.rule @ immutable) =
  B.closed_bindings graph rule rule.vars [] graph.count

let[@def] rec (closed_rules @ total) (graph : Q.graph @ immutable) (rules : R.t @ immutable) =
  match rules with
  | R.No_rules -> true
  | R.Rule_cons (rule, rest) -> closed_rule graph rule && closed_rules graph rest
