module L = Vox_egraph_language_spec
module R = Vox_egraph_rule_spec
module E = Vox_egraph_derivation_spec

val sound :
    (rules : R.t) @ immutable -> (proof : E.evidence) @ immutable ->
    (env : L.env) @ immutable ->
    ((index : int) -> (rule : R.rule) @ immutable ->
      (subst : R.subst) @ immutable ->
      {u : unit | R.lookup_rule rules index === Some rule &&
        R.rule_valid rule && R.subst_valid rule.vars subst} ->
      {u : unit | L.eval (R.instantiate rule.lhs subst) env ===
        L.eval (R.instantiate rule.rhs subst) env} @ ghost) @ total ->
    {u : unit | E.valid rules proof} ->
    {u : unit | L.eval (E.left proof) env ===
      L.eval (E.right proof) env} @ ghost @@ total
