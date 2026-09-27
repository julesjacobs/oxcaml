(* From derivations to evaluation; part of the trusted specification. The
   rules of an e-graph need not agree with [L.eval], so an [Equal] answer
   says only that the two expressions are derivable from each other. [sound
   rules proof env interpretation ()]: if [proof] is valid for [rules] and
   [interpretation] shows that, in [env], both sides of every valid instance
   of every rule evaluate to the same value, then the two ends of [proof]
   evaluate to the same value in [env]. Vox_egraph_language_proof has
   lemmas for common rules, and testsuite/tests/vox/egraph_derivation.ml
   discharges the premise for a concrete rule. vox_egraph_interpret_wrapping.ml
   proves the theorem by recursion on the derivation. *)

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
