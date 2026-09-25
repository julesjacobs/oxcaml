module R = Vox_egraph_rule_spec
module Q = Vox_egraph_match_spec
module C = Vox_egraph_saturation_spec
module G = Vox_egraph_congruence_spec

let[@def] (fixed @ total) (graph : Q.graph @ immutable) (rules : R.t @ immutable) =
  R.valid rules && C.closed_rules graph rules && G.closed graph
