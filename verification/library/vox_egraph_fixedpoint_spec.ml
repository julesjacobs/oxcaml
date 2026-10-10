(* The meaning of [Fixed_point] from [Vox_egraph_rule_handle.saturate]; part
   of the trusted specification. The rules are valid, and the graph is
   closed under every rule ([Vox_egraph_saturation_spec.closed_rules]) and
   under congruence ([Vox_egraph_congruence_spec.closed]). It describes one
   graph: a later [admit] or [query] can add nodes and end it. *)

module R = Vox_egraph_rule_spec
module Q = Vox_egraph_match_spec
module C = Vox_egraph_saturation_spec
module G = Vox_egraph_congruence_spec

let[@def] (fixed @ total) (graph : Q.graph @ immutable) (rules : R.t @ immutable) =
  R.valid rules && C.closed_rules graph rules && G.closed graph
