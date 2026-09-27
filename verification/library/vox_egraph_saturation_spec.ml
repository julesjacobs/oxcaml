(* Closure under the rules; part of the trusted specification.
   [closed_rule graph rule]: for every assignment of the rule's variables
   (each an id below the node count, or -1) and every root, if the
   assignment is well sorted and the left side matches at the root, so does
   the right side ([Vox_egraph_quantifier.closed_bindings] from the empty
   prefix). [closed_rules] asks it of every rule in the list. For n
   variables there are (count + 1)^n assignments; Vox_egraph_assignment_spec
   lists them as the cases that saturation checks. *)

module R = Vox_egraph_rule_spec
module Q = Vox_egraph_match_spec
module B = Vox_egraph_quantifier

let[@def] (closed_rule @ total) (graph : Q.graph @ immutable) (rule : R.rule @ immutable) =
  B.closed_bindings graph rule rule.vars [] graph.count

let[@def] rec (closed_rules @ total) (graph : Q.graph @ immutable) (rules : R.t @ immutable) =
  match rules with
  | R.No_rules -> true
  | R.Rule_cons (rule, rest) -> closed_rule graph rule && closed_rules graph rest
