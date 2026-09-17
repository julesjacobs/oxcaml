open Copy_spec
open Level_spec
open Level_unifier_spec
let rec (lower_searched @ total) : (h : Pref.heap) @ immutable -> (bound : int) -> (edits : lowering) @ immutable ->
    (needle : node Pref.t) @ immutable -> (p : node Pref.t) @ immutable -> (found : bool) -> (d : search) @ immutable ->
    {u : unit | Terminal_lower_spec.terminal_valid h bound edits && searched h needle p found d} ->
    {u : unit | searched (lower_heap h bound edits) needle p found d} @ ghost = fun h bound edits needle p found d premise -> ghost_ (
  let refine_ premise = premise in let after = lower_heap h bound edits in
  searched_def h needle p found d; searched_def after needle p found d;
  let u = () in Effective_unifier_finite.lower_observe h bound edits p (refine_ u);
  match d with Hit | Leaf -> refine_ u
  | Follow (q, rest) -> lower_searched h bound edits needle q found rest (refine_ u); refine_ u
  | Left (a, _, left) -> let flag = true in lower_searched h bound edits needle a flag left (refine_ u); refine_ u
  | Both (a, b, left, right) -> let flag = false in lower_searched h bound edits needle a flag left (refine_ u);
    lower_searched h bound edits needle b found right (refine_ u); refine_ u)


let (bounded_terminal @ total) : (h : Pref.heap) @ immutable -> (bound : int) ->
    (tree : bounded) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | Terminal_lower_spec.terminal_bounded h bound tree && bound_root tree === p && terminal h p} ->
    {u : unit | below h p bound} @ ghost = fun h bound tree p premise -> ghost_ (
      let refine_ premise = premise in Terminal_lower_spec.terminal_bounded_def h bound tree;
      bound_root_def tree; terminal_def h p; observe_def h p;
      let u = () in match tree with Tip _ | Fork _ | Through _ -> refine_ u)
