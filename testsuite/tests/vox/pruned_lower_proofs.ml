open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec

let rec (bounded_tree @ total) : (h : Pref.heap) @ immutable ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x})) @ total ->
    (bound : int) -> (t : tree) @ immutable ->
    {u : unit | finite h t && below h (tree_root t) bound} ->
    {b : bounded | bound_root b === tree_root t && bounded h bound b} @ immutable ghost =
  fun h order bound t premise -> ghost_ (
    finite_def h t; tree_root_def t;
    let p = tree_root t in order p;
    ordered_def h p; below_def h p bound; at_level_def h p; observe_def h p;
    let level = match at_level h p with Generic -> 0 | Finite n -> n in
    let u = () in
    let b = match t with
    | Free p | Constant_tree p -> Tip p
    | Alias_tree (p, child) ->
      let q = tree_root child in let desc = Link q in
      children_below_def h desc level;
      below_def h q level; below_def h q bound; at_level_def h q;
      let c = bounded_tree h order bound child (u) in Through (p, c)
    | Branch (p, left, right) ->
      let a = tree_root left in let b = tree_root right in let desc = Arrow (a, b) in
      children_below_def h desc level;
      below_def h a level; below_def h b level; below_def h a bound; below_def h b bound; at_level_def h a; at_level_def h b;
      let a = bounded_tree h order bound left (u) in
      let b = bounded_tree h order bound right (u) in Fork (p, a, b) in
    bound_root_def b; bounded_def h bound b; b)
