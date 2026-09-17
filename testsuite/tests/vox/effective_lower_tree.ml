open Copy_spec
open Level_spec
open Level_finite_spec
open Effective_lower_spec
module E = Effective_level
module U = Level_unifier_spec

let rec (bounded_tree @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (witness : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads x})) @ total ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | E.effective_ordered h heads x})) @ total ->
    (bound : int) -> (t : tree) @ immutable ->
    {u : unit | finite h t && E.effective_below h heads (tree_root t) bound} ->
    {b : bounded | bound_root b === tree_root t && effective_bounded h heads bound b} @ immutable ghost =
  fun h heads witness order bound t premise -> ghost_ (
    let refine_ premise = premise in finite_def h t; tree_root_def t;
    let p = tree_root t in order p; witness p;
    E.effective_ordered_def h heads p; E.effective_below_def h heads p bound;
    U.observe_def h p; at_level_def h p;
    let level = match at_level h p with Generic -> 0 | Finite n -> n in
    let u = () in
    let b = match t with
    | Free p | Constant_tree p -> Tip p
    | Alias_tree (p, child) ->
      let q = tree_root child in witness q; finite_def h child;
      E.link_level h heads p q (refine_ u); E.effective_below_def h heads q bound;
      let refine_ c = bounded_tree h heads witness order bound child (refine_ u) in Through (p, c)
    | Branch (p, left, right) ->
      U.terminal_def h p; E.terminal_level h heads p (refine_ u);
      let a = tree_root left in let b = tree_root right in
      E.effective_below_def h heads a level; E.effective_below_def h heads b level;
      E.effective_below_def h heads a bound; E.effective_below_def h heads b bound;
      let refine_ a = bounded_tree h heads witness order bound left (refine_ u) in
      let refine_ b = bounded_tree h heads witness order bound right (refine_ u) in Fork (p, a, b) in
    bound_root_def b; effective_bounded_def h heads bound b; refine_ b)
