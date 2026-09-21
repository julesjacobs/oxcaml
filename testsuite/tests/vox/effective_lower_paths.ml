open Copy_spec
open Level_spec
open Generalize_spec
open Lower_locality_spec
open Effective_lower_spec
open Effective_lower_proofs
module E = Effective_level
let rec (bounded_path @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (bound : int) -> (tree : bounded) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | effective_bounded h heads bound tree && contains tree x} ->
    {p : path | reaches h (bound_root tree) x p} @ immutable ghost =
  fun h heads bound tree x premise -> ghost_ (
    let refine_ premise = premise in effective_bounded_def h heads bound tree;
    contains_def tree x; bound_root_def tree; let root = bound_root tree in
    if root === x then (let p = Stop in reaches_def h root x p; refine_ p)
    else let u = () in match tree with
    | Tip _ -> let p = Stop in reaches_def h root x p; refine_ p
    | Through (_, child) ->
      let next = bound_root child in edge_def h root next;
      let refine_ rest = bounded_path h heads bound child x (refine_ u) in
      let p = Step (next, rest) in reaches_def h root x p; refine_ p
    | Fork (_, a, b) -> if contains a x then (
      let next = bound_root a in edge_def h root next;
      let refine_ rest = bounded_path h heads bound a x (refine_ u) in
      let p = Step (next, rest) in reaches_def h root x p; refine_ p)
      else (
      let next = bound_root b in edge_def h root next;
      let refine_ rest = bounded_path h heads bound b x (refine_ u) in
      let p = Step (next, rest) in reaches_def h root x p; refine_ p))

let rec (lower_path @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (bound : int) -> (edits : lowering) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (path : path) @ immutable ->
    {u : unit | effective_lower_valid h heads bound edits && reaches h p q path} ->
    {u : unit | reaches (lower_heap h bound edits) p q path} @ ghost =
  fun h heads bound edits p q path premise -> ghost_ (
    let refine_ premise = premise in let after = lower_heap h bound edits in
    reaches_def h p q path; reaches_def after p q path;
    let u = () in match path with Stop -> refine_ u
    | Step (next, rest) -> lowering_at h heads bound edits p (refine_ u);
      lower_frame_def h after p; edge_def h p next; edge_def after p next;
      lower_path h heads bound edits next q rest (refine_ u); refine_ u)

