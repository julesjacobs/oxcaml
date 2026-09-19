open Copy_spec
open Level_spec
open Level_proofs
open Lower_locality_spec

let rec (confined_widen @ total) : (edits : lowering) @ immutable ->
    (small : bounded) @ immutable -> (big : bounded) @ immutable ->
    (include_node : ((x : node Pref.t) @ immutable ->
      {u : unit | not (contains small x) || contains big x})) @ total ->
    {u : unit | confined edits small} ->
    {u : unit | confined edits big} @ ghost =
  fun edits small big include_node premise -> ghost_ (
    confined_def edits small;
    confined_def edits big; match edits with
    | Keep -> ()
    | Lower (p, _, rest) -> include_node p;
      confined_widen rest small big include_node (); ()
    | Sequence (a, b) ->
      confined_widen a small big include_node ();
      confined_widen b small big include_node (); ())

let (confined_root @ total) : (edits : lowering) @ immutable ->
    (tree : bounded) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | bound_root tree === p && confined edits (Tip p)} ->
    {u : unit | confined edits tree} @ ghost = fun edits tree p premise -> ghost_ (
  let small = Tip p in
  let include_node : ((x : node Pref.t) @ immutable ->
      {u : unit | not (contains small x) || contains tree x}) @ total =
    fun x -> contains_def small x; contains_def tree x; bound_root_def tree;
      () in
  confined_widen edits small tree include_node ())

let (confined_through @ total) : (edits : lowering) @ immutable ->
    (p : node Pref.t) @ immutable -> (child : bounded) @ immutable ->
    {u : unit | confined edits child} ->
    {u : unit | confined edits (Through (p, child))} @ ghost =
  fun edits p child premise -> ghost_ (
    let tree = Through (p, child) in
    let include_node : ((x : node Pref.t) @ immutable ->
      {u : unit | not (contains child x) || contains tree x}) @ total =
      fun x -> contains_def tree x; () in
    let () = confined_widen edits child tree include_node () in ())

let (confined_fork @ total) : (left : lowering) @ immutable ->
    (right : lowering) @ immutable -> (p : node Pref.t) @ immutable ->
    (a : bounded) @ immutable -> (b : bounded) @ immutable ->
    {u : unit | confined left a && confined right b} ->
    {u : unit | confined left (Fork (p, a, b)) &&
      confined right (Fork (p, a, b))} @ ghost =
  fun left right p a b premise -> ghost_ (
    let tree = Fork (p, a, b) in
    let include_a : ((x : node Pref.t) @ immutable ->
      {u : unit | not (contains a x) || contains tree x}) @ total =
      fun x -> contains_def tree x; () in
    let include_b : ((x : node Pref.t) @ immutable ->
      {u : unit | not (contains b x) || contains tree x}) @ total =
      fun x -> contains_def tree x; () in
    confined_widen left a tree include_a ();
    confined_widen right b tree include_b (); ())

let rec (confined_frame @ total) : (h : Pref.heap) @ immutable ->
    (bound : int) -> (edits : lowering) @ immutable ->
    (tree : bounded) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | confined edits tree && not (contains tree x)} ->
    {u : unit | H.at (lower_heap h bound edits) x === H.at h x} @ ghost =
  fun h bound edits tree x premise -> ghost_ (
    confined_def edits tree;
    lower_heap_def h bound edits; match edits with
    | Keep -> ()
    | Lower (p, old, rest) -> confined_frame h bound rest tree x ();
      let mid = lower_heap h bound rest in let v = lower_cell old bound in
      Copy_heap_proofs.put_frame mid p v x; ()
    | Sequence (a, b) -> confined_frame h bound a tree x ();
      let mid = lower_heap h bound a in
      confined_frame mid bound b tree x (); ())
