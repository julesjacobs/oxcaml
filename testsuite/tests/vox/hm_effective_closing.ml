open Copy_spec
open Level_spec
open Level_unifier_spec
open Generalize_spec
open Hm_effective_runtime
module E = Effective_level
module R = Representative_level
module P = Representative_pool_spec

let (close_runtime @ total) : (h : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (depth : int) ->
    (child : pool) @ immutable -> (parent : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable ->
      {u : unit | runtime_at h heads (depth + 1) child x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | pool_scoped h child
      && (R.representative_covered h (depth - 1) parent x
        || listed child x)} ->
    {u : unit | runtime_at (P.close_heap h depth child) heads depth
      (P.transfer_rep (P.close_heap h depth child) child parent) x} @ ghost =
  fun h heads depth child parent facts x premise -> ghost_ (
    let refine_ premise = premise in let child_depth = depth + 1 in
    let valid : ((y : node Pref.t) @ immutable ->
      {u : unit | E.valid_head h heads y}) @ total = fun y ->
      facts y; runtime_at_def h heads child_depth child y;
      safe_def h heads y; let u = () in refine_ u in
    let after = P.close_heap h depth child in
    let next = P.transfer_rep after child parent in
    P.close_heap_def h depth child;
    let filtered = R.representatives h child in let u = () in
    R.representatives_scoped h child (refine_ u);
    facts x; runtime_at_def h heads child_depth child x;
    safe_def h heads x; depth_bound_def h heads child_depth x;
    E.closed_head h heads depth child x (refine_ u);
    Generalize_proofs.closed_observe h depth filtered x (refine_ u);
    closed_at_def h after depth filtered x;
    if H.mem h x then (
      Generalize_proofs.closed_source h depth filtered x (refine_ u);
      E.closed_ordered h heads valid depth child x (refine_ u);
      E.head_terminal h heads x (refine_ u);
      let r = heads x in facts r.root;
      runtime_at_def h heads child_depth child r.root;
      E.closed_level h heads depth child x (refine_ u); ())
    else (E.effective_ordered_def after heads x; ());
    let old = E.level h heads x in close_level_def depth old;
    E.effective_below_def h heads x child_depth;
    E.effective_below_def after heads x depth;
    let outer = depth - 1 in
    Representative_pool_proofs.closed_coverage h depth child parent outer x
      (refine_ u);
    runtime_at_def after heads depth next x; safe_def after heads x;
    depth_bound_def after heads depth x; refine_ u)

let (run_coverage @ total) : (h : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (depth : int) -> (parent : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable ->
      {u : unit | runtime_at h heads depth parent x})) @ total ->
    (env : Hm_environment_spec.env) @ immutable ->
    (rhs : Hm_effective_execution_spec.execution) @ immutable ->
    (middle : node Pref.heap) @ immutable -> (child : pool) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | depth >= 0 && Hm_effective_execution_spec.ran h (depth + 1) Empty env rhs middle child
      && not (Hm_effective_execution_spec.result rhs === None)} ->
    {u : unit | R.representative_covered middle (depth - 1) parent x
      || listed child x} @ ghost =
  fun h heads depth parent facts env rhs middle child x premise -> ghost_ (
    let refine_ premise = premise in let child_depth = depth + 1 in
    let empty : pool = Empty in let outer = depth - 1 in
    Hm_effective_execution_spec.ran_def h child_depth empty env rhs middle child;
    R.representative_covered_def middle outer parent x;
    covered_def middle outer parent x; at_level_def middle x;
    finite_node_def middle x; let u = () in
    if terminal middle x && H.mem middle x && finite_node middle x
        && not (listed child x) then (
      Hm_effective_registration.run_unlisted h child_depth empty env rhs middle child x (refine_ u);
      facts x; runtime_at_def h heads depth parent x;
      safe_def h heads x; depth_bound_def h heads depth x;
      R.representative_covered_def h outer parent x;
      covered_def h outer parent x;
      E.terminal_level h heads x (refine_ u);
      E.effective_below_def h heads x depth;
      finite_node_def h x; at_level_def h x;
      below_def h x outer;
      Hm_effective_bound.run_member h child_depth empty env rhs middle child outer x (refine_ u);
      Hm_effective_bound.preserved_bound_def h middle outer x;
      below_def middle x outer; ()) else ();
    refine_ u)

let (close_after_run @ total) : (h : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (next_heads : E.heads) @ total ->
    (depth : int) -> (parent : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable ->
      {u : unit | runtime_at h heads depth parent x})) @ total ->
    (env : Hm_environment_spec.env) @ immutable ->
    (rhs : Hm_effective_execution_spec.execution) @ immutable ->
    (middle : node Pref.heap) @ immutable -> (child : pool) @ immutable ->
    (next_facts : ((x : node Pref.t) @ immutable ->
      {u : unit | runtime_at middle next_heads (depth + 1) child x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | depth >= 0 && Hm_effective_execution_spec.ran h (depth + 1) Empty env rhs middle child
      && not (Hm_effective_execution_spec.result rhs === None)
      && pool_scoped middle child} ->
    {u : unit | runtime_at (P.close_heap middle depth child) next_heads depth
      (P.transfer_rep (P.close_heap middle depth child) child parent) x} @ ghost =
  fun h heads next_heads depth parent facts env rhs middle child next_facts x premise -> ghost_ (
    let refine_ premise = premise in let u = () in
    run_coverage h heads depth parent facts env rhs middle child x (refine_ u);
    close_runtime middle next_heads depth child parent next_facts x (refine_ u);
    refine_ u)
