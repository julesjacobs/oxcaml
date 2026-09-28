open Copy_spec
open Copy_heap_proofs
open Generalize_spec
open Copy_cleanup_spec
open Effective_copy_spec
open Effective_copy_heap_proofs
open Hm_effective_runtime
module E = Effective_level
module R = Representative_level
module M = Effective_copy_metadata
module O = Effective_copy_order
module U = Level_unifier_spec

let rec (unmarked @ total) : (h : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total ->
    (marks : ((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h x with None -> true
        | Some v -> not v.visited})) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | effective_valid h heads epoch depth d} ->
    {u : unit | match H.at (heap h epoch depth d) x with
      None -> true | Some v -> not v.visited} @ ghost =
  fun h heads marks epoch depth d x premise -> ghost_ (
    effective_valid_def h heads epoch depth d;
    heap_def h epoch depth d; match d with
    | Clean -> marks x; ()
    | Start -> marks x; let desc = Bool in let v = cell desc depth in
      cell_def desc depth; put_frame h epoch v x; ()
    | Fresh (rest, p, q, old, desc) ->
      unmarked h heads marks epoch depth rest x ();
      unmarked h heads marks epoch depth rest p ();
      let mid = heap h epoch depth rest in let v = cell desc depth in
      cell_def desc depth; let h1 = H.put mid q v in let w = session_mark rest old epoch q in
      session_mark_def rest old epoch q; mark_def old epoch q;
      put_frame h1 p w x; ()
    | Alias (rest, p, q, old) ->
      unmarked h heads marks epoch depth rest x ();
      unmarked h heads marks epoch depth rest p ();
      let mid = heap h epoch depth rest in let w = session_mark rest old epoch q in
      session_mark_def rest old epoch q; mark_def old epoch q;
      put_frame mid p w x; ())

let (copy_runtime @ total) : (h : node Pref.heap) @ immutable ->
    (a : E.heads) @ total -> (b : E.heads) @ total ->
    (depth : int) -> (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable ->
      {u : unit | runtime_at h a depth pool x})) @ total ->
    (epoch : node Pref.t) @ immutable -> (d : history) @ immutable ->
    (valid : ((x : node Pref.t) @ immutable ->
      {u : unit | E.valid_head (heap h epoch depth d) b x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | depth >= 0 && effective_valid h a epoch depth d} ->
    {u : unit | runtime_at (swept (heap h epoch depth d) (Pooled_spec.touched d))
      b depth (Pooled_spec.registered pool epoch d) x} @ ghost =
  fun h a b depth pool facts epoch d valid x premise -> ghost_ (
    let va : ((y : node Pref.t) @ immutable ->
      {u : unit | E.valid_head h a y}) @ total = fun y ->
      facts y; runtime_at_def h a depth pool y; safe_def h a y;
      () in
    let scope : ((y : node Pref.t) @ immutable ->
      {u : unit | if H.mem h y then source_ok h y else H.at h y === None}) @ total = fun y ->
      facts y; runtime_at_def h a depth pool y; safe_def h a y;
      () in
    let bounds : ((y : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h y) || E.level h a y === Generic
        || E.effective_below h a y depth}) @ total = fun y ->
      facts y; runtime_at_def h a depth pool y; depth_bound_def h a depth y;
      () in
    let order : ((y : node Pref.t) @ immutable ->
      {u : unit | E.effective_ordered h a y}) @ total = fun y ->
      facts y; runtime_at_def h a depth pool y; safe_def h a y;
      () in
    let marks : ((y : node Pref.t) @ immutable ->
      {u : unit | match H.at h y with None -> true | Some v -> not v.visited}) @ total = fun y ->
      facts y; runtime_at_def h a depth pool y; safe_def h a y;
      () in
    let raw = heap h epoch depth d in let trail = Pooled_spec.touched d in
    let after = swept raw trail in let next = Pooled_spec.registered pool epoch d in
    let frame : ((y : node Pref.t) @ immutable ->
      {u : unit | swept_at raw after trail y}) @ total = fun y ->
      let out = M.result_at h a epoch depth d y () in out in
    let observed : ((y : node Pref.t) @ immutable ->
      {u : unit | H.mem raw y === H.mem after y
        && U.observe raw y === U.observe after y}) @ total = fun y ->
      frame y; swept_at_def raw after trail y;
      U.observe_def raw y; U.observe_def after y; () in
    facts x; runtime_at_def h a depth pool x; safe_def h a x;
    M.history_scope h a scope epoch depth d x ();
    Copy_cleanup_proofs.sweep_scope raw after trail frame x ();
    O.copy_ordered h scope a b va epoch depth bounds order d valid x ();
    O.copy_bounds h a b va epoch depth bounds d valid x ();
    O.sweep_ordered raw after b trail frame x;
    O.sweep_level raw after b trail frame x;
    O.sweep_below raw after b trail frame x depth;
    unmarked h a marks epoch depth d x ();
    M.clean_result h a epoch depth d x ();
    frame x; swept_at_def raw after trail x;
    valid x; E.valid_head_def raw b x; E.valid_head_def after b x;
    let r = b x in R.resolution_frame raw after observed x r.root r.path;
    let cut = depth - 1 in
    Effective_copy_pool.registered_representatives h a pool epoch depth d cut x ();
    R.representative_covered_def raw cut next x;
    R.representative_covered_def after cut next x;
    U.terminal_def raw x; U.terminal_def after x;
    U.observe_def raw x; U.observe_def after x;
    covered_def raw cut next x; covered_def after cut next x;
    Level_spec.at_level_def raw x; Level_spec.at_level_def after x;
    runtime_at_def after b depth next x; safe_def after b x;
    depth_bound_def after b depth x; ())
