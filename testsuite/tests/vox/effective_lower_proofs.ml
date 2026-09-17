open Copy_spec
open Level_spec
open Level_proofs
open Effective_lower_spec
module E = Effective_level
module U = Level_unifier_spec
let rec (lowering_at @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total -> (bound : int) ->
    (d : lowering) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | effective_lower_valid h heads bound d} -> {u : unit | lower_frame h (lower_heap h bound d) x} @ ghost =
  fun h heads bound d x premise -> ghost_ (
    let refine_ premise = premise in effective_lower_valid_def h heads bound d; lower_heap_def h bound d;
    let u = () in match d with Keep -> frame_refl h x; refine_ u
    | Lower (p, old, rest) ->
      lowering_at h heads bound rest x (refine_ u); let mid = lower_heap h bound rest in
      write_frame mid p old bound x (refine_ u); let after = lower_heap h bound d in
      frame_trans h mid after x (refine_ u); refine_ u
    | Sequence (a, b) ->
      lowering_at h heads bound a x (refine_ u); let mid = lower_heap h bound a in
      lowering_at mid heads bound b x (refine_ u); let after = lower_heap h bound d in
      frame_trans h mid after x (refine_ u); refine_ u)

let (frame_head @ total) : (h : Pref.heap) @ immutable -> (after : Pref.heap) @ immutable ->
    (heads : E.heads) @ total ->
    (frame : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame h after x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads x} ->
    {u : unit | E.valid_head after heads x} @ ghost =
  fun h after heads frame x premise -> ghost_ (
    let refine_ premise = premise in
    let observed : ((y : node Pref.t) @ immutable -> {u : unit |
        H.mem h y === H.mem after y && U.observe h y === U.observe after y}) @ total = fun y ->
      frame y; lower_frame_def h after y; U.observe_def h y; U.observe_def after y; let u = () in refine_ u in
    frame x; lower_frame_def h after x;
    E.valid_head_def h heads x; E.valid_head_def after heads x;
    let r = heads x in Representative_level.resolution_frame h after observed x r.root r.path;
    let u = () in refine_ u)

let (frame_level @ total) : (h : Pref.heap) @ immutable -> (after : Pref.heap) @ immutable ->
    (heads : E.heads) @ total ->
    (frame : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame h after x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | H.mem h x === H.mem after x && decreases (E.level h heads x) (E.level after heads x)} @ ghost =
  fun h after heads frame x -> ghost_ (
    frame x; lower_frame_def h after x;
    E.level_def h heads x; E.level_def after heads x;
    let r = heads x in frame r.root; lower_frame_def h after r.root;
    at_level_def h r.root; at_level_def after r.root;
    let generic = Generic in decreases_def generic generic;
    let u = () in refine_ u)

let (frame_below @ total) : (h : Pref.heap) @ immutable -> (after : Pref.heap) @ immutable ->
    (heads : E.heads) @ total ->
    (frame : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame h after x})) @ total ->
    (x : node Pref.t) @ immutable -> (bound : int) ->
    {u : unit | E.effective_below h heads x bound} ->
    {u : unit | E.effective_below after heads x bound} @ ghost =
  fun h after heads frame x bound premise -> ghost_ (
    let refine_ premise = premise in frame_level h after heads frame x;
    let before_level = E.level h heads x in let after_level = E.level after heads x in
    decreases_def before_level after_level;
    E.effective_below_def h heads x bound; E.effective_below_def after heads x bound;
    let u = () in refine_ u)

let (frame_active @ total) : (h : Pref.heap) @ immutable -> (after : Pref.heap) @ immutable ->
    (heads : E.heads) @ total ->
    (frame : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame h after x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | E.effective_active h heads x === E.effective_active after heads x} @ ghost =
  fun h after heads frame x -> ghost_ (
    frame_level h after heads frame x;
    let before_level = E.level h heads x in let after_level = E.level after heads x in
    decreases_def before_level after_level;
    E.effective_active_def h heads x; E.effective_active_def after heads x;
    let u = () in refine_ u)

let (frame_scope @ total) : (h : Pref.heap) @ immutable -> (after : Pref.heap) @ immutable ->
    (heads : E.heads) @ total ->
    (frame : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame h after x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || E.effective_scope h heads x} ->
    {u : unit | not (H.mem after x) || E.effective_scope after heads x} @ ghost =
  fun h after heads frame x premise -> ghost_ (
    let refine_ premise = premise in frame x; lower_frame_def h after x;
    E.effective_scope_def h heads x; E.effective_scope_def after heads x;
    source_ok_def h x; source_ok_def after x; U.observe_def h x; U.observe_def after x;
    frame_active h after heads frame x;
    (match H.at h x with None -> () | Some v ->
      (match v.memo with Empty_memo | Forward _ -> () | Memo (stamp, _) -> frame stamp; lower_frame_def h after stamp; ());
      match v.desc with Var | Bool -> ()
      | Link q -> frame q; lower_frame_def h after q; frame_active h after heads frame q; ()
      | Arrow (a, b) -> frame a; frame b; lower_frame_def h after a; lower_frame_def h after b; frame_active h after heads frame a; frame_active h after heads frame b; ());
    let u = () in refine_ u)

let rec (bounded_frame @ total) : (h : Pref.heap) @ immutable -> (after : Pref.heap) @ immutable ->
    (heads : E.heads) @ total ->
    (frame : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame h after x})) @ total ->
    (limit : int) -> (t : bounded) @ immutable -> {u : unit | effective_bounded h heads limit t} ->
    {u : unit | effective_bounded after heads limit t} @ ghost =
  fun h after heads frame limit t premise -> ghost_ (
    let refine_ premise = premise in effective_bounded_def h heads limit t;
    effective_bounded_def after heads limit t; bound_root_def t;
    let p = bound_root t in frame p; lower_frame_def h after p;
    let u = () in frame_below h after heads frame p limit (refine_ u);
    match t with Tip _ -> refine_ u
    | Through (_, c) -> bounded_frame h after heads frame limit c (refine_ u); refine_ u
    | Fork (_, a, b) -> bounded_frame h after heads frame limit a (refine_ u);
      bounded_frame h after heads frame limit b (refine_ u); refine_ u)

let (children_frame @ total) : (h : Pref.heap) @ immutable -> (after : Pref.heap) @ immutable ->
    (heads : E.heads) @ total ->
    (frame : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame h after x})) @ total ->
    (desc : desc) @ immutable -> (bound : int) ->
    {u : unit | effective_children_below h heads desc bound} ->
    {u : unit | effective_children_below after heads desc bound} @ ghost =
  fun h after heads frame desc bound premise -> ghost_ (
    let refine_ premise = premise in effective_children_below_def h heads desc bound;
    effective_children_below_def after heads desc bound; let u = () in
    match desc with Var | Bool -> refine_ u
    | Link q -> frame_below h after heads frame q bound (refine_ u); refine_ u
    | Arrow (a, b) -> frame_below h after heads frame a bound (refine_ u);
      frame_below h after heads frame b bound (refine_ u); refine_ u)

let (write_ordered @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (p : node Pref.t) @ immutable -> (old : node) @ immutable -> (bound : int) ->
    (x : node Pref.t) @ immutable ->
    {u : unit | H.mem h p && H.at h p === Some old && bound >= 0 && E.effective_ordered h heads x
      && effective_children_below h heads old.desc bound
      && (match old.desc with Link _ -> false | _ -> true)
      && (match old.level with Generic -> false | Finite n -> n >= 0)} ->
    {u : unit | E.effective_ordered (H.put h p (lower_cell old bound)) heads x} @ ghost =
  fun h heads p old bound x premise -> ghost_ (
    let refine_ premise = premise in let v = lower_cell old bound in lower_cell_def old bound;
    let after = H.put h p v in
    let frame : ((y : node Pref.t) @ immutable -> {u : unit | lower_frame h after y}) @ total = fun y ->
      let u = () in write_frame h p old bound y (refine_ u); refine_ u in
    frame x; lower_frame_def h after x; E.effective_ordered_def h heads x;
    E.effective_ordered_def after heads x;
    let u = () in children_frame h after heads frame old.desc bound (refine_ u);
    effective_children_below_def after heads old.desc bound;
    (match H.at h x with None -> () | Some before -> match before.desc, before.level with
      | Arrow (a, b), Finite n ->
        effective_children_below_def h heads before.desc n;
        children_frame h after heads frame before.desc n (refine_ u);
        effective_children_below_def after heads before.desc n; ()
      | _ -> ());
    Copy_heap_proofs.put_frame h p v x; refine_ u)

let rec (lowering_ordered @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (bound : int) -> (d : lowering) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | effective_lower_valid h heads bound d && E.effective_ordered h heads x} ->
    {u : unit | E.effective_ordered (lower_heap h bound d) heads x} @ ghost =
  fun h heads bound d x premise -> ghost_ (
    let refine_ premise = premise in effective_lower_valid_def h heads bound d;
    lower_heap_def h bound d; let u = () in match d with
    | Keep -> refine_ u
    | Lower (p, old, rest) -> lowering_ordered h heads bound rest x (refine_ u);
      let mid = lower_heap h bound rest in write_ordered mid heads p old bound x (refine_ u); refine_ u
    | Sequence (a, b) -> lowering_ordered h heads bound a x (refine_ u);
      let mid = lower_heap h bound a in lowering_ordered mid heads bound b x (refine_ u); refine_ u)

let rec (links_unchanged @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (bound : int) -> (d : lowering) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | effective_lower_valid h heads bound d && (match H.at h x with Some {desc = Link _; _} -> true | _ -> false)} ->
    {u : unit | H.at (lower_heap h bound d) x === H.at h x} @ ghost =
  fun h heads bound d x premise -> ghost_ (
    let refine_ premise = premise in effective_lower_valid_def h heads bound d;
    lower_heap_def h bound d; let u = () in match d with
    | Keep -> refine_ u
    | Lower (p, old, rest) -> links_unchanged h heads bound rest x (refine_ u);
      let mid = lower_heap h bound rest in let v = lower_cell old bound in
      Copy_heap_proofs.put_frame mid p v x; refine_ u
    | Sequence (a, b) -> links_unchanged h heads bound a x (refine_ u);
      let mid = lower_heap h bound a in links_unchanged mid heads bound b x (refine_ u); refine_ u)

let (lower_head @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (bound : int) -> (d : lowering) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | effective_lower_valid h heads bound d && E.valid_head h heads x} ->
    {u : unit | E.valid_head (lower_heap h bound d) heads x} @ ghost =
  fun h heads bound d x premise -> ghost_ (
    let refine_ premise = premise in let after = lower_heap h bound d in
    let frame : ((y : node Pref.t) @ immutable -> {u : unit | lower_frame h after y}) @ total = fun y ->
      let u = () in lowering_at h heads bound d y (refine_ u); refine_ u in
    let u = () in frame_head h after heads frame x (refine_ u); refine_ u)

let (lower_scope @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || E.effective_scope h heads x})) @ total ->
    (bound : int) -> (d : lowering) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | effective_lower_valid h heads bound d} ->
    {u : unit | not (H.mem (lower_heap h bound d) x) || E.effective_scope (lower_heap h bound d) heads x} @ ghost =
  fun h heads scope bound d x premise -> ghost_ (
    let refine_ premise = premise in let after = lower_heap h bound d in
    let frame : ((y : node Pref.t) @ immutable -> {u : unit | lower_frame h after y}) @ total = fun y ->
      let u = () in lowering_at h heads bound d y (refine_ u); refine_ u in
    scope x; let u = () in frame_scope h after heads frame x (refine_ u); refine_ u)

let (lower_finite @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (bound : int) -> (d : lowering) @ immutable -> (tree : Level_finite_spec.tree) @ immutable ->
    {u : unit | effective_lower_valid h heads bound d && Level_finite_spec.finite h tree} ->
    {u : unit | Level_finite_spec.finite (lower_heap h bound d) tree} @ ghost =
  fun h heads bound d tree premise -> ghost_ (
    let refine_ premise = premise in let after = lower_heap h bound d in
    let observed : ((x : node Pref.t) @ immutable -> {u : unit |
        H.mem h x === H.mem after x && U.observe h x === U.observe after x}) @ total = fun x ->
      let u = () in lowering_at h heads bound d x (refine_ u);
      lower_frame_def h after x; U.observe_def h x; U.observe_def after x; refine_ u in
    let u = () in Forest_transport.finite_frame h after observed tree (refine_ u); refine_ u)

let (lower_bounded_at @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (bound : int) -> (t : bounded) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | effective_bounded h heads bound t && bound_root t === q} ->
    {u : unit | E.effective_below h heads q bound} @ ghost =
  fun h heads bound t q premise -> ghost_ (
    let refine_ premise = premise in effective_bounded_def h heads bound t;
    let u = () in refine_ u)

let (lower_active @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (bound : int) -> (d : lowering) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | effective_lower_valid h heads bound d} ->
    {u : unit | E.effective_active h heads x === E.effective_active (lower_heap h bound d) heads x} @ ghost =
  fun h heads bound d x premise -> ghost_ (
    let refine_ premise = premise in let after = lower_heap h bound d in
    let frame : ((y : node Pref.t) @ immutable -> {u : unit | lower_frame h after y}) @ total = fun y ->
      let u = () in lowering_at h heads bound d y (refine_ u); refine_ u in
    frame_active h after heads frame x; let u = () in refine_ u)

let (lower_below @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (bound : int) -> (d : lowering) @ immutable -> (x : node Pref.t) @ immutable -> (limit : int) ->
    {u : unit | effective_lower_valid h heads bound d && E.effective_below h heads x limit} ->
    {u : unit | E.effective_below (lower_heap h bound d) heads x limit} @ ghost =
  fun h heads bound d x limit premise -> ghost_ (
    let refine_ premise = premise in let after = lower_heap h bound d in
    let frame : ((y : node Pref.t) @ immutable -> {u : unit | lower_frame h after y}) @ total = fun y ->
      let u = () in lowering_at h heads bound d y (refine_ u); refine_ u in
    let u = () in frame_below h after heads frame x limit (refine_ u); refine_ u)

let rec (lower_fixed @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total -> (bound : int) ->
    (d : lowering) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | effective_lower_valid h heads bound d && (below h x bound || at_level h x === Generic)} ->
    {u : unit | H.at (lower_heap h bound d) x === H.at h x} @ ghost = fun h heads bound d x premise -> ghost_ (
  let refine_ premise = premise in effective_lower_valid_def h heads bound d; lower_heap_def h bound d;
  below_def h x bound; at_level_def h x;
  let u = () in match d with Keep -> refine_ u
  | Lower (p, old, rest) ->
    lower_fixed h heads bound rest x (refine_ u); let mid = lower_heap h bound rest in
    lower_cell_def old bound; let v = lower_cell old bound in Copy_heap_proofs.put_frame mid p v x; refine_ u
  | Sequence (a, b) -> lower_fixed h heads bound a x (refine_ u); let mid = lower_heap h bound a in
    lowering_at h heads bound a x (refine_ u); lower_frame_def h mid x; below_def mid x bound; at_level_def mid x;
    lower_fixed mid heads bound b x (refine_ u); refine_ u)
