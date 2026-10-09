open Copy_spec
open Level_spec
open Level_proofs
open Effective_lower_spec
module E = Effective_level
module U = Level_unifier_spec
let rec (lowering_at @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total -> (bound : int) ->
    (d : lowering) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | effective_lower_valid h heads bound d} -> {u : unit | lower_frame h (lower_heap h bound d) x} @ ghost =
  fun h heads bound d x premise -> ghost_ (
    effective_lower_valid_def h heads bound d; lower_heap_def h bound d;
    match d with Keep -> frame_refl h x; ()
    | Lower (p, old, rest) ->
      lowering_at h heads bound rest x (); let mid = lower_heap h bound rest in
      write_frame mid p old bound x (); let after = lower_heap h bound d in
      frame_trans h mid after x (); ()
    | Sequence (a, b) ->
      lowering_at h heads bound a x (); let mid = lower_heap h bound a in
      lowering_at mid heads bound b x (); let after = lower_heap h bound d in
      frame_trans h mid after x (); ())

let (frame_head @ total) : (h : node Pref.heap) @ immutable -> (after : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total ->
    (frame : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame h after x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads x} ->
    {u : unit | E.valid_head after heads x} @ ghost =
  fun h after heads frame x premise -> ghost_ (
    let observed : ((y : node Pref.t) @ immutable -> {u : unit |
        H.mem h y === H.mem after y && U.observe h y === U.observe after y}) @ total = fun y ->
      frame y; lower_frame_def h after y; U.observe_def h y; U.observe_def after y; () in
    frame x; lower_frame_def h after x;
    E.valid_head_def h heads x; E.valid_head_def after heads x;
    let r = heads x in Representative_level.resolution_frame h after observed x r.root r.path;
    ())

let (frame_level @ total) : (h : node Pref.heap) @ immutable -> (after : node Pref.heap) @ immutable ->
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
    ())

let (frame_below @ total) : (h : node Pref.heap) @ immutable -> (after : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total ->
    (frame : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame h after x})) @ total ->
    (x : node Pref.t) @ immutable -> (bound : int) ->
    {u : unit | E.effective_below h heads x bound} ->
    {u : unit | E.effective_below after heads x bound} @ ghost =
  fun h after heads frame x bound premise -> ghost_ (
    frame_level h after heads frame x;
    let before_level = E.level h heads x in let after_level = E.level after heads x in
    decreases_def before_level after_level;
    E.effective_below_def h heads x bound; E.effective_below_def after heads x bound;
    ())

let (frame_active @ total) : (h : node Pref.heap) @ immutable -> (after : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total ->
    (frame : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame h after x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | E.effective_active h heads x === E.effective_active after heads x} @ ghost =
  fun h after heads frame x -> ghost_ (
    frame_level h after heads frame x;
    let before_level = E.level h heads x in let after_level = E.level after heads x in
    decreases_def before_level after_level;
    E.effective_active_def h heads x; E.effective_active_def after heads x;
    ())

let (frame_scope @ total) : (h : node Pref.heap) @ immutable -> (after : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total ->
    (frame : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame h after x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || E.effective_scope h heads x} ->
    {u : unit | not (H.mem after x) || E.effective_scope after heads x} @ ghost =
  fun h after heads frame x premise -> ghost_ (
    frame x; lower_frame_def h after x;
    E.effective_scope_def h heads x; E.effective_scope_def after heads x;
    source_ok_def h x; source_ok_def after x; U.observe_def h x; U.observe_def after x;
    frame_active h after heads frame x;
    (match H.at h x with None -> () | Some v ->
      (match v.memo with Empty_memo | Forward _ -> () | Memo (stamp, _) -> frame stamp; lower_frame_def h after stamp; ());
      match v.desc with Var | Bool -> ()
      | Link q -> frame q; lower_frame_def h after q; frame_active h after heads frame q; ()
      | Arrow (a, b) -> frame a; frame b; lower_frame_def h after a; lower_frame_def h after b; frame_active h after heads frame a; frame_active h after heads frame b; ());
    ())

let rec (bounded_frame @ total) : (h : node Pref.heap) @ immutable -> (after : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total ->
    (frame : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame h after x})) @ total ->
    (limit : int) -> (t : bounded) @ immutable -> {u : unit | effective_bounded h heads limit t} ->
    {u : unit | effective_bounded after heads limit t} @ ghost =
  fun h after heads frame limit t premise -> ghost_ (
    effective_bounded_def h heads limit t;
    effective_bounded_def after heads limit t; bound_root_def t;
    let p = bound_root t in frame p; lower_frame_def h after p;
    frame_below h after heads frame p limit ();
    match t with Tip _ -> ()
    | Through (_, c) -> bounded_frame h after heads frame limit c (); ()
    | Fork (_, a, b) -> bounded_frame h after heads frame limit a ();
      bounded_frame h after heads frame limit b (); ())

let (children_frame @ total) : (h : node Pref.heap) @ immutable -> (after : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total ->
    (frame : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame h after x})) @ total ->
    (desc : desc) @ immutable -> (bound : int) ->
    {u : unit | effective_children_below h heads desc bound} ->
    {u : unit | effective_children_below after heads desc bound} @ ghost =
  fun h after heads frame desc bound premise -> ghost_ (
    effective_children_below_def h heads desc bound;
    effective_children_below_def after heads desc bound; match desc with Var | Bool -> ()
    | Link q -> frame_below h after heads frame q bound (); ()
    | Arrow (a, b) -> frame_below h after heads frame a bound ();
      frame_below h after heads frame b bound (); ())

let (write_ordered @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (p : node Pref.t) @ immutable -> (old : node) @ immutable -> (bound : int) ->
    (x : node Pref.t) @ immutable ->
    {u : unit | H.mem h p && H.at h p === Some old && bound >= 0 && E.effective_ordered h heads x
      && effective_children_below h heads old.desc bound
      && (match old.desc with Link _ -> false | _ -> true)
      && (match old.level with Generic -> false | Finite n -> n >= 0)} ->
    {u : unit | E.effective_ordered (H.put h p (lower_cell old bound)) heads x} @ ghost =
  fun h heads p old bound x premise -> ghost_ (
    let v = lower_cell old bound in lower_cell_def old bound;
    let after = H.put h p v in
    let frame : ((y : node Pref.t) @ immutable -> {u : unit | lower_frame h after y}) @ total = fun y ->
      write_frame h p old bound y (); () in
    frame x; lower_frame_def h after x; E.effective_ordered_def h heads x;
    E.effective_ordered_def after heads x;
    children_frame h after heads frame old.desc bound ();
    effective_children_below_def after heads old.desc bound;
    (match H.at h x with None -> () | Some before -> match before.desc, before.level with
      | Arrow (a, b), Finite n ->
        effective_children_below_def h heads before.desc n;
        children_frame h after heads frame before.desc n ();
        effective_children_below_def after heads before.desc n; ()
      | _ -> ());
    ())

let rec (lowering_ordered @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (bound : int) -> (d : lowering) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | effective_lower_valid h heads bound d && E.effective_ordered h heads x} ->
    {u : unit | E.effective_ordered (lower_heap h bound d) heads x} @ ghost =
  fun h heads bound d x premise -> ghost_ (
    effective_lower_valid_def h heads bound d;
    lower_heap_def h bound d; match d with
    | Keep -> ()
    | Lower (p, old, rest) -> lowering_ordered h heads bound rest x ();
      let mid = lower_heap h bound rest in write_ordered mid heads p old bound x (); ()
    | Sequence (a, b) -> lowering_ordered h heads bound a x ();
      let mid = lower_heap h bound a in lowering_ordered mid heads bound b x (); ())

let rec (links_unchanged @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (bound : int) -> (d : lowering) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | effective_lower_valid h heads bound d && (match H.at h x with Some {desc = Link _; _} -> true | _ -> false)} ->
    {u : unit | H.at (lower_heap h bound d) x === H.at h x} @ ghost =
  fun h heads bound d x premise -> ghost_ (
    effective_lower_valid_def h heads bound d;
    lower_heap_def h bound d; match d with
    | Keep -> ()
    | Lower (p, old, rest) -> links_unchanged h heads bound rest x ();
      let mid = lower_heap h bound rest in let v = lower_cell old bound in
      Copy_heap_proofs.put_frame mid p v x; ()
    | Sequence (a, b) -> links_unchanged h heads bound a x ();
      let mid = lower_heap h bound a in links_unchanged mid heads bound b x (); ())

let (lower_head @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (bound : int) -> (d : lowering) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | effective_lower_valid h heads bound d && E.valid_head h heads x} ->
    {u : unit | E.valid_head (lower_heap h bound d) heads x} @ ghost =
  fun h heads bound d x premise -> ghost_ (
    let after = lower_heap h bound d in
    let frame : ((y : node Pref.t) @ immutable -> {u : unit | lower_frame h after y}) @ total = fun y ->
      lowering_at h heads bound d y (); () in
    frame_head h after heads frame x (); ())

let (lower_scope @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || E.effective_scope h heads x})) @ total ->
    (bound : int) -> (d : lowering) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | effective_lower_valid h heads bound d} ->
    {u : unit | not (H.mem (lower_heap h bound d) x) || E.effective_scope (lower_heap h bound d) heads x} @ ghost =
  fun h heads scope bound d x premise -> ghost_ (
    let after = lower_heap h bound d in
    let frame : ((y : node Pref.t) @ immutable -> {u : unit | lower_frame h after y}) @ total = fun y ->
      lowering_at h heads bound d y (); () in
    scope x; frame_scope h after heads frame x (); ())

let (lower_finite @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (bound : int) -> (d : lowering) @ immutable -> (tree : Level_finite_spec.tree) @ immutable ->
    {u : unit | effective_lower_valid h heads bound d && Level_finite_spec.finite h tree} ->
    {u : unit | Level_finite_spec.finite (lower_heap h bound d) tree} @ ghost =
  fun h heads bound d tree premise -> ghost_ (
    let after = lower_heap h bound d in
    let observed : ((x : node Pref.t) @ immutable -> {u : unit |
        H.mem h x === H.mem after x && U.observe h x === U.observe after x}) @ total = fun x ->
      lowering_at h heads bound d x ();
      lower_frame_def h after x; U.observe_def h x; U.observe_def after x; () in
    Forest_transport.finite_frame h after observed tree (); ())

let (lower_bounded_at @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (bound : int) -> (t : bounded) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | effective_bounded h heads bound t && bound_root t === q} ->
    {u : unit | E.effective_below h heads q bound} @ ghost =
  fun h heads bound t q premise -> ghost_ (
    effective_bounded_def h heads bound t;
    ())

let (lower_active @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (bound : int) -> (d : lowering) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | effective_lower_valid h heads bound d} ->
    {u : unit | E.effective_active h heads x === E.effective_active (lower_heap h bound d) heads x} @ ghost =
  fun h heads bound d x premise -> ghost_ (
    let after = lower_heap h bound d in
    let frame : ((y : node Pref.t) @ immutable -> {u : unit | lower_frame h after y}) @ total = fun y ->
      lowering_at h heads bound d y (); () in
    frame_active h after heads frame x; ())

let (lower_below @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (bound : int) -> (d : lowering) @ immutable -> (x : node Pref.t) @ immutable -> (limit : int) ->
    {u : unit | effective_lower_valid h heads bound d && E.effective_below h heads x limit} ->
    {u : unit | E.effective_below (lower_heap h bound d) heads x limit} @ ghost =
  fun h heads bound d x limit premise -> ghost_ (
    let after = lower_heap h bound d in
    let frame : ((y : node Pref.t) @ immutable -> {u : unit | lower_frame h after y}) @ total = fun y ->
      lowering_at h heads bound d y (); () in
    frame_below h after heads frame x limit (); ())

let rec (lower_fixed @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total -> (bound : int) ->
    (d : lowering) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | effective_lower_valid h heads bound d && (below h x bound || at_level h x === Generic)} ->
    {u : unit | H.at (lower_heap h bound d) x === H.at h x} @ ghost = fun h heads bound d x premise -> ghost_ (
  effective_lower_valid_def h heads bound d; lower_heap_def h bound d;
  below_def h x bound; at_level_def h x;
  match d with Keep -> ()
  | Lower (p, old, rest) ->
    lower_fixed h heads bound rest x (); let mid = lower_heap h bound rest in
    lower_cell_def old bound; let v = lower_cell old bound in Copy_heap_proofs.put_frame mid p v x; ()
  | Sequence (a, b) -> lower_fixed h heads bound a x (); let mid = lower_heap h bound a in
    lowering_at h heads bound a x (); lower_frame_def h mid x; below_def mid x bound; at_level_def mid x;
    lower_fixed mid heads bound b x (); ())
