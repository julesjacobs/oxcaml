open Copy_spec
open Copy_heap_proofs
open Level_spec

let (frame_refl @ total) : (h : node Pref.heap) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | lower_frame h h x} @ ghost = fun h x -> ghost_ (
  lower_frame_def h h x; (match H.at h x with None -> () | Some v -> decreases_def v.level v.level; ());
  let u = () in refine_ u)
let (frame_trans @ total) : (h : node Pref.heap) @ immutable -> (mid : node Pref.heap) @ immutable ->
    (after : node Pref.heap) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | lower_frame h mid x && lower_frame mid after x} ->
    {u : unit | lower_frame h after x} @ ghost = fun h mid after x premise -> ghost_ (
  let refine_ premise = premise in lower_frame_def h mid x; lower_frame_def mid after x; lower_frame_def h after x;
  (match H.at h x, H.at mid x, H.at after x with Some a, Some b, Some c ->
    decreases_def a.level b.level; decreases_def b.level c.level; decreases_def a.level c.level; () | _ -> ());
  let u = () in refine_ u)
let (write_frame @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (old : node) @ immutable -> (bound : int) -> (x : node Pref.t) @ immutable ->
    {u : unit | H.mem h p && H.at h p === Some old && bound >= 0 &&
      match old.level with Generic -> false | Finite n -> n >= 0} ->
    {u : unit | lower_frame h (H.put h p (lower_cell old bound)) x} @ ghost = fun h p old bound x premise -> ghost_ (
  let refine_ premise = premise in let v = lower_cell old bound in lower_cell_def old bound;
  let after = H.put h p v in put_frame h p v x; lower_frame_def h after x;
  (match H.at h x, H.at after x with Some a, Some b -> decreases_def a.level b.level; () | _ -> ());
  let u = () in refine_ u)
let rec (lowering_at @ total) : (h : node Pref.heap) @ immutable -> (bound : int) ->
    (d : lowering) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | lower_valid h bound d} -> {u : unit | lower_frame h (lower_heap h bound d) x} @ ghost =
  fun h bound d x premise -> ghost_ (
    let refine_ premise = premise in lower_valid_def h bound d; lower_heap_def h bound d;
    let u = () in match d with Keep -> frame_refl h x; refine_ u
    | Lower (p, old, rest) ->
      lowering_at h bound rest x (refine_ u); let mid = lower_heap h bound rest in
      write_frame mid p old bound x (refine_ u); let after = lower_heap h bound d in
      frame_trans h mid after x (refine_ u); refine_ u
    | Sequence (a, b) ->
      lowering_at h bound a x (refine_ u); let mid = lower_heap h bound a in
      lowering_at mid bound b x (refine_ u); let after = lower_heap h bound d in
      frame_trans h mid after x (refine_ u); refine_ u)
let (frame_active @ total) : (h : node Pref.heap) @ immutable -> (after : node Pref.heap) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | lower_frame h after x} ->
    {u : unit | not (active h x) || active after x} @ ghost = fun h after x premise -> ghost_ (
  let refine_ premise = premise in lower_frame_def h after x; active_def h x; active_def after x;
  at_level_def h x; at_level_def after x;
  (match H.at h x, H.at after x with Some a, Some b -> decreases_def a.level b.level; () | _ -> ());
  let u = () in refine_ u)
let (frame_below @ total) : (h : node Pref.heap) @ immutable -> (after : node Pref.heap) @ immutable ->
    (x : node Pref.t) @ immutable -> (bound : int) -> {u : unit | lower_frame h after x && below h x bound} ->
    {u : unit | below after x bound} @ ghost = fun h after x bound premise -> ghost_ (
  let refine_ premise = premise in lower_frame_def h after x; below_def h x bound; below_def after x bound;
  at_level_def h x; at_level_def after x;
  (match H.at h x, H.at after x with Some a, Some b -> decreases_def a.level b.level; () | _ -> ());
  let u = () in refine_ u)
let (frame_scope @ total) : (h : node Pref.heap) @ immutable -> (after : node Pref.heap) @ immutable ->
    (frame : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame h after x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || finite_scope h x} ->
    {u : unit | not (H.mem after x) || finite_scope after x} @ ghost = fun h after frame x premise -> ghost_ (
  let refine_ premise = premise in frame x; lower_frame_def h after x;
  finite_scope_def h x; finite_scope_def after x; source_ok_def h x; source_ok_def after x;
  active_def h x; active_def after x; at_level_def h x; at_level_def after x;
  let u = () in match H.at h x, H.at after x with
  | Some a, Some b -> decreases_def a.level b.level;
    (match a.memo with Empty_memo -> () | Memo (stamp, _) -> frame stamp; lower_frame_def h after stamp; ());
    (match a.desc with Var | Bool -> () | Link q -> frame q; lower_frame_def h after q; frame_active h after q (refine_ u); ()
    | Arrow (a, b) -> frame a; frame b; lower_frame_def h after a; lower_frame_def h after b;
      frame_active h after a (refine_ u); frame_active h after b (refine_ u); ()); refine_ u
  | _ -> refine_ u)
let (frame_model @ total) : (h : node Pref.heap) @ immutable -> (after : node Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | lower_frame h after x} ->
    {u : unit | equation h rho x === equation after rho x} @ ghost = fun h after rho x premise -> ghost_ (
  let refine_ premise = premise in lower_frame_def h after x; equation_def h rho x; equation_def after rho x;
  let u = () in refine_ u)
let rec (bounded_frame @ total) : (h : node Pref.heap) @ immutable -> (after : node Pref.heap) @ immutable ->
    (frame : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame h after x})) @ total ->
    (limit : int) -> (t : bounded) @ immutable -> {u : unit | bounded h limit t} ->
    {u : unit | bounded after limit t} @ ghost = fun h after frame limit t premise -> ghost_ (
  let refine_ premise = premise in bounded_def h limit t; bounded_def after limit t; bound_root_def t;
  let p = bound_root t in frame p; lower_frame_def h after p;
  let u = () in frame_below h after p limit (refine_ u);
  match t with Tip _ -> refine_ u
  | Through (_, c) -> bounded_frame h after frame limit c (refine_ u); refine_ u
  | Fork (_, a, b) -> bounded_frame h after frame limit a (refine_ u);
    bounded_frame h after frame limit b (refine_ u); refine_ u)

let (lower_scope @ total) : (h : node Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || finite_scope h x})) @ total ->
    (bound : int) -> (d : lowering) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | lower_valid h bound d} ->
    {u : unit | not (H.mem (lower_heap h bound d) x) || finite_scope (lower_heap h bound d) x} @ ghost =
  fun h scope bound d x premise -> ghost_ (
    let refine_ premise = premise in let after = lower_heap h bound d in
    let frame : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame h after x}) @ total =
      fun x -> let u = () in let refine_ u = lowering_at h bound d x (refine_ u) in refine_ u in
    scope x; let u = () in let refine_ u = frame_scope h after frame x (refine_ u) in refine_ u)

let (lower_bounded_at @ total) : (h : node Pref.heap) @ immutable -> (bound : int) ->
    (t : bounded) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | bounded h bound t && bound_root t === q} ->
    {u : unit | below h q bound} @ ghost = fun h bound t q premise -> ghost_ (
  let refine_ premise = premise in bounded_def h bound t; let u = () in refine_ u)

let (children_frame @ total) : (h : node Pref.heap) @ immutable -> (after : node Pref.heap) @ immutable ->
    (frame : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame h after x})) @ total ->
    (desc : desc) @ immutable -> (bound : int) -> {u : unit | children_below h desc bound} ->
    {u : unit | children_below after desc bound} @ ghost = fun h after frame desc bound premise -> ghost_ (
  let refine_ premise = premise in children_below_def h desc bound; children_below_def after desc bound;
  let u = () in match desc with Var | Bool -> refine_ u
  | Link q -> frame q; frame_below h after q bound (refine_ u); refine_ u
  | Arrow (a, b) -> frame a; frame b; frame_below h after a bound (refine_ u);
    frame_below h after b bound (refine_ u); refine_ u)
let (write_ordered @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (old : node) @ immutable -> (bound : int) -> (x : node Pref.t) @ immutable ->
    {u : unit | H.mem h p && H.at h p === Some old && bound >= 0 && ordered h x
      && children_below h old.desc bound && match old.level with Generic -> false | Finite n -> n >= 0} ->
    {u : unit | ordered (H.put h p (lower_cell old bound)) x} @ ghost = fun h p old bound x premise -> ghost_ (
  let refine_ premise = premise in let v = lower_cell old bound in lower_cell_def old bound;
  let after = H.put h p v in
  let frame : ((y : node Pref.t) @ immutable -> {u : unit | lower_frame h after y}) @ total = fun y ->
    let u = () in let refine_ u = write_frame h p old bound y (refine_ u) in refine_ u in
  frame x; lower_frame_def h after x; ordered_def h x; ordered_def after x;
  let u = () in children_frame h after frame old.desc bound (refine_ u);
  (match H.at h x with None -> () | Some before -> match before.level with Generic -> () | Finite n ->
    children_frame h after frame before.desc n (refine_ u); ()); refine_ u)
let rec (lowering_ordered @ total) : (h : node Pref.heap) @ immutable -> (bound : int) ->
    (d : lowering) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | lower_valid h bound d && ordered h x} ->
    {u : unit | ordered (lower_heap h bound d) x} @ ghost = fun h bound d x premise -> ghost_ (
  let refine_ premise = premise in lower_valid_def h bound d; lower_heap_def h bound d;
  let u = () in match d with Keep -> refine_ u
  | Lower (p, old, rest) -> lowering_ordered h bound rest x (refine_ u);
    let mid = lower_heap h bound rest in write_ordered mid p old bound x (refine_ u); refine_ u
  | Sequence (a, b) -> lowering_ordered h bound a x (refine_ u); let mid = lower_heap h bound a in
    lowering_ordered mid bound b x (refine_ u); refine_ u)

let rec (lower_fixed @ total) : (h : node Pref.heap) @ immutable -> (bound : int) ->
    (d : lowering) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | lower_valid h bound d && (below h x bound || at_level h x === Generic)} ->
    {u : unit | H.at (lower_heap h bound d) x === H.at h x} @ ghost = fun h bound d x premise -> ghost_ (
  let refine_ premise = premise in lower_valid_def h bound d; lower_heap_def h bound d;
  below_def h x bound; at_level_def h x;
  let u = () in match d with Keep -> refine_ u
  | Lower (p, old, rest) ->
    lower_fixed h bound rest x (refine_ u); let mid = lower_heap h bound rest in
    lower_cell_def old bound; let v = lower_cell old bound in Copy_heap_proofs.put_frame mid p v x; refine_ u
  | Sequence (a, b) -> lower_fixed h bound a x (refine_ u); let mid = lower_heap h bound a in
    lowering_at h bound a x (refine_ u); lower_frame_def h mid x; below_def mid x bound; at_level_def mid x;
    lower_fixed mid bound b x (refine_ u); refine_ u)
