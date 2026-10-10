open Marked_occurs_proofs
open Copy_spec
open Level_spec
open Level_proofs
open Level_unifier_spec
open Level_unifier_proofs

let (redirect_active @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | active h p} ->
    {u : unit | active h x === active (H.put h p (redirect h p q)) x} @ ghost = fun h p q x premise -> ghost_ (
  active_def h p; at_level_def h p; redirect_def h p q;
  let after = H.put h p (redirect h p q) in active_def h x; active_def after x; at_level_def h x; at_level_def after x;
  ())
let (redirect_scope @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | active h p && active h q && (not (H.mem h x) || finite_scope h x)} ->
    {u : unit | not (H.mem (H.put h p (redirect h p q)) x) || finite_scope (H.put h p (redirect h p q)) x} @ ghost =
  fun h p q x premise -> ghost_ (
    active_def h p; active_def h q;
    let v = redirect h p q in let after = H.put h p v in redirect_def h p q;
    finite_scope_def h x; finite_scope_def after x; source_ok_def h x; source_ok_def after x;
    redirect_active h p q x (); redirect_active h p q q ();
    (match H.at h x with None -> () | Some old ->
      (match old.memo with Empty_memo | Forward _ -> () | Memo (stamp, _) -> ());
      (match old.desc with Var | Bool | Word -> () | Link a | List a -> redirect_active h p q a (); ()
      | Arrow (a, b) -> redirect_active h p q a (); redirect_active h p q b ();
        ())); ())
let rec (unified_active @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (ok : bool) -> (after : node Pref.heap) @ immutable ->
    (d : derivation) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q ok after d} ->
    {u : unit | not (active h x) || active after x} @ ghost = fun h p q ok after d x premise -> ghost_ (
  unified_def h p q ok after d; match d with
  | Same | Constants | Occurs_left _ | Occurs_right _ | Clash -> ()
  | Bind_left _ -> redirect_active h p q x (); ()
  | Bind_right _ -> redirect_active h q p x (); ()
  | Swap rest -> unified_active h q p ok after rest x (); ()
  | Resolve (a, b, _, _, rest) | List_children (a, b, rest) -> unified_active h a b ok after rest x (); ()
  | Scanned (needle, marks, rest) -> let mid = scan_heap h marks in
    scan_observe h needle marks x ();
    unified_active mid p q ok after rest x (); ()
  | Lowering (bound, edits, _, rest) -> let mid = lower_heap h bound edits in
    lowering_at h bound edits x (); frame_active h mid x ();
    unified_active mid p q ok after rest x (); ()
  | Children (a, b, c, e, mid, left_ok, left, right) ->
    unified_active h a c left_ok mid left x ();
    if left_ok then (unified_active mid b e ok after right x (); ()) else ())
let rec (unified_scope @ total) : (h : node Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || finite_scope h x})) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (ok : bool) -> (after : node Pref.heap) @ immutable ->
    (d : derivation) @ immutable -> (x : node Pref.t) @ immutable -> {u : unit | unified h p q ok after d} ->
    {u : unit | not (H.mem after x) || finite_scope after x} @ ghost = fun h scope p q ok after d x premise -> ghost_ (
  unified_def h p q ok after d; scope x; match d with
  | Same | Constants | Occurs_left _ | Occurs_right _ | Clash -> ()
  | Bind_left _ -> redirect_scope h p q x (); ()
  | Bind_right _ -> redirect_scope h q p x (); ()
  | Swap rest -> unified_scope h scope q p ok after rest x (); ()
  | Resolve (a, b, _, _, rest) | List_children (a, b, rest) -> unified_scope h scope a b ok after rest x (); ()
  | Scanned (needle, marks, rest) -> let mid = scan_heap h marks in
    let mid_scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem mid x) || finite_scope mid x}) @ total =
      fun x -> let () = scan_scope h scope needle marks x () in () in
    unified_scope mid mid_scope p q ok after rest x (); ()
  | Lowering (bound, edits, _, rest) -> let mid = lower_heap h bound edits in
    let mid_scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem mid x) || finite_scope mid x}) @ total =
      fun x -> let () = lower_scope h scope bound edits x () in () in
    unified_scope mid mid_scope p q ok after rest x (); ()
  | Children (a, b, c, e, mid, left_ok, left, right) ->
    let mid_scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem mid x) || finite_scope mid x}) @ total =
      fun x -> let () = unified_scope h scope a c left_ok mid left x () in () in
    if left_ok then (unified_scope mid mid_scope b e ok after right x (); ()) else (mid_scope x; ()))
let rec (resolution_active @ total) : (h : node Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || finite_scope h x})) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (d : resolution) @ immutable ->
    {u : unit | resolves h p q d && active h p} -> {u : unit | active h q} @ ghost = fun h scope p q d premise -> ghost_ (
  resolves_def h p q d; match d with Here -> ()
  | Via (a, rest) -> scope p; finite_scope_def h p; observe_def h p;
    resolution_active h scope a q rest (); ())
let (finite_scoped @ total) : (h : node Pref.heap) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | finite_scope h x} -> {u : unit | scoped h x} @ ghost = fun h x premise -> ghost_ (
  finite_scope_def h x; source_ok_def h x; scoped_def h x; observe_def h x;
  ())

let rec (lower_searched @ total) : (h : node Pref.heap) @ immutable -> (bound : int) -> (edits : lowering) @ immutable ->
    (needle : node Pref.t) @ immutable -> (p : node Pref.t) @ immutable -> (found : bool) -> (d : search) @ immutable ->
    {u : unit | lower_valid h bound edits && searched h needle p found d} ->
    {u : unit | searched (lower_heap h bound edits) needle p found d} @ ghost = fun h bound edits needle p found d premise -> ghost_ (
  let after = lower_heap h bound edits in
  searched_def h needle p found d; searched_def after needle p found d;
  lower_observe h bound edits p ();
  match d with Hit | Leaf -> ()
  | Follow (q, rest) -> lower_searched h bound edits needle q found rest (); ()
  | Left (a, _, left) -> let flag = true in lower_searched h bound edits needle a flag left (); ()
  | Both (a, b, left, right) -> let flag = false in lower_searched h bound edits needle a flag left ();
    lower_searched h bound edits needle b found right (); ())

let (redirect_below @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (x : node Pref.t) @ immutable -> (bound : int) ->
    {u : unit | active h p} ->
    {u : unit | below h x bound === below (H.put h p (redirect h p q)) x bound} @ ghost = fun h p q x bound premise -> ghost_ (
  active_def h p; at_level_def h p; redirect_def h p q;
  let after = H.put h p (redirect h p q) in below_def h x bound; below_def after x bound;
  at_level_def h x; at_level_def after x; ())
let (redirect_children @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (desc : desc) @ immutable -> (bound : int) ->
    {u : unit | active h p} ->
    {u : unit | children_below h desc bound === children_below (H.put h p (redirect h p q)) desc bound} @ ghost =
  fun h p q desc bound premise -> ghost_ (
    let after = H.put h p (redirect h p q) in
    children_below_def h desc bound; children_below_def after desc bound; match desc with
    | Var | Bool | Word -> () | Link a | List a -> redirect_below h p q a bound (); ()
    | Arrow (a, b) -> redirect_below h p q a bound (); redirect_below h p q b bound (); ())
let (redirect_ordered @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | active h p && ordered h x && match at_level h p with Generic -> false | Finite n -> below h q n} ->
    {u : unit | ordered (H.put h p (redirect h p q)) x} @ ghost = fun h p q x premise -> ghost_ (
  active_def h p; at_level_def h p; redirect_def h p q;
  let after = H.put h p (redirect h p q) in ordered_def h x; ordered_def after x;
  (match H.at h p with None -> () | Some v -> match v.level with Generic -> () | Finite n ->
    redirect_below h p q q n (); let desc = Link q in children_below_def after desc n; ());
  (match H.at h x with None -> () | Some v -> match v.level with Generic -> () | Finite n ->
    redirect_children h p q v.desc n (); ()); ())
let rec (unified_ordered @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (ok : bool) -> (after : node Pref.heap) @ immutable ->
    (d : derivation) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q ok after d && ordered h x} ->
    {u : unit | ordered after x} @ ghost = fun h p q ok after d x premise -> ghost_ (
  unified_def h p q ok after d; match d with
  | Same | Constants | Occurs_left _ | Occurs_right _ | Clash -> ()
  | Bind_left _ -> redirect_ordered h p q x (); ()
  | Bind_right _ -> redirect_ordered h q p x (); ()
  | Swap rest -> unified_ordered h q p ok after rest x (); ()
  | Resolve (a, b, _, _, rest) | List_children (a, b, rest) -> unified_ordered h a b ok after rest x (); ()
  | Scanned (needle, marks, rest) -> let mid = scan_heap h marks in
    scan_ordered h needle marks x ();
    unified_ordered mid p q ok after rest x (); ()
  | Lowering (bound, edits, _, rest) -> let mid = lower_heap h bound edits in
    lowering_ordered h bound edits x (); unified_ordered mid p q ok after rest x (); ()
  | Children (a, b, c, e, mid, left_ok, left, right) ->
    unified_ordered h a c left_ok mid left x ();
    if left_ok then (unified_ordered mid b e ok after right x (); ()) else ())

let (copy_equation @ total) : (h : node Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (x : node Pref.t) @ immutable ->
    {u : unit | node_equation h rho x === Copy_spec.equation h rho x} @ ghost = fun h rho x -> ghost_ (
  observe_def h x; node_equation_def h rho x; Copy_spec.equation_def h rho x; ())

let[@def] (scratch_frame @ total) (h : node Pref.heap @ immutable) (after : node Pref.heap @ immutable)
    (x : node Pref.t @ immutable) = ghost_ (match H.at h x, H.at after x with
  | None, None -> true | Some a, Some b -> a.memo === b.memo && a.visited === b.visited && decreases a.level b.level
    && (not (a.level === Generic) || a === b) | _ -> false)
let (scratch_trans @ total) : (h : node Pref.heap) @ immutable -> (mid : node Pref.heap) @ immutable ->
    (after : node Pref.heap) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | scratch_frame h mid x && scratch_frame mid after x} ->
    {u : unit | scratch_frame h after x} @ ghost = fun h mid after x premise -> ghost_ (
  scratch_frame_def h mid x; scratch_frame_def mid after x; scratch_frame_def h after x;
  (match H.at h x, H.at mid x, H.at after x with Some a, Some b, Some c ->
    decreases_def a.level b.level; decreases_def b.level c.level; decreases_def a.level c.level; () | _ -> ());
  ())
let (redirect_scratch @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (x : node Pref.t) @ immutable -> {u : unit | active h p} ->
    {u : unit | scratch_frame h (H.put h p (redirect h p q)) x} @ ghost = fun h p q x premise -> ghost_ (
  active_def h p; at_level_def h p; redirect_def h p q;
  let after = H.put h p (redirect h p q) in scratch_frame_def h after x;
  (match H.at h x, H.at after x with Some a, Some b -> decreases_def a.level b.level; () | _ -> ());
  ())
let (lower_scratch @ total) : (h : node Pref.heap) @ immutable -> (bound : int) ->
    (d : lowering) @ immutable -> (x : node Pref.t) @ immutable -> {u : unit | lower_valid h bound d} ->
    {u : unit | scratch_frame h (lower_heap h bound d) x} @ ghost = fun h bound d x premise -> ghost_ (
  let after = lower_heap h bound d in lowering_at h bound d x (); lower_frame_def h after x; scratch_frame_def h after x;
  at_level_def h x; if at_level h x === Generic then (lower_fixed h bound d x (); ()) else ())
let rec (unified_scratch @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (ok : bool) -> (after : node Pref.heap) @ immutable ->
    (d : derivation) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q ok after d} -> {u : unit | scratch_frame h after x} @ ghost = fun h p q ok after d x premise -> ghost_ (
  unified_def h p q ok after d; match d with
  | Same | Constants | Occurs_left _ | Occurs_right _ | Clash ->
    scratch_frame_def h h x; (match H.at h x with None -> () | Some v -> decreases_def v.level v.level; ()); ()
  | Bind_left _ -> redirect_scratch h p q x (); ()
  | Bind_right _ -> redirect_scratch h q p x (); ()
  | Swap rest -> unified_scratch h q p ok after rest x (); ()
  | Resolve (a, b, _, _, rest) | List_children (a, b, rest) -> unified_scratch h a b ok after rest x (); ()
  | Scanned (needle, marks, rest) -> let mid = scan_heap h marks in
    unified_scratch mid p q ok after rest x ();
    scan_at h needle marks x ();
    scratch_frame_def mid after x; scratch_frame_def h after x; ()
  | Lowering (bound, edits, _, rest) -> let mid = lower_heap h bound edits in
    lower_scratch h bound edits x (); unified_scratch mid p q ok after rest x ();
    scratch_trans h mid after x (); ()
  | Children (a, b, c, e, mid, left_ok, left, right) ->
    unified_scratch h a c left_ok mid left x ();
    if left_ok then (unified_scratch mid b e ok after right x (); scratch_trans h mid after x (); ()) else ())
