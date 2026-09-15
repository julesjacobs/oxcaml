open Copy_spec
open Level_spec
open Level_proofs
open Level_unifier_spec
open Level_unifier_proofs

let (redirect_active @ total) : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | active h p} ->
    {u : unit | active h x === active (H.put h p (redirect h p q)) x} @ ghost = fun h p q x premise -> ghost_ (
  let refine_ premise = premise in active_def h p; at_level_def h p; redirect_def h p q;
  let after = H.put h p (redirect h p q) in active_def h x; active_def after x; at_level_def h x; at_level_def after x;
  let u = () in refine_ u)
let (redirect_scope @ total) : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | active h p && active h q && (not (H.mem h x) || finite_scope h x)} ->
    {u : unit | not (H.mem (H.put h p (redirect h p q)) x) || finite_scope (H.put h p (redirect h p q)) x} @ ghost =
  fun h p q x premise -> ghost_ (
    let refine_ premise = premise in active_def h p; active_def h q;
    let v = redirect h p q in let after = H.put h p v in redirect_def h p q;
    Copy_heap_proofs.put_frame h p v x;
    finite_scope_def h x; finite_scope_def after x; source_ok_def h x; source_ok_def after x;
    let u = () in redirect_active h p q x (refine_ u); redirect_active h p q q (refine_ u);
    (match H.at h x with None -> () | Some old ->
      (match old.memo with Empty_memo -> () | Memo (stamp, _) -> Copy_heap_proofs.put_frame h p v stamp; ());
      (match old.desc with Var | Bool -> () | Link a -> redirect_active h p q a (refine_ u); Copy_heap_proofs.put_frame h p v a; ()
      | Arrow (a, b) -> redirect_active h p q a (refine_ u); redirect_active h p q b (refine_ u);
        Copy_heap_proofs.put_frame h p v a; Copy_heap_proofs.put_frame h p v b; ())); refine_ u)
let rec (unified_active @ total) : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (ok : bool) -> (after : Pref.heap) @ immutable ->
    (d : derivation) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q ok after d} ->
    {u : unit | not (active h x) || active after x} @ ghost = fun h p q ok after d x premise -> ghost_ (
  let refine_ premise = premise in unified_def h p q ok after d; let u = () in match d with
  | Same | Constants | Occurs_left _ | Occurs_right _ | Clash -> refine_ u
  | Bind_left _ -> redirect_active h p q x (refine_ u); refine_ u
  | Bind_right _ -> redirect_active h q p x (refine_ u); refine_ u
  | Swap rest -> unified_active h q p ok after rest x (refine_ u); refine_ u
  | Resolve (a, b, _, _, rest) -> unified_active h a b ok after rest x (refine_ u); refine_ u
  | Lowering (bound, edits, _, rest) -> let mid = lower_heap h bound edits in
    lowering_at h bound edits x (refine_ u); frame_active h mid x (refine_ u);
    unified_active mid p q ok after rest x (refine_ u); refine_ u
  | Children (a, b, c, e, mid, left_ok, left, right) ->
    unified_active h a c left_ok mid left x (refine_ u);
    if left_ok then (unified_active mid b e ok after right x (refine_ u); refine_ u) else refine_ u)
let rec (unified_scope @ total) : (h : Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || finite_scope h x})) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (ok : bool) -> (after : Pref.heap) @ immutable ->
    (d : derivation) @ immutable -> (x : node Pref.t) @ immutable -> {u : unit | unified h p q ok after d} ->
    {u : unit | not (H.mem after x) || finite_scope after x} @ ghost = fun h scope p q ok after d x premise -> ghost_ (
  let refine_ premise = premise in unified_def h p q ok after d; scope x; let u = () in match d with
  | Same | Constants | Occurs_left _ | Occurs_right _ | Clash -> refine_ u
  | Bind_left _ -> redirect_scope h p q x (refine_ u); refine_ u
  | Bind_right _ -> redirect_scope h q p x (refine_ u); refine_ u
  | Swap rest -> unified_scope h scope q p ok after rest x (refine_ u); refine_ u
  | Resolve (a, b, _, _, rest) -> unified_scope h scope a b ok after rest x (refine_ u); refine_ u
  | Lowering (bound, edits, _, rest) -> let mid = lower_heap h bound edits in
    let mid_scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem mid x) || finite_scope mid x}) @ total =
      fun x -> let u = () in let refine_ u = lower_scope h scope bound edits x (refine_ u) in refine_ u in
    unified_scope mid mid_scope p q ok after rest x (refine_ u); refine_ u
  | Children (a, b, c, e, mid, left_ok, left, right) ->
    let mid_scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem mid x) || finite_scope mid x}) @ total =
      fun x -> let u = () in let refine_ u = unified_scope h scope a c left_ok mid left x (refine_ u) in refine_ u in
    if left_ok then (unified_scope mid mid_scope b e ok after right x (refine_ u); refine_ u) else (mid_scope x; refine_ u))
let rec (resolution_active @ total) : (h : Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || finite_scope h x})) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (d : resolution) @ immutable ->
    {u : unit | resolves h p q d && active h p} -> {u : unit | active h q} @ ghost = fun h scope p q d premise -> ghost_ (
  let refine_ premise = premise in resolves_def h p q d; let u = () in match d with Here -> refine_ u
  | Via (a, rest) -> scope p; finite_scope_def h p; observe_def h p;
    resolution_active h scope a q rest (refine_ u); refine_ u)
let (finite_scoped @ total) : (h : Pref.heap) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | finite_scope h x} -> {u : unit | scoped h x} @ ghost = fun h x premise -> ghost_ (
  let refine_ premise = premise in finite_scope_def h x; source_ok_def h x; scoped_def h x; observe_def h x;
  let u = () in refine_ u)

let rec (lower_searched @ total) : (h : Pref.heap) @ immutable -> (bound : int) -> (edits : lowering) @ immutable ->
    (needle : node Pref.t) @ immutable -> (p : node Pref.t) @ immutable -> (found : bool) -> (d : search) @ immutable ->
    {u : unit | lower_valid h bound edits && searched h needle p found d} ->
    {u : unit | searched (lower_heap h bound edits) needle p found d} @ ghost = fun h bound edits needle p found d premise -> ghost_ (
  let refine_ premise = premise in let after = lower_heap h bound edits in
  searched_def h needle p found d; searched_def after needle p found d;
  let u = () in lower_observe h bound edits p (refine_ u);
  match d with Hit | Leaf -> refine_ u
  | Follow (q, rest) -> lower_searched h bound edits needle q found rest (refine_ u); refine_ u
  | Left (a, _, left) -> let flag = true in lower_searched h bound edits needle a flag left (refine_ u); refine_ u
  | Both (a, b, left, right) -> let flag = false in lower_searched h bound edits needle a flag left (refine_ u);
    lower_searched h bound edits needle b found right (refine_ u); refine_ u)

let (redirect_below @ total) : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (x : node Pref.t) @ immutable -> (bound : int) ->
    {u : unit | active h p} ->
    {u : unit | below h x bound === below (H.put h p (redirect h p q)) x bound} @ ghost = fun h p q x bound premise -> ghost_ (
  let refine_ premise = premise in active_def h p; at_level_def h p; redirect_def h p q;
  let after = H.put h p (redirect h p q) in below_def h x bound; below_def after x bound;
  at_level_def h x; at_level_def after x; let u = () in refine_ u)
let (redirect_children @ total) : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (desc : desc) @ immutable -> (bound : int) ->
    {u : unit | active h p} ->
    {u : unit | children_below h desc bound === children_below (H.put h p (redirect h p q)) desc bound} @ ghost =
  fun h p q desc bound premise -> ghost_ (
    let refine_ premise = premise in let after = H.put h p (redirect h p q) in
    children_below_def h desc bound; children_below_def after desc bound; let u = () in match desc with
    | Var | Bool -> refine_ u | Link a -> redirect_below h p q a bound (refine_ u); refine_ u
    | Arrow (a, b) -> redirect_below h p q a bound (refine_ u); redirect_below h p q b bound (refine_ u); refine_ u)
let (redirect_ordered @ total) : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | active h p && ordered h x && match at_level h p with Generic -> false | Finite n -> below h q n} ->
    {u : unit | ordered (H.put h p (redirect h p q)) x} @ ghost = fun h p q x premise -> ghost_ (
  let refine_ premise = premise in active_def h p; at_level_def h p; redirect_def h p q;
  let after = H.put h p (redirect h p q) in ordered_def h x; ordered_def after x;
  let u = () in (match H.at h p with None -> () | Some v -> match v.level with Generic -> () | Finite n ->
    redirect_below h p q q n (refine_ u); let desc = Link q in children_below_def after desc n; ());
  (match H.at h x with None -> () | Some v -> match v.level with Generic -> () | Finite n ->
    redirect_children h p q v.desc n (refine_ u); ()); refine_ u)
let rec (unified_ordered @ total) : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (ok : bool) -> (after : Pref.heap) @ immutable ->
    (d : derivation) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q ok after d && ordered h x} ->
    {u : unit | ordered after x} @ ghost = fun h p q ok after d x premise -> ghost_ (
  let refine_ premise = premise in unified_def h p q ok after d; let u = () in match d with
  | Same | Constants | Occurs_left _ | Occurs_right _ | Clash -> refine_ u
  | Bind_left _ -> redirect_ordered h p q x (refine_ u); refine_ u
  | Bind_right _ -> redirect_ordered h q p x (refine_ u); refine_ u
  | Swap rest -> unified_ordered h q p ok after rest x (refine_ u); refine_ u
  | Resolve (a, b, _, _, rest) -> unified_ordered h a b ok after rest x (refine_ u); refine_ u
  | Lowering (bound, edits, _, rest) -> let mid = lower_heap h bound edits in
    lowering_ordered h bound edits x (refine_ u); unified_ordered mid p q ok after rest x (refine_ u); refine_ u
  | Children (a, b, c, e, mid, left_ok, left, right) ->
    unified_ordered h a c left_ok mid left x (refine_ u);
    if left_ok then (unified_ordered mid b e ok after right x (refine_ u); refine_ u) else refine_ u)

let (copy_equation @ total) : (h : Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (x : node Pref.t) @ immutable ->
    {u : unit | node_equation h rho x === Copy_spec.equation h rho x} @ ghost = fun h rho x -> ghost_ (
  observe_def h x; node_equation_def h rho x; Copy_spec.equation_def h rho x; let u = () in refine_ u)

let[@def] (scratch_frame @ total) (h : Pref.heap @ immutable) (after : Pref.heap @ immutable)
    (x : node Pref.t @ immutable) = ghost_ (match H.at h x, H.at after x with
  | None, None -> true | Some a, Some b -> a.memo === b.memo && decreases a.level b.level
    && (not (a.level === Generic) || a === b) | _ -> false)
let (scratch_trans @ total) : (h : Pref.heap) @ immutable -> (mid : Pref.heap) @ immutable ->
    (after : Pref.heap) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | scratch_frame h mid x && scratch_frame mid after x} ->
    {u : unit | scratch_frame h after x} @ ghost = fun h mid after x premise -> ghost_ (
  let refine_ premise = premise in scratch_frame_def h mid x; scratch_frame_def mid after x; scratch_frame_def h after x;
  (match H.at h x, H.at mid x, H.at after x with Some a, Some b, Some c ->
    decreases_def a.level b.level; decreases_def b.level c.level; decreases_def a.level c.level; () | _ -> ());
  let u = () in refine_ u)
let (redirect_scratch @ total) : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (x : node Pref.t) @ immutable -> {u : unit | active h p} ->
    {u : unit | scratch_frame h (H.put h p (redirect h p q)) x} @ ghost = fun h p q x premise -> ghost_ (
  let refine_ premise = premise in active_def h p; at_level_def h p; redirect_def h p q;
  let after = H.put h p (redirect h p q) in scratch_frame_def h after x;
  (match H.at h x, H.at after x with Some a, Some b -> decreases_def a.level b.level; () | _ -> ());
  let u = () in refine_ u)
let (lower_scratch @ total) : (h : Pref.heap) @ immutable -> (bound : int) ->
    (d : lowering) @ immutable -> (x : node Pref.t) @ immutable -> {u : unit | lower_valid h bound d} ->
    {u : unit | scratch_frame h (lower_heap h bound d) x} @ ghost = fun h bound d x premise -> ghost_ (
  let refine_ premise = premise in let after = lower_heap h bound d in let u = () in
  lowering_at h bound d x (refine_ u); lower_frame_def h after x; scratch_frame_def h after x;
  at_level_def h x; if at_level h x === Generic then (lower_fixed h bound d x (refine_ u); refine_ u) else refine_ u)
let rec (unified_scratch @ total) : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (ok : bool) -> (after : Pref.heap) @ immutable ->
    (d : derivation) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q ok after d} -> {u : unit | scratch_frame h after x} @ ghost = fun h p q ok after d x premise -> ghost_ (
  let refine_ premise = premise in unified_def h p q ok after d; let u = () in match d with
  | Same | Constants | Occurs_left _ | Occurs_right _ | Clash ->
    scratch_frame_def h h x; (match H.at h x with None -> () | Some v -> decreases_def v.level v.level; ()); refine_ u
  | Bind_left _ -> redirect_scratch h p q x (refine_ u); refine_ u
  | Bind_right _ -> redirect_scratch h q p x (refine_ u); refine_ u
  | Swap rest -> unified_scratch h q p ok after rest x (refine_ u); refine_ u
  | Resolve (a, b, _, _, rest) -> unified_scratch h a b ok after rest x (refine_ u); refine_ u
  | Lowering (bound, edits, _, rest) -> let mid = lower_heap h bound edits in
    lower_scratch h bound edits x (refine_ u); unified_scratch mid p q ok after rest x (refine_ u);
    scratch_trans h mid after x (refine_ u); refine_ u
  | Children (a, b, c, e, mid, left_ok, left, right) ->
    unified_scratch h a c left_ok mid left x (refine_ u);
    if left_ok then (unified_scratch mid b e ok after right x (refine_ u); scratch_trans h mid after x (refine_ u); refine_ u) else refine_ u)
