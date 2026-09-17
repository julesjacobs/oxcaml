open Copy_spec
open Level_spec
open Level_unifier_spec
module E = Effective_level
module P = Effective_lower_proofs
module M = Marked_occurs_proofs

let (head @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (needle : node Pref.t) @ immutable -> (marks : marks) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | marks_valid h needle marks && E.valid_head h heads x} ->
    {u : unit | E.valid_head (scan_heap h marks) heads x} @ ghost =
  fun h heads needle marks x premise -> ghost_ (
    let refine_ premise = premise in let after = scan_heap h marks in
    let frame : ((y : node Pref.t) @ immutable -> {u : unit | lower_frame h after y}) @ total = fun y ->
      let u = () in let refine_ out = M.scan_frame h needle marks y (refine_ u) in refine_ out in
    let u = () in P.frame_head h after heads frame x (refine_ u); refine_ u)

let (scope @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (needle : node Pref.t) @ immutable -> (marks : marks) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | marks_valid h needle marks && (not (H.mem h x) || E.effective_scope h heads x)} ->
    {u : unit | not (H.mem (scan_heap h marks) x) || E.effective_scope (scan_heap h marks) heads x} @ ghost =
  fun h heads needle marks x premise -> ghost_ (
    let refine_ premise = premise in let after = scan_heap h marks in
    let frame : ((y : node Pref.t) @ immutable -> {u : unit | lower_frame h after y}) @ total = fun y ->
      let u = () in let refine_ out = M.scan_frame h needle marks y (refine_ u) in refine_ out in
    let u = () in P.frame_scope h after heads frame x (refine_ u); refine_ u)

let (level @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (needle : node Pref.t) @ immutable -> (marks : marks) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | marks_valid h needle marks} ->
    {u : unit | E.level h heads x === E.level (scan_heap h marks) heads x} @ ghost =
  fun h heads needle marks x premise -> ghost_ (
    let refine_ premise = premise in let after = scan_heap h marks in let u = () in
    M.scan_at h needle marks x (refine_ u); let r = heads x in M.scan_at h needle marks r.root (refine_ u);
    E.level_def h heads x; E.level_def after heads x;
    at_level_def h r.root; at_level_def after r.root; refine_ u)

let (order @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (needle : node Pref.t) @ immutable -> (marks : marks) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | marks_valid h needle marks && E.effective_ordered h heads x} ->
    {u : unit | E.effective_ordered (scan_heap h marks) heads x} @ ghost =
  fun h heads needle marks x premise -> ghost_ (
    let refine_ premise = premise in let after = scan_heap h marks in let u = () in
    M.scan_at h needle marks x (refine_ u);
    E.effective_ordered_def h heads x; E.effective_ordered_def after heads x;
    (match H.at h x with None -> () | Some v -> match v.desc, v.level with
      | Arrow (a, b), Finite n ->
        M.scan_at h needle marks a (refine_ u); M.scan_at h needle marks b (refine_ u);
        level h heads needle marks a (refine_ u); level h heads needle marks b (refine_ u);
        E.effective_below_def h heads a n; E.effective_below_def h heads b n;
        E.effective_below_def after heads a n; E.effective_below_def after heads b n; ()
      | _ -> ()); refine_ u)
