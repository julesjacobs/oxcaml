open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec
module R = Representative_level
module C = Effective_compression_proofs

let[@def] (progress @ total) (h : node Pref.heap @ immutable) (root : node Pref.t @ immutable)
    (after : node Pref.heap @ immutable) (next : node Pref.t @ immutable) = ghost_ (
  decreases (at_level h root) (at_level after next)
  && (not (at_level h root === Generic) || next === root))

let (progress_refl @ total) : (h : node Pref.heap) @ immutable -> (root : node Pref.t) @ immutable ->
    {u : unit | progress h root h root} @ ghost = fun h root -> ghost_ (
      progress_def h root h root; let n = at_level h root in decreases_def n n;
      ())

let (progress_trans @ total) : (h : node Pref.heap) @ immutable -> (root : node Pref.t) @ immutable ->
    (middle : node Pref.heap) @ immutable -> (mid : node Pref.t) @ immutable ->
    (after : node Pref.heap) @ immutable -> (next : node Pref.t) @ immutable ->
    {u : unit | progress h root middle mid && progress middle mid after next} ->
    {u : unit | progress h root after next} @ ghost = fun h root middle mid after next premise -> ghost_ (
      progress_def h root middle mid;
      progress_def middle mid after next; progress_def h root after next;
      let a = at_level h root in let b = at_level middle mid in let c = at_level after next in
      decreases_def a b; decreases_def b c; decreases_def a c; ())

let (framed @ total) : (h : node Pref.heap) @ immutable -> (after : node Pref.heap) @ immutable ->
    (frame : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame h after x})) @ total ->
    (p : node Pref.t) @ immutable -> (root : node Pref.t) @ immutable -> (path : resolution) @ immutable ->
    {u : unit | resolves h p root path} ->
    {u : unit | resolves after p root path && progress h root after root} @ ghost =
  fun h after frame p root path premise -> ghost_ (
    let observed : ((x : node Pref.t) @ immutable -> {u : unit | H.mem h x === H.mem after x
        && observe h x === observe after x}) @ total = fun x ->
      frame x; lower_frame_def h after x; observe_def h x; observe_def after x; () in
    R.resolution_frame h after observed p root path;
    Compression_path_proofs.resolution_terminal h p root path ();
    terminal_def h root; observe_def h root; frame root; lower_frame_def h after root;
    progress_def h root after root; at_level_def h root; at_level_def after root; ())

let (redirect @ total) : (h : node Pref.heap) @ immutable ->
    (source : node Pref.t) @ immutable -> (target : node Pref.t) @ immutable ->
    (p : node Pref.t) @ immutable -> (root : node Pref.t) @ immutable -> (path : resolution) @ immutable ->
    {u : unit | H.mem h source && terminal h source && H.mem h target && terminal h target
      && not (source === target) && resolves h p root path
      && (match at_level h source with Generic -> false | Finite n -> n >= 0 && below h target n)} ->
    {r : R.representative | let after = H.put h source (Level_unifier_spec.redirect h source target) in
      resolves after p r.root r.path && progress h root after r.root} @ immutable ghost =
  fun h source target p root path premise -> ghost_ (
    let next_path = Representative_mutation.redirect_head h source target p root path () in
    let next = if root === source then target else root in
    let v = Level_unifier_spec.redirect h source target in let after = H.put h source v in
    progress_def h root after next; at_level_def h root; at_level_def after next;
    at_level_def h source; at_level_def h target;
    (match at_level h source with Generic -> () | Finite n -> below_def h target n; ());
    let a = at_level h root in let b = at_level after next in decreases_def a b;
    let out = {R.root = next; path = next_path} in out)

open Effective_unifier_spec
let rec (unified_head @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (ok : bool) ->
    (after : node Pref.heap) @ immutable -> (d : derivation) @ immutable ->
    (x : node Pref.t) @ immutable -> (root : node Pref.t) @ immutable -> (path : resolution) @ immutable ->
    {u : unit | unified h p q ok after d && resolves h x root path} ->
    {r : R.representative | resolves after x r.root r.path && progress h root after r.root} @ immutable ghost =
  fun h p q ok after d x root path premise -> ghost_ (
    unified_def h p q ok after d; match d with
    | Base old -> U.unified_def h p q ok after old;
      (match old with
      | U.Bind_left _ ->
        active_def h p;
        let out = redirect h p q x root path () in out
      | U.Bind_right _ ->
        active_def h q;
        let out = redirect h q p x root path () in out
      | U.Same | U.Constants | U.Occurs_left _ | U.Occurs_right _ | U.Clash ->
        progress_refl h root; let out = {R.root; path} in out
      | _ -> let out = {R.root; path} in out)
    | Swap rest -> let out = unified_head h q p ok after rest x root path () in out
    | Resolve (r, s, _, _, rest) -> let out = unified_head h r s ok after rest x root path () in out
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      let first = unified_head h a c left_ok middle left x root path () in
      if left_ok then (
        let second = unified_head middle b e ok after right x first.root first.path () in
        progress_trans h root middle first.root after second.root (); second)
      else first
    | Post_link (middle, rest, source, target) ->
      let first = unified_head h p q true middle rest x root path () in
      Structure_spec.linkable_def middle source target;
      finite_def middle source; finite_def middle target;
      let s = tree_root source in let t = tree_root target in active_def middle s;
      let second = redirect middle s t x first.root first.path () in
      progress_trans h root middle first.root after second.root (); second
    | Pre_compress (middle, edits, rest) ->
      let next = C.resolution h middle edits x root path () in
      C.frame h middle edits root ();
      progress_def h root middle root; let n = at_level h root in decreases_def n n;
      let out = unified_head middle p q ok after rest x root next () in
      progress_trans h root middle root after out.root (); out
    | Scanned (needle, marks, rest) ->
      let middle = scan_heap h marks in
      let frame : ((y : node Pref.t) @ immutable -> {u : unit | lower_frame h middle y}) @ total = fun y ->
        let out = Marked_occurs_proofs.scan_frame h needle marks y () in out in
      framed h middle frame x root path ();
      let out = unified_head middle p q ok after rest x root path () in
      progress_trans h root middle root after out.root (); out
    | Terminal_lower (bound, edits, tree, rest) ->
      let middle = lower_heap h bound edits in Terminal_lower_spec.completed_def h bound q middle edits tree;
      let frame : ((y : node Pref.t) @ immutable -> {u : unit | lower_frame h middle y}) @ total = fun y ->
        let out = Terminal_lower_proofs.lowering_at h bound edits y () in out in
      framed h middle frame x root path ();
      let out = unified_head middle p q ok after rest x root path () in
      progress_trans h root middle root after out.root (); out)
