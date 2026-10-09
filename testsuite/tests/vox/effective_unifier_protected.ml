open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec
open Effective_unifier_spec
module P = Effective_unifier_heads
module R = Representative_level
module E = Effective_level

let (redirect_generic @ total) : (h : node Pref.heap) @ immutable ->
    (source : node Pref.t) @ immutable -> (target : node Pref.t) @ immutable ->
    (x : node Pref.t) @ immutable -> (root : node Pref.t) @ immutable -> (path : resolution) @ immutable ->
    {u : unit | H.mem h source && terminal h source && active h source && resolves h x root path && at_level h root === Generic} ->
    {u : unit | H.at h x === H.at (H.put h source (U.redirect h source target)) x} @ ghost =
  fun h source target x root path premise -> ghost_ (
    active_def h source;
    if x === source then (
      let here = Here in resolves_def h source source here;
      R.unique h x root path source here (); ()) else ();
    let _ = U.redirect h source target in ())

let rec (generic @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (ok : bool) ->
    (after : node Pref.heap) @ immutable -> (d : derivation) @ immutable ->
    (x : node Pref.t) @ immutable -> (root : node Pref.t) @ immutable -> (path : resolution) @ immutable ->
    {u : unit | unified h p q ok after d && resolves h x root path && at_level h root === Generic} ->
    {u : unit | H.at h x === H.at after x} @ ghost =
  fun h p q ok after d x root path premise -> ghost_ (
    unified_def h p q ok after d; match d with
    | Base old -> U.unified_def h p q ok after old;
      (match old with
      | U.Bind_left _ -> redirect_generic h p q x root path (); ()
      | U.Bind_right _ -> redirect_generic h q p x root path (); ()
      | _ -> ())
    | Swap rest -> generic h q p ok after rest x root path (); ()
    | Resolve (r, s, _, _, rest) -> generic h r s ok after rest x root path (); ()
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      generic h a c left_ok middle left x root path ();
      if left_ok then (
        let next = P.unified_head h a c left_ok middle left x root path () in
        P.progress_def h root middle next.root; let n = at_level middle next.root in let g = Generic in decreases_def g n;
        generic middle b e ok after right x next.root next.path (); ()) else ();
      ()
    | Post_link (middle, rest, source, target) ->
      generic h p q true middle rest x root path ();
      let next = P.unified_head h p q true middle rest x root path () in
      P.progress_def h root middle next.root; let n = at_level middle next.root in let g = Generic in decreases_def g n;
      Structure_spec.linkable_def middle source target; finite_def middle source;
      let s = tree_root source in let t = tree_root target in
      redirect_generic middle s t x next.root next.path (); ()
    | Pre_compress (middle, edits, rest) ->
      Effective_compression_proofs.generic h middle edits x root path ();
      let next = Effective_compression_proofs.resolution h middle edits x root path () in
      Effective_compression_proofs.frame h middle edits root ();
      generic middle p q ok after rest x root next (); ()
    | Scanned (needle, marks, rest) ->
      let middle = scan_heap h marks in Marked_occurs_proofs.scan_at h needle marks x ();
      let frame : ((y : node Pref.t) @ immutable -> {u : unit | lower_frame h middle y}) @ total = fun y ->
        let out = Marked_occurs_proofs.scan_frame h needle marks y () in out in
      P.framed h middle frame x root path (); P.progress_def h root middle root;
      let n = at_level middle root in let g = Generic in decreases_def g n;
      generic middle p q ok after rest x root path (); ()
    | Terminal_lower (bound, edits, tree, rest) ->
      let middle = lower_heap h bound edits in Terminal_lower_spec.completed_def h bound q middle edits tree;
      let frame : ((y : node Pref.t) @ immutable -> {u : unit | lower_frame h middle y}) @ total = fun y ->
        let out = Terminal_lower_proofs.lowering_at h bound edits y () in out in
      P.framed h middle frame x root path (); P.progress_def h root middle root;
      let n = at_level middle root in let g = Generic in decreases_def g n;
      resolves_def h x root path; terminal_def h x; observe_def h x;
      (match H.at h x with Some {desc = Link _; _} -> Terminal_lower_proofs.links_unchanged h bound edits x (); ()
      | _ ->
        let here = Here in resolves_def h x x here;
        R.unique h x root path x here ();
        Terminal_lower_proofs.lower_fixed h bound edits x (); ());
      generic middle p q ok after rest x root path (); ())

let (protected @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (ok : bool) ->
    (after : node Pref.heap) @ immutable -> (d : derivation) @ immutable ->
    (a : E.heads) @ total -> (b : E.heads) @ total -> (depth : int) -> (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q ok after d && E.valid_head h a x && E.valid_head after b x} ->
    {u : unit | Effective_template.protected h a after b depth x} @ ghost =
  fun h p q ok after d a b depth x premise -> ghost_ (
    Effective_unifier_metadata.cells h p q ok after d x ();
    Effective_unifier_metadata.cell_frame_def h after x;
    Effective_unifier_metadata.levels h p q ok after d a b x ();
    if E.effective_below h a x depth then (Effective_unifier_metadata.below h p q ok after d a b x depth (); ()) else ();
    Effective_template.protected_def h a after b depth x;
    Effective_template.generic_def h a x; Effective_template.generic_def after b x;
    let before_level = E.level h a x in let after_level = E.level after b x in decreases_def before_level after_level;
    if H.mem h x && E.level h a x === Generic then (
      E.valid_head_def h a x; E.level_def h a x; let root = a x in
      generic h p q ok after d x root.root root.path (); observe_def h x; observe_def after x; ()) else ();
    ())
