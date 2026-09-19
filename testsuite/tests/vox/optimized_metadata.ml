open Copy_spec
open Level_spec
open Level_finite_spec
open Optimized_unifier_spec
module M = Level_unifier_metadata
module P = Level_unifier_proofs
module C = Compression_proofs

let rec (unified_frame @ total) : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (ok : bool) -> (after : Pref.heap) @ immutable ->
    (d : derivation) @ immutable -> (x : node Pref.t) @ immutable -> {u : unit | unified h p q ok after d} ->
    {u : unit | H.mem h x === H.mem after x} @ ghost = fun h p q ok after d x premise -> ghost_ (
    unified_def h p q ok after d; match d with
    | Base old -> P.unified_frame h p q ok after old x (); ()
    | Resolve (r, s, _, _, rest) -> unified_frame h r s ok after rest x (); ()
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      unified_frame h a c left_ok middle left x ();
      if left_ok then (unified_frame middle b e ok after right x (); ()) else ()
    | Post_link (middle, rest, source, target) -> unified_frame h p q true middle rest x ();
      Structure_spec.linkable_def middle source target; finite_def middle source;
      let s = tree_root source in let t = tree_root target in let _ = U.redirect middle s t in
      ()
    | Pre_compress (middle, edits, rest) -> C.frame h middle edits x ();
      unified_frame middle p q ok after rest x (); ())

let rec (unified_scratch @ total) : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (ok : bool) -> (after : Pref.heap) @ immutable ->
    (d : derivation) @ immutable -> (x : node Pref.t) @ immutable -> {u : unit | unified h p q ok after d} ->
    {u : unit | M.scratch_frame h after x} @ ghost = fun h p q ok after d x premise -> ghost_ (
    unified_def h p q ok after d; match d with
    | Base old -> M.unified_scratch h p q ok after old x (); ()
    | Resolve (r, s, _, _, rest) -> unified_scratch h r s ok after rest x (); ()
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      unified_scratch h a c left_ok middle left x ();
      if left_ok then (unified_scratch middle b e ok after right x ();
        M.scratch_trans h middle after x (); ()) else ()
    | Post_link (middle, rest, source, target) -> unified_scratch h p q true middle rest x ();
      Structure_spec.linkable_def middle source target;
      let s = tree_root source in let t = tree_root target in M.redirect_scratch middle s t x ();
      M.scratch_trans h middle after x (); ()
    | Pre_compress (middle, edits, rest) -> C.frame h middle edits x ();
      unified_scratch middle p q ok after rest x ();
      M.scratch_trans h middle after x (); ())

let (unified_active @ total) : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (ok : bool) -> (after : Pref.heap) @ immutable ->
    (d : derivation) @ immutable -> (x : node Pref.t) @ immutable -> {u : unit | unified h p q ok after d && active h x} ->
    {u : unit | active after x} @ ghost = fun h p q ok after d x premise -> ghost_ (
    unified_frame h p q ok after d x ();
    unified_scratch h p q ok after d x (); M.scratch_frame_def h after x;
    active_def h x; active_def after x; at_level_def h x; at_level_def after x;
    (match H.at h x, H.at after x with Some a, Some b -> decreases_def a.level b.level; () | _ -> ()); ())

let rec (unified_scope @ total) : (h : Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || finite_scope h x})) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (ok : bool) ->
    (after : Pref.heap) @ immutable -> (d : derivation) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q ok after d} -> {u : unit | not (H.mem after x) || finite_scope after x} @ ghost =
  fun h scope p q ok after d x premise -> ghost_ (
    unified_def h p q ok after d; match d with
    | Base old -> M.unified_scope h scope p q ok after old x (); ()
    | Resolve (r, s, _, _, rest) -> unified_scope h scope r s ok after rest x (); ()
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      let next : ((y : node Pref.t) @ immutable -> {u : unit | not (H.mem middle y) || finite_scope middle y}) @ total = fun y ->
        let () = unified_scope h scope a c left_ok middle left y () in () in
      if left_ok then (unified_scope middle next b e ok after right x (); ())
      else (next x; ())
    | Post_link (middle, rest, source, target) -> unified_scope h scope p q true middle rest x ();
      Structure_spec.linkable_def middle source target;
      let s = tree_root source in let t = tree_root target in M.redirect_scope middle s t x (); ()
    | Pre_compress (middle, edits, rest) ->
      let next : ((y : node Pref.t) @ immutable -> {u : unit | not (H.mem middle y) || finite_scope middle y}) @ total = fun y ->
        let () = C.scope h middle edits scope y () in () in
      unified_scope middle next p q ok after rest x (); ())

let rec (unified_ordered @ total) : (h : Pref.heap) @ immutable ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x})) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (ok : bool) ->
    (after : Pref.heap) @ immutable -> (d : derivation) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q ok after d} -> {u : unit | ordered after x} @ ghost =
  fun h order p q ok after d x premise -> ghost_ (
    unified_def h p q ok after d; match d with
    | Base old -> order x; M.unified_ordered h p q ok after old x (); ()
    | Resolve (r, s, _, _, rest) -> unified_ordered h order r s ok after rest x (); ()
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      let next : ((y : node Pref.t) @ immutable -> {u : unit | ordered middle y}) @ total = fun y ->
        let () = unified_ordered h order a c left_ok middle left y () in () in
      if left_ok then (unified_ordered middle next b e ok after right x (); ())
      else (next x; ())
    | Post_link (middle, rest, source, target) -> unified_ordered h order p q true middle rest x ();
      Structure_spec.linkable_def middle source target;
      let s = tree_root source in let t = tree_root target in M.redirect_ordered middle s t x (); ()
    | Pre_compress (middle, edits, rest) ->
      let next : ((y : node Pref.t) @ immutable -> {u : unit | ordered middle y}) @ total = fun y ->
        let () = C.ordered h middle edits order y () in () in
      unified_ordered middle next p q ok after rest x (); ())
