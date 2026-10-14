open Copy_spec
open Level_spec
open Level_finite_spec
open Optimized_unifier_spec

let rec (unified_finite_at @ total) : (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h x then finite h t else U.observe h x === None)} @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (ok : bool) ->
    (after : node Pref.heap) @ immutable -> (d : derivation) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q ok after d} ->
    {t : tree | tree_root t === x && (if H.mem after x then finite after t else U.observe after x === None)} @ immutable ghost =
  fun h trees p q ok after d x premise -> ghost_ (
    let refine_ premise = premise in unified_def h p q ok after d; match d with
    | Base old -> Level_finite_proofs.unified_finite_at h trees p q ok after old x ()
    | Resolve (r, s, _, _, rest) | List_children (r, s, rest) -> unified_finite_at h trees r s ok after rest x ()
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      let next : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem middle y then finite middle t else U.observe middle y === None)} @ immutable) @ total = fun y ->
        unified_finite_at h trees a c left_ok middle left y () in
      if left_ok then (unified_finite_at middle next b e ok after right x ())
      else (next x)
    | Post_link (middle, rest, source, target) ->
      let refine_ old = unified_finite_at h trees p q true middle rest x () in
      Structure_spec.linkable_def middle source target; finite_def middle source;
      let s = tree_root source in let t = tree_root target in let v = U.redirect middle s t in
      Level_unifier_proofs.observe_write middle s v x;
      if H.mem middle x then (Structure_finite_proofs.redirect middle source target old ())
      else old
    | Pre_compress (middle, edits, rest) ->
      let next : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem middle y then finite middle t else U.observe middle y === None)} @ immutable) @ total = fun y ->
        let refine_ old = trees y in Compression_proofs.frame h middle edits y ();
        if H.mem h y then (Compression_proofs.finite h middle edits old ())
        else (
          Level_unifier_metadata.scratch_frame_def h middle y; U.observe_def h y; U.observe_def middle y;
          old) in
      unified_finite_at middle next p q ok after rest x ())
