open Copy_spec
open Level_spec
open Level_finite_spec
open Optimized_unifier_spec

let rec (unified_finite_at @ total) : (h : Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h x then finite h t else U.observe h x === None)} @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (ok : bool) ->
    (after : Pref.heap) @ immutable -> (d : derivation) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q ok after d} ->
    {t : tree | tree_root t === x && (if H.mem after x then finite after t else U.observe after x === None)} @ immutable ghost =
  fun h trees p q ok after d x premise -> ghost_ (
    unified_def h p q ok after d; let u = () in match d with
    | Base old -> let t = Level_finite_proofs.unified_finite_at h trees p q ok after old x (u) in t
    | Resolve (r, s, _, _, rest) -> let t = unified_finite_at h trees r s ok after rest x (u) in t
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      let next : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem middle y then finite middle t else U.observe middle y === None)} @ immutable) @ total = fun y ->
        let u = () in let t = unified_finite_at h trees a c left_ok middle left y (u) in t in
      if left_ok then (let t = unified_finite_at middle next b e ok after right x (u) in t)
      else (let t = next x in t)
    | Post_link (middle, rest, source, target) ->
      let old = unified_finite_at h trees p q true middle rest x (u) in
      Structure_spec.linkable_def middle source target; finite_def middle source;
      let s = tree_root source in let t = tree_root target in let v = U.redirect middle s t in
      Level_unifier_proofs.observe_write middle s v x;
      if H.mem middle x then (let t = Structure_finite_proofs.redirect middle source target old (u) in t)
      else old
    | Pre_compress (middle, edits, rest) ->
      let next : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem middle y then finite middle t else U.observe middle y === None)} @ immutable) @ total = fun y ->
        let old = trees y in let u = () in Compression_proofs.frame h middle edits y (u);
        if H.mem h y then (let t = Compression_proofs.finite h middle edits old (u) in t)
        else (
          Level_unifier_metadata.scratch_frame_def h middle y; U.observe_def h y; U.observe_def middle y;
          old) in
      let t = unified_finite_at middle next p q ok after rest x (u) in t)
