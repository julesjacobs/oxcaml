open Copy_spec
open Level_spec
open Level_finite_spec
open Level_unifier_spec
let (lower_observe @ total) : (h : node Pref.heap) @ immutable -> (bound : int) ->
    (d : lowering) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | Terminal_lower_spec.terminal_valid h bound d} ->
    {u : unit | observe h x === observe (lower_heap h bound d) x
      && H.mem h x === H.mem (lower_heap h bound d) x} @ ghost = fun h bound d x premise -> ghost_ (
  let after = lower_heap h bound d in
  Terminal_lower_proofs.lowering_at h bound d x (); lower_frame_def h after x;
  observe_def h x; observe_def after x; ())
let rec (lower_finite @ total) : (h : node Pref.heap) @ immutable -> (bound : int) ->
    (edits : lowering) @ immutable -> (t : tree) @ immutable ->
    {u : unit | Terminal_lower_spec.terminal_valid h bound edits && finite h t} ->
    {u : unit | finite (lower_heap h bound edits) t} @ ghost = fun h bound edits t premise -> ghost_ (
  finite_def h t; tree_root_def t;
  let after = lower_heap h bound edits in finite_def after t; let x = tree_root t in
  lower_observe h bound edits x (); match t with
  | Free _ | Constant_tree _ | Word_tree _ -> ()
  | Alias_tree (_, c) | List_tree (_, c) -> lower_finite h bound edits c (); ()
  | Branch (_, a, b) -> lower_finite h bound edits a (); lower_finite h bound edits b (); ())

open Copy_spec
open Level_spec
open Level_finite_spec
open Effective_unifier_spec

let rec (unified_finite_at @ total) : (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h x then finite h t else U.observe h x === None)} @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (ok : bool) ->
    (after : node Pref.heap) @ immutable -> (d : derivation) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q ok after d} ->
    {t : tree | tree_root t === x && (if H.mem after x then finite after t else U.observe after x === None)} @ immutable ghost =
  fun h trees p q ok after d x premise -> ghost_ (
    unified_def h p q ok after d; match d with
    | Swap rest -> let t = unified_finite_at h trees q p ok after rest x () in t
    | Scanned (needle, marks, rest) -> let middle = U.scan_heap h marks in 
      let next : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem middle y then finite middle t else U.observe middle y === None)} @ immutable) @ total = fun y ->
        let old = trees y in Marked_occurs_proofs.scan_observe h needle marks y ();
        if H.mem h y then (Level_finite_proofs.scan_finite h needle marks old (); old) else old in
      let t = unified_finite_at middle next p q ok after rest x () in t
    | Terminal_lower (bound, edits, tree, rest) -> let middle = lower_heap h bound edits in Terminal_lower_spec.completed_def h bound q middle edits tree;
      let next : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem middle y then finite middle t else U.observe middle y === None)} @ immutable) @ total = fun y ->
        let old = trees y in lower_observe h bound edits y ();
        if H.mem h y then (lower_finite h bound edits old (); old) else old in
      let t = unified_finite_at middle next p q ok after rest x () in t
    | Base old -> let t = Level_finite_proofs.unified_finite_at h trees p q ok after old x () in t
    | List_children (r, s, rest) | Resolve (r, s, _, _, rest) -> let t = unified_finite_at h trees r s ok after rest x () in t
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      let next : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem middle y then finite middle t else U.observe middle y === None)} @ immutable) @ total = fun y ->
        let t = unified_finite_at h trees a c left_ok middle left y () in t in
      if left_ok then (let t = unified_finite_at middle next b e ok after right x () in t)
      else (let t = next x in t)
    | Post_link (middle, rest, source, target) ->
      let old = unified_finite_at h trees p q true middle rest x () in
      Structure_spec.linkable_def middle source target; finite_def middle source;
      let s = tree_root source in let t = tree_root target in let v = U.redirect middle s t in
      Level_unifier_proofs.observe_write middle s v x;
      if H.mem middle x then (let t = Structure_finite_proofs.redirect middle source target old () in t)
      else old
    | Pre_compress (middle, edits, rest) ->
      let next : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem middle y then finite middle t else U.observe middle y === None)} @ immutable) @ total = fun y ->
        let old = trees y in Effective_compression_proofs.frame h middle edits y ();
        if H.mem h y then (let t = Effective_compression_proofs.finite h middle edits old () in t)
        else (
           U.observe_def h y; U.observe_def middle y;
          old) in
      let t = unified_finite_at middle next p q ok after rest x () in t)
