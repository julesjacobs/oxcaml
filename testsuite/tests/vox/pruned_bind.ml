open Marked_occurs_proofs
open Copy_spec
open Level_unifier_spec
open Level_unifier_proofs
open Level_spec
open Level_proofs
open Level_unifier_metadata

open Level_finite_spec

let bind_searched :
    (h : (Pref.heap) Ghost.t) @ immutable ->
    (scope : (((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h.Ghost.ghost q) || finite_scope h.Ghost.ghost q})) Ghost.t) @ total ->
    (order : (((x : node Pref.t) @ immutable -> {u : unit | ordered h.Ghost.ghost x})) Ghost.t) @ total ->
    (trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (found : bool) ->
    (search : (search) Ghost.t) @ immutable ->
    (t : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p && H.mem h.Ghost.ghost q && active h.Ghost.ghost p && active h.Ghost.ghost q
      && observe h.Ghost.ghost p === Some Var && terminal h.Ghost.ghost q && not (p === q)
      && searched h.Ghost.ghost p q found search.Ghost.ghost}) @ unique ->
    {r : result | unified h.Ghost.ghost p q r.#ok (Pref.own r.#state) r.#derivation} @ unique = fun h scope order trees p q found search t ->
  if found then
    let d = ghost_ (Occurs_left search.Ghost.ghost) in
    let ok = false in
    ghost_ (unified_def h.Ghost.ghost p q ok h.Ghost.ghost d);
    let r = #{ok; state = t; derivation = d} in
    (r : {r : result | unified h.Ghost.ghost p q r.#ok (Pref.own r.#state) r.#derivation})
  else
    let t : {t : Pref.token | H.mem (Pref.own t) p} = t in
    let old = Pref.read p (borrow_ t) in ghost_ (active_def h.Ghost.ghost p; at_level_def h.Ghost.ghost p);
    match old.level with
    | Generic ->
      let _proof = ghost_ (let u = () in (u : {u : unit | false})) in
      assert false
    | Finite bound ->
      let t : {t : Pref.token | Pref.own t === h.Ghost.ghost && bound >= 0 && active h.Ghost.ghost q} = t in
      let lower_heap_witness : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
      let lower_scope_witness : (((x : node Pref.t) @ immutable ->
        {u : unit | not (H.mem lower_heap_witness.Ghost.ghost x) || Level_spec.finite_scope lower_heap_witness.Ghost.ghost x})) Ghost.t =
        {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
      let lower_order_witness : (((x : node Pref.t) @ immutable ->
        {u : unit | Level_spec.ordered lower_heap_witness.Ghost.ghost x})) Ghost.t =
        {Ghost.ghost = ghost_ (refine_ order.Ghost.ghost)} in
      let lower_trees_witness : (((x : node Pref.t) @ immutable ->
        {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
          (if H.mem lower_heap_witness.Ghost.ghost x then Level_finite_spec.finite lower_heap_witness.Ghost.ghost t else Level_unifier_spec.observe lower_heap_witness.Ghost.ghost x === None)} @ immutable)) Ghost.t =
        {Ghost.ghost = ghost_ (refine_ trees.Ghost.ghost)} in
      let lowered = Pruned_lower.lower lower_heap_witness lower_scope_witness lower_order_witness lower_trees_witness bound q (t) in
      let levels = ghost_ lowered.#edits in
      let middle = ghost_ (Pref.own (borrow_ lowered.#state)) in
      ghost_ (let u = () in let flag = false in
        lower_searched h.Ghost.ghost bound levels p q flag search.Ghost.ghost (u);
        lower_observe h.Ghost.ghost bound levels p (u); lower_observe h.Ghost.ghost bound levels q (u);
        lowering_at h.Ghost.ghost bound levels p (u); frame_active h.Ghost.ghost middle p (u);
        lowering_at h.Ghost.ghost bound levels q (u); frame_active h.Ghost.ghost middle q (u);
        below_def h.Ghost.ghost p bound; lower_fixed h.Ghost.ghost bound levels p (u); at_level_def middle p;
        let tree = lowered.#tree in lower_bounded_at middle bound tree q (u));
      let t = lowered.#state in
      let t : {t : Pref.token | H.mem (Pref.own t) p} = t in
      let current = Pref.read p (borrow_ t) in let link = {current with desc = Link q} in ghost_ (redirect_def middle p q);
      let t : {t : Pref.token | H.mem (Pref.own t) p} = t in
      let t = Pref.write p link t in
      let after = ghost_ (Pref.own (borrow_ t)) in
      let step = ghost_ (Bind_left search.Ghost.ghost) in let tree = ghost_ lowered.#tree in
      let d = ghost_ (Lowering (bound, levels, tree, step)) in
      let ok = true in
      ghost_ (unified_def middle p q ok after step; unified_def h.Ghost.ghost p q ok after d);
      let r = #{ok; state = t; derivation = d} in r

let bind :
    (h : (Pref.heap) Ghost.t) @ immutable ->
    (scope : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h.Ghost.ghost x) || finite_scope h.Ghost.ghost x})) Ghost.t) @ total ->
    (unmarked : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t) @ total ->
    (order : (((x : node Pref.t) @ immutable -> {u : unit | ordered h.Ghost.ghost x})) Ghost.t) @ total ->
    (trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (t : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p && H.mem h.Ghost.ghost q
      && active h.Ghost.ghost p && active h.Ghost.ghost q && observe h.Ghost.ghost p === Some Var
      && terminal h.Ghost.ghost q && not (p === q)}) @ unique ->
    {r : result | unified h.Ghost.ghost p q r.#ok (Pref.own r.#state) r.#derivation} @ unique = fun h scope unmarked order trees p q t ->
    let t : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost q && active h.Ghost.ghost q} = t in
    let h_witness1 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
    let scope_witness2 : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness1.Ghost.ghost x) || finite_scope h_witness1.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
    let unmarked_witness3 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h_witness1.Ghost.ghost x with
        None -> true | Some v -> not v.visited})) Ghost.t = {Ghost.ghost = ghost_ (refine_ unmarked.Ghost.ghost)} in
    let state_argument4 = t in
    let checked = Marked_occurs.occurs h_witness1 scope_witness2 unmarked_witness3 p q (state_argument4) in
    let marks = ghost_ checked.#marks in let search = ghost_ checked.#search in
    let found = checked.#found in let mid = ghost_ (scan_heap h.Ghost.ghost marks) in
    let scope_mid : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem mid x) || finite_scope mid x}) @ total ghost = ghost_ (fun x ->
      let u = () in let u = scan_scope h.Ghost.ghost scope.Ghost.ghost p marks x (u) in u) in
    ghost_ (let u = () in scan_heap_def h.Ghost.ghost marks;
      scan_observe h.Ghost.ghost p marks p (u); scan_observe h.Ghost.ghost p marks q (u);
      terminal_def h.Ghost.ghost q; terminal_def mid q;
      scan_searched h.Ghost.ghost p marks p q found search (u));
    let t = checked.#state in
    let t : {t : Pref.token | Pref.own t === mid && H.mem mid p && H.mem mid q
      && active mid p && active mid q && observe mid p === Some Var
      && terminal mid q && not (p === q) && searched mid p q found search} = t in
    let order_mid : ((x : node Pref.t) @ immutable -> {u : unit | ordered mid x}) @ total ghost = ghost_ (fun x ->
      order.Ghost.ghost x; let u = () in Marked_occurs_proofs.scan_ordered h.Ghost.ghost p marks x (u); u) in
    let trees_mid : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem mid x then finite mid t else observe mid x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
        let tree = trees.Ghost.ghost x in let u = () in Marked_occurs_proofs.scan_at h.Ghost.ghost p marks x (u);
        if H.mem h.Ghost.ghost x then (Level_finite_proofs.scan_finite h.Ghost.ghost p marks tree (u); tree)
        else (observe_def h.Ghost.ghost x; observe_def mid x; tree)) in
    let h_witness5 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (mid)} in
    let scope_witness6 : (((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness5.Ghost.ghost q) || finite_scope h_witness5.Ghost.ghost q})) Ghost.t = {Ghost.ghost = ghost_ (scope_mid)} in
    let order_witness7 : (((x : node Pref.t) @ immutable -> {u : unit | ordered h_witness5.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (order_mid)} in
    let trees_witness8 : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h_witness5.Ghost.ghost x then finite h_witness5.Ghost.ghost t else observe h_witness5.Ghost.ghost x === None)} @ immutable)) Ghost.t = {Ghost.ghost = ghost_ (trees_mid)} in
    let search_witness9 : (search) Ghost.t = {Ghost.ghost = ghost_ (search)} in
    let state_argument10 = t in
    let out = bind_searched h_witness5 scope_witness6 order_witness7 trees_witness8 p q found search_witness9 (state_argument10) in
    let derivation = ghost_ (Scanned (p, marks, out.#derivation)) in
    let state = out.#state in let after = ghost_ (Pref.own (borrow_ state)) in
    let ok = out.#ok in ghost_ (unified_def h.Ghost.ghost p q ok after derivation);
    let r = #{ok; state; derivation} in r

