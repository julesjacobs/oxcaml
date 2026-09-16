open Marked_occurs_proofs
open Copy_spec
open Level_unifier_spec
open Level_unifier_proofs
open Level_spec
open Level_proofs
open Level_unifier_metadata

open Level_finite_spec

let bind_searched :
    (h : Pref.heap) @ immutable ghost ->
    (scope : ((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h q) || finite_scope h q})) @ total ghost ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x})) @ total ghost ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ghost ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (found : bool) -> (search : search) @ immutable ghost ->
    (t : {t : Pref.token | Pref.own t === h && H.mem h p && H.mem h q && active h p && active h q
      && observe h p === Some Var && terminal h q && not (p === q)
      && searched h p q found search}) @ unique ->
    {r : result | unified h p q r.#ok (Pref.own r.#state) r.#derivation} @ unique =
  fun h scope order trees p q found search t ->
  let refine_ t = t in
  if found then
    let d = ghost_ (Occurs_left search) in
    let ok = false in
    ghost_ (unified_def h p q ok h d);
    let r = #{ok; state = t; derivation = d} in
    (refine_ r : {r : result | unified h p q r.#ok (Pref.own r.#state) r.#derivation})
  else
    let t : {t : Pref.token | H.mem (Pref.own t) p} = refine_ t in
    let refine_ old = Pref.read p (borrow_ t) in let refine_ t = t in
    ghost_ (active_def h p; at_level_def h p);
    match old.level with
    | Generic ->
      let proof = ghost_ (let u = () in (refine_ u : {u : unit | false})) in
      let refine_ proof = proof in assert false
    | Finite bound ->
      let t : {t : Pref.token | Pref.own t === h && bound >= 0 && active h q} = refine_ t in
      let lower_heap_witness : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (h)} in
      let lower_scope_witness : (((x : node Pref.t) @ immutable ->
        {u : unit | not (H.mem lower_heap_witness.Ghost.ghost x) || Level_spec.finite_scope lower_heap_witness.Ghost.ghost x})) Ghost.t =
        {Ghost.ghost = ghost_ (refine_ scope)} in
      let lower_order_witness : (((x : node Pref.t) @ immutable ->
        {u : unit | Level_spec.ordered lower_heap_witness.Ghost.ghost x})) Ghost.t =
        {Ghost.ghost = ghost_ (refine_ order)} in
      let lower_trees_witness : (((x : node Pref.t) @ immutable ->
        {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
          (if H.mem lower_heap_witness.Ghost.ghost x then Level_finite_spec.finite lower_heap_witness.Ghost.ghost t else Level_unifier_spec.observe lower_heap_witness.Ghost.ghost x === None)} @ immutable)) Ghost.t =
        {Ghost.ghost = ghost_ (refine_ trees)} in
      let refine_ t = t in
      let refine_ lowered = Pruned_lower.lower lower_heap_witness lower_scope_witness lower_order_witness lower_trees_witness bound q (refine_ t) in
      let levels = ghost_ lowered.#edits in
      let middle = ghost_ (Pref.own (borrow_ lowered.#state)) in
      ghost_ (let u = () in let flag = false in
        lower_searched h bound levels p q flag search (refine_ u);
        lower_observe h bound levels p (refine_ u); lower_observe h bound levels q (refine_ u);
        lowering_at h bound levels p (refine_ u); frame_active h middle p (refine_ u);
        lowering_at h bound levels q (refine_ u); frame_active h middle q (refine_ u);
        below_def h p bound; lower_fixed h bound levels p (refine_ u); at_level_def middle p;
        let tree = lowered.#tree in lower_bounded_at middle bound tree q (refine_ u));
      let t = lowered.#state in
      let t : {t : Pref.token | H.mem (Pref.own t) p} = refine_ t in
      let refine_ current = Pref.read p (borrow_ t) in let refine_ t = t in
      let link = {current with desc = Link q} in ghost_ (redirect_def middle p q);
      let t : {t : Pref.token | H.mem (Pref.own t) p} = refine_ t in
      let refine_ t = Pref.write p link t in
      let after = ghost_ (Pref.own (borrow_ t)) in
      let step = ghost_ (Bind_left search) in let tree = ghost_ lowered.#tree in
      let d = ghost_ (Lowering (bound, levels, tree, step)) in
      let ok = true in
      ghost_ (unified_def middle p q ok after step; unified_def h p q ok after d);
      let r = #{ok; state = t; derivation = d} in refine_ r

let bind :
    (h : Pref.heap) @ immutable ghost ->
    (scope : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h x) || finite_scope h x})) @ total ghost ->
    (unmarked : ((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h x with None -> true | Some v -> not v.visited})) @ total ghost ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x})) @ total ghost ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ghost ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (t : {t : Pref.token | Pref.own t === h && H.mem h p && H.mem h q
      && active h p && active h q && observe h p === Some Var
      && terminal h q && not (p === q)}) @ unique ->
    {r : result | unified h p q r.#ok (Pref.own r.#state) r.#derivation} @ unique =
  fun h scope unmarked order trees p q t ->
    let refine_ t = t in
    let t : {t : Pref.token | Pref.own t === h && H.mem h q && active h q} = refine_ t in
    let refine_ checked = Marked_occurs.occurs h scope unmarked p q t in
    let marks = ghost_ checked.#marks in let search = ghost_ checked.#search in
    let found = checked.#found in let mid = ghost_ (scan_heap h marks) in
    let scope_mid : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem mid x) || finite_scope mid x}) @ total ghost = ghost_ (fun x ->
      let u = () in let refine_ u = scan_scope h scope p marks x (refine_ u) in refine_ u) in
    ghost_ (let u = () in scan_heap_def h marks;
      scan_observe h p marks p (refine_ u); scan_observe h p marks q (refine_ u);
      terminal_def h q; terminal_def mid q;
      scan_searched h p marks p q found search (refine_ u));
    let t = checked.#state in
    let t : {t : Pref.token | Pref.own t === mid && H.mem mid p && H.mem mid q
      && active mid p && active mid q && observe mid p === Some Var
      && terminal mid q && not (p === q) && searched mid p q found search} = refine_ t in
    let order_mid : ((x : node Pref.t) @ immutable -> {u : unit | ordered mid x}) @ total ghost = ghost_ (fun x ->
      order x; let u = () in Marked_occurs_proofs.scan_ordered h p marks x (refine_ u); refine_ u) in
    let trees_mid : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem mid x then finite mid t else observe mid x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
        let refine_ tree = trees x in let u = () in Marked_occurs_proofs.scan_at h p marks x (refine_ u);
        if H.mem h x then (Level_finite_proofs.scan_finite h p marks tree (refine_ u); refine_ tree)
        else (observe_def h x; observe_def mid x; refine_ tree)) in
    let refine_ out = bind_searched mid scope_mid order_mid trees_mid p q found search t in
    let derivation = ghost_ (Scanned (p, marks, out.#derivation)) in
    let state = out.#state in let after = ghost_ (Pref.own (borrow_ state)) in
    let ok = out.#ok in ghost_ (unified_def h p q ok after derivation);
    let r = #{ok; state; derivation} in refine_ r

