open Marked_occurs_proofs
open Copy_spec
open Level_unifier_spec
open Level_unifier_proofs
open Level_spec
open Level_proofs
open Level_unifier_metadata
open Effective_unifier_spec
module E = Effective_level

open Level_finite_spec

let bind_searched :
    (h : (Pref.heap) Ghost.t) @ immutable ->
    (heads : E.heads Ghost.t) @ total ->
    (witness : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (scope : (((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h.Ghost.ghost q) || E.effective_scope h.Ghost.ghost heads.Ghost.ghost q})) Ghost.t) @ total ->
    (order : (((x : node Pref.t) @ immutable -> {u : unit | E.effective_ordered h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (found : bool) ->
    (search : (search) Ghost.t) @ immutable ->
    (t : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p && H.mem h.Ghost.ghost q && active h.Ghost.ghost p && active h.Ghost.ghost q
      && observe h.Ghost.ghost p === Some Var && terminal h.Ghost.ghost q && not (p === q)
      && searched h.Ghost.ghost p q found search.Ghost.ghost}) @ unique ->
    {r : result | unified h.Ghost.ghost p q r.#ok (Pref.own r.#state) r.#derivation} @ unique = fun h heads witness scope order trees p q found search t ->
  if found then
    let old = ghost_ (Occurs_left search.Ghost.ghost) in
    let d = ghost_ (Base old) in
    let ok = false in
    ghost_ (terminal_def h.Ghost.ghost p; Level_unifier_spec.unified_def h.Ghost.ghost p q ok h.Ghost.ghost old; unified_def h.Ghost.ghost p q ok h.Ghost.ghost d);
    let r = #{ok; state = t; derivation = d} in
    (refine_ r : {r : result | unified h.Ghost.ghost p q r.#ok (Pref.own r.#state) r.#derivation})
  else
    let refine_ old = Pref.read p (borrow_ t) in ghost_ (active_def h.Ghost.ghost p; at_level_def h.Ghost.ghost p);
    match old.level with
    | Generic ->
      let _ = ghost_ ((() : {u : unit | false})) in
      assert false
    | Finite bound ->
      ghost_ (witness.Ghost.ghost q; E.terminal_level h.Ghost.ghost heads.Ghost.ghost q ();
        E.effective_active_def h.Ghost.ghost heads.Ghost.ghost q; active_def h.Ghost.ghost q);
      let t : {t : Pref.token | Pref.own t === h.Ghost.ghost && bound >= 0 && E.effective_active h.Ghost.ghost heads.Ghost.ghost q} = refine_ t in
      let refine_ lowered = Effective_lower_runtime.lower h heads witness scope order trees bound q (refine_ t) in
      let levels = ghost_ lowered.#edits in
      let middle = ghost_ (Pref.own (borrow_ lowered.#state)) in
      ghost_ (let flag = false in let tree = lowered.#tree in
        Terminal_lower_spec.completed_def h.Ghost.ghost bound q middle levels tree;
        Effective_bind_proofs.lower_searched h.Ghost.ghost bound levels p q flag search.Ghost.ghost ();
        Effective_unifier_finite.lower_observe h.Ghost.ghost bound levels p ();
        Effective_unifier_finite.lower_observe h.Ghost.ghost bound levels q ();
        Terminal_lower_proofs.lowering_at h.Ghost.ghost bound levels p (); Level_proofs.frame_active h.Ghost.ghost middle p ();
        Terminal_lower_proofs.lowering_at h.Ghost.ghost bound levels q (); Level_proofs.frame_active h.Ghost.ghost middle q ();
        below_def h.Ghost.ghost p bound; Terminal_lower_proofs.lower_fixed h.Ghost.ghost bound levels p (); at_level_def middle p;
        terminal_def h.Ghost.ghost p; terminal_def h.Ghost.ghost q; terminal_def middle p; terminal_def middle q;
        Effective_bind_proofs.bounded_terminal middle bound tree q ());
      let t = lowered.#state in
      let refine_ current = Pref.read p (borrow_ t) in let link = {current with desc = Link q} in ghost_ (redirect_def middle p q);
      let refine_ t = Pref.write p link t in
      let after = ghost_ (Pref.own (borrow_ t)) in
      let step = ghost_ (Bind_left search.Ghost.ghost) in let tree = ghost_ lowered.#tree in
      let base = ghost_ (Base step) in
      let d = ghost_ (Terminal_lower (bound, levels, tree, base)) in
      let ok = true in
      ghost_ (Level_unifier_spec.unified_def middle p q ok after step; unified_def middle p q ok after base; unified_def h.Ghost.ghost p q ok after d);
      let r = #{ok; state = t; derivation = d} in refine_ r

let bind :
    (h : (Pref.heap) Ghost.t) @ immutable ->
    (heads : E.heads Ghost.t) @ total ->
    (witness : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (scope : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h.Ghost.ghost x) || E.effective_scope h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (unmarked : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t) @ total ->
    (order : (((x : node Pref.t) @ immutable -> {u : unit | E.effective_ordered h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (t : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p && H.mem h.Ghost.ghost q
      && active h.Ghost.ghost p && active h.Ghost.ghost q && observe h.Ghost.ghost p === Some Var
      && terminal h.Ghost.ghost q && not (p === q)}) @ unique ->
    {r : result | unified h.Ghost.ghost p q r.#ok (Pref.own r.#state) r.#derivation} @ unique = fun h heads witness scope unmarked order trees p q t ->
    let t : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost q && active h.Ghost.ghost q} = refine_ t in
    let h_witness1 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
    let scope_witness2 : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness1.Ghost.ghost x) || source_ok h_witness1.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (fun x -> scope.Ghost.ghost x; E.effective_scope_def h.Ghost.ghost heads.Ghost.ghost x; ())} in
    let unmarked_witness3 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h_witness1.Ghost.ghost x with
        None -> true | Some v -> not v.visited})) Ghost.t = {Ghost.ghost = ghost_ (refine_ unmarked.Ghost.ghost)} in
    let refine_ state_argument4 = t in
    let refine_ checked = Graph_occurs.occurs h_witness1 scope_witness2 unmarked_witness3 p q (refine_ state_argument4) in
    let marks = ghost_ checked.#marks in let search = ghost_ checked.#search in
    let found = checked.#found in let mid = ghost_ (scan_heap h.Ghost.ghost marks) in
    let scope_mid : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem mid x) || E.effective_scope mid heads.Ghost.ghost x}) @ total ghost = ghost_ (fun x ->
      scope.Ghost.ghost x; let () = Effective_scan_proofs.scope h.Ghost.ghost heads.Ghost.ghost p marks x () in ()) in
    ghost_ (scan_heap_def h.Ghost.ghost marks;
      scan_observe h.Ghost.ghost p marks p (); scan_observe h.Ghost.ghost p marks q ();
      terminal_def h.Ghost.ghost q; terminal_def mid q;
      scan_searched h.Ghost.ghost p marks p q found search ());
    let t = checked.#state in
    let t : {t : Pref.token | Pref.own t === mid && H.mem mid p && H.mem mid q
      && active mid p && active mid q && observe mid p === Some Var
      && terminal mid q && not (p === q) && searched mid p q found search} = refine_ t in
    let order_mid : ((x : node Pref.t) @ immutable -> {u : unit | E.effective_ordered mid heads.Ghost.ghost x}) @ total ghost = ghost_ (fun x ->
      order.Ghost.ghost x; Effective_scan_proofs.order h.Ghost.ghost heads.Ghost.ghost p marks x (); ()) in
    let trees_mid : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem mid x then finite mid t else observe mid x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
        let refine_ tree = trees.Ghost.ghost x in Marked_occurs_proofs.scan_at h.Ghost.ghost p marks x ();
        if H.mem h.Ghost.ghost x then (Level_finite_proofs.scan_finite h.Ghost.ghost p marks tree (); refine_ tree)
        else (observe_def h.Ghost.ghost x; observe_def mid x; refine_ tree)) in
    let h_witness5 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (mid)} in
    let scope_witness6 : (((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness5.Ghost.ghost q) || E.effective_scope h_witness5.Ghost.ghost heads.Ghost.ghost q})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope_mid)} in
    let order_witness7 : (((x : node Pref.t) @ immutable -> {u : unit | E.effective_ordered h_witness5.Ghost.ghost heads.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ order_mid)} in
    let trees_witness8 : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h_witness5.Ghost.ghost x then finite h_witness5.Ghost.ghost t else observe h_witness5.Ghost.ghost x === None)} @ immutable)) Ghost.t = {Ghost.ghost = ghost_ (refine_ trees_mid)} in
    let search_witness9 : (search) Ghost.t = {Ghost.ghost = ghost_ (search)} in
    let refine_ state_argument10 = t in
    let witness_mid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h_witness5.Ghost.ghost heads.Ghost.ghost x})) Ghost.t =
      {Ghost.ghost = ghost_ (fun x -> witness.Ghost.ghost x; Effective_scan_proofs.head h.Ghost.ghost heads.Ghost.ghost p marks x (); ())} in
    let refine_ out = bind_searched h_witness5 heads witness_mid scope_witness6 order_witness7 trees_witness8 p q found search_witness9 (refine_ state_argument10) in
    let derivation = ghost_ (Scanned (p, marks, out.#derivation)) in
    let state = out.#state in let after = ghost_ (Pref.own (borrow_ state)) in
    let ok = out.#ok in ghost_ (unified_def h.Ghost.ghost p q ok after derivation);
    let r = #{ok; state; derivation} in refine_ r

