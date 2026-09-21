open Copy_spec
open Level_spec
open Level_proofs
open Lower_locality_spec
open Lower_locality_proofs
open Level_finite_spec
open Level_lower
open Effective_lower_spec
open Effective_lower_proofs
open Effective_lower_write
module E = Effective_level
module U = Level_unifier_spec

type lower_goal = { heap : Pref.heap @@ ghost; bound : int @@ ghost;
  root : node Pref.t @@ ghost }

let rec run_lower : (goal : lower_goal) @ immutable -> (h : Pref.heap Ghost.t) @ immutable -> (heads : E.heads Ghost.t) @ total ->
    (witness : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h.Ghost.ghost x) || E.effective_scope h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (order : (((x : node Pref.t) @ immutable -> {u : unit | E.effective_ordered h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else Level_unifier_spec.observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total ->
    (bound : int) -> (p : node Pref.t) @ immutable ->
    (t : {t : Pref.token | Pref.own t === h.Ghost.ghost && bound >= 0 && E.effective_active h.Ghost.ghost heads.Ghost.ghost p}) @ unique ->
    (use : ((r : {r : lowered | effective_lower_valid h.Ghost.ghost heads.Ghost.ghost bound r.#edits
      && Pref.own r.#state === lower_heap h.Ghost.ghost bound r.#edits
      && bound_root r.#tree === p && effective_bounded (Pref.own r.#state) heads.Ghost.ghost bound r.#tree
      && confined r.#edits r.#tree}) @ unique -> {r : lowered | effective_lower_valid goal.heap heads.Ghost.ghost goal.bound r.#edits
      && Pref.own r.#state === lower_heap goal.heap goal.bound r.#edits
      && bound_root r.#tree === goal.root && effective_bounded (Pref.own r.#state) heads.Ghost.ghost goal.bound r.#tree
      && confined r.#edits r.#tree} @ unique)) ->
    {r : lowered | effective_lower_valid goal.heap heads.Ghost.ghost goal.bound r.#edits
      && Pref.own r.#state === lower_heap goal.heap goal.bound r.#edits
      && bound_root r.#tree === goal.root && effective_bounded (Pref.own r.#state) heads.Ghost.ghost goal.bound r.#tree
      && confined r.#edits r.#tree} @ unique =
  fun goal h heads witness scope order trees bound p t use ->
    let scope = ghost_ scope.Ghost.ghost in
    let order = ghost_ order.Ghost.ghost in
    let trees = ghost_ trees.Ghost.ghost in
    ghost_ (E.effective_active_def h.Ghost.ghost heads.Ghost.ghost p; scope p; E.effective_scope_def h.Ghost.ghost heads.Ghost.ghost p; source_ok_def h.Ghost.ghost p; U.observe_def h.Ghost.ghost p);
    let old = Pref.read p (borrow_ t) in let t : {t : Pref.token | Pref.own t === h.Ghost.ghost && bound >= 0 && E.effective_active h.Ghost.ghost heads.Ghost.ghost p} = t in
    match old.desc with
    | Link q ->
      let t : {t : Pref.token | Pref.own t === h.Ghost.ghost && bound >= 0 && E.effective_active h.Ghost.ghost heads.Ghost.ghost q} = t in
      let lower_heap_witness : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
      let lower_scope_witness : (((x : node Pref.t) @ immutable ->
        {u : unit | not (H.mem lower_heap_witness.Ghost.ghost x) || E.effective_scope lower_heap_witness.Ghost.ghost heads.Ghost.ghost x})) Ghost.t =
        {Ghost.ghost = ghost_ (scope)} in
      let lower_order_witness : (((x : node Pref.t) @ immutable ->
        {u : unit | E.effective_ordered lower_heap_witness.Ghost.ghost heads.Ghost.ghost x})) Ghost.t =
        {Ghost.ghost = ghost_ (order)} in
      let lower_trees_witness : (((x : node Pref.t) @ immutable ->
        {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
          (if H.mem lower_heap_witness.Ghost.ghost x then Level_finite_spec.finite lower_heap_witness.Ghost.ghost t else Level_unifier_spec.observe lower_heap_witness.Ghost.ghost x === None)} @ immutable)) Ghost.t =
        {Ghost.ghost = ghost_ (trees)} in
      let resume_child : (child : {r : lowered | effective_lower_valid lower_heap_witness.Ghost.ghost heads.Ghost.ghost bound r.#edits
      && Pref.own r.#state === lower_heap lower_heap_witness.Ghost.ghost bound r.#edits
      && bound_root r.#tree === q && effective_bounded (Pref.own r.#state) heads.Ghost.ghost bound r.#tree
      && confined r.#edits r.#tree}) @ unique ->
        {r : lowered | effective_lower_valid goal.heap heads.Ghost.ghost goal.bound r.#edits
      && Pref.own r.#state === lower_heap goal.heap goal.bound r.#edits
      && bound_root r.#tree === goal.root && effective_bounded (Pref.own r.#state) heads.Ghost.ghost goal.bound r.#tree
      && confined r.#edits r.#tree} @ unique = fun child ->
        let d = ghost_ child.#edits in let tree_child = ghost_ child.#tree in
      let mid = ghost_ (Pref.own (borrow_ child.#state)) in
      let tree = ghost_ (Through (p, tree_child)) in
      ghost_ (lowering_at h.Ghost.ghost heads.Ghost.ghost bound d p ();
        lower_bounded_at mid heads.Ghost.ghost bound tree_child q ();
        lower_frame_def h.Ghost.ghost mid p;
        witness.Ghost.ghost p; witness.Ghost.ghost q;
        lower_head h.Ghost.ghost heads.Ghost.ghost bound d p ();
        lower_head h.Ghost.ghost heads.Ghost.ghost bound d q ();
        U.observe_def mid p; E.effective_below_def mid heads.Ghost.ghost q bound;
        E.link_level mid heads.Ghost.ghost p q ();
        E.effective_below_def mid heads.Ghost.ghost p bound;
        bound_root_def tree; effective_bounded_def mid heads.Ghost.ghost bound tree;
        confined_through d p tree_child ());
      let r = #{state = child.#state; edits = d; tree} in use (r) in
      let lower_witness : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head lower_heap_witness.Ghost.ghost heads.Ghost.ghost x})) Ghost.t =
        {Ghost.ghost = ghost_ (refine_ witness.Ghost.ghost)} in
      run_lower goal lower_heap_witness heads lower_witness lower_scope_witness lower_order_witness lower_trees_witness bound q (t) resume_child
    | Var | Bool | Arrow _ ->
      ghost_ (witness.Ghost.ghost p; U.terminal_def h.Ghost.ghost p; U.observe_def h.Ghost.ghost p; E.terminal_level h.Ghost.ghost heads.Ghost.ghost p ());
    if (match old.level with Generic -> false | Finite n -> n <= bound) then (
      let tree : {b : bounded | bound_root b === p && effective_bounded h.Ghost.ghost heads.Ghost.ghost bound b} @ immutable ghost = ghost_ (
        let ft = trees p in
        E.effective_below_def h.Ghost.ghost heads.Ghost.ghost p bound; at_level_def h.Ghost.ghost p;
        let b = Effective_lower_tree.bounded_tree h.Ghost.ghost heads.Ghost.ghost witness.Ghost.ghost order bound ft () in b) in
      let edits = ghost_ Keep in
      ghost_ (effective_lower_valid_def h.Ghost.ghost heads.Ghost.ghost bound edits; lower_heap_def h.Ghost.ghost bound edits; confined_def edits tree);
      let r = #{state = t; edits; tree} in use (r)
    ) else match old.desc with
    | Var | Bool ->
      ghost_ (effective_children_below_def h.Ghost.ghost heads.Ghost.ghost old.desc bound; witness.Ghost.ghost p; U.terminal_def h.Ghost.ghost p; U.observe_def h.Ghost.ghost p);
      let t : {t : Pref.token | Pref.own t === h.Ghost.ghost && bound >= 0 && E.effective_active h.Ghost.ghost heads.Ghost.ghost p
        && match H.at h.Ghost.ghost p with None -> false | Some v -> effective_children_below h.Ghost.ghost heads.Ghost.ghost v.desc bound} = t in
      let write_heap : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
      let r = write_level write_heap heads bound p (t) in
      let tree = ghost_ (Tip p) in let edits = ghost_ r.#edits in
      let after = ghost_ (Pref.own (borrow_ r.#state)) in
      ghost_ (lowering_at h.Ghost.ghost heads.Ghost.ghost bound edits p ();
        lower_frame_def h.Ghost.ghost after p; bound_root_def tree; effective_bounded_def after heads.Ghost.ghost bound tree);
      let r = #{state = r.#state; edits; tree} in use (r)
    | Link _ -> assert false
    | Arrow (a, b) ->
      let t : {t : Pref.token | Pref.own t === h.Ghost.ghost && bound >= 0 && E.effective_active h.Ghost.ghost heads.Ghost.ghost a} = t in
      let lower_heap_witness : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
      let lower_scope_witness : (((x : node Pref.t) @ immutable ->
        {u : unit | not (H.mem lower_heap_witness.Ghost.ghost x) || E.effective_scope lower_heap_witness.Ghost.ghost heads.Ghost.ghost x})) Ghost.t =
        {Ghost.ghost = ghost_ (scope)} in
      let lower_order_witness : (((x : node Pref.t) @ immutable ->
        {u : unit | E.effective_ordered lower_heap_witness.Ghost.ghost heads.Ghost.ghost x})) Ghost.t =
        {Ghost.ghost = ghost_ (order)} in
      let lower_trees_witness : (((x : node Pref.t) @ immutable ->
        {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
          (if H.mem lower_heap_witness.Ghost.ghost x then Level_finite_spec.finite lower_heap_witness.Ghost.ghost t else Level_unifier_spec.observe lower_heap_witness.Ghost.ghost x === None)} @ immutable)) Ghost.t =
        {Ghost.ghost = ghost_ (trees)} in
      let resume_left : (left : {r : lowered | effective_lower_valid lower_heap_witness.Ghost.ghost heads.Ghost.ghost bound r.#edits
      && Pref.own r.#state === lower_heap lower_heap_witness.Ghost.ghost bound r.#edits
      && bound_root r.#tree === a && effective_bounded (Pref.own r.#state) heads.Ghost.ghost bound r.#tree
      && confined r.#edits r.#tree}) @ unique ->
        {r : lowered | effective_lower_valid goal.heap heads.Ghost.ghost goal.bound r.#edits
      && Pref.own r.#state === lower_heap goal.heap goal.bound r.#edits
      && bound_root r.#tree === goal.root && effective_bounded (Pref.own r.#state) heads.Ghost.ghost goal.bound r.#tree
      && confined r.#edits r.#tree} @ unique = fun left ->
        let d1 = ghost_ left.#edits in let ta = ghost_ left.#tree in
      let h1 = ghost_ (Pref.own (borrow_ left.#state)) in
      let scope1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || E.effective_scope h1 heads.Ghost.ghost x}) @ total ghost = ghost_ (fun x ->
        let () = Effective_lower_proofs.lower_scope h.Ghost.ghost heads.Ghost.ghost scope bound d1 x () in ()) in
      ghost_ (lowering_at h.Ghost.ghost heads.Ghost.ghost bound d1 b (); lower_active h.Ghost.ghost heads.Ghost.ghost bound d1 b ());
      let t = left.#state in let t : {t : Pref.token | Pref.own t === h1 && bound >= 0 && E.effective_active h1 heads.Ghost.ghost b} = t in
      let order1 : ((x : node Pref.t) @ immutable -> {u : unit | E.effective_ordered h1 heads.Ghost.ghost x}) @ total ghost = ghost_ (fun x ->
        order x; let () = lowering_ordered h.Ghost.ghost heads.Ghost.ghost bound d1 x () in ()) in
      let trees1 : ((x : node Pref.t) @ immutable ->
        {t : tree | tree_root t === x && (if H.mem h1 x then finite h1 t else Level_unifier_spec.observe h1 x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
          let ft = trees x in lowering_at h.Ghost.ghost heads.Ghost.ghost bound d1 x (); lower_frame_def h.Ghost.ghost h1 x;
          if H.mem h.Ghost.ghost x then (Effective_lower_proofs.lower_finite h.Ghost.ghost heads.Ghost.ghost bound d1 ft (); ft)
          else (Level_unifier_spec.observe_def h.Ghost.ghost x; Level_unifier_spec.observe_def h1 x; ft)) in
      let witness1 : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h1 heads.Ghost.ghost x}) @ total ghost = ghost_ (fun x ->
        witness.Ghost.ghost x; lower_head h.Ghost.ghost heads.Ghost.ghost bound d1 x (); ()) in
      let lower_heap_witness : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (h1)} in
      let lower_scope_witness : (((x : node Pref.t) @ immutable ->
        {u : unit | not (H.mem lower_heap_witness.Ghost.ghost x) || E.effective_scope lower_heap_witness.Ghost.ghost heads.Ghost.ghost x})) Ghost.t =
        {Ghost.ghost = ghost_ (scope1)} in
      let lower_order_witness : (((x : node Pref.t) @ immutable ->
        {u : unit | E.effective_ordered lower_heap_witness.Ghost.ghost heads.Ghost.ghost x})) Ghost.t =
        {Ghost.ghost = ghost_ (order1)} in
      let lower_trees_witness : (((x : node Pref.t) @ immutable ->
        {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
          (if H.mem lower_heap_witness.Ghost.ghost x then Level_finite_spec.finite lower_heap_witness.Ghost.ghost t else Level_unifier_spec.observe lower_heap_witness.Ghost.ghost x === None)} @ immutable)) Ghost.t =
        {Ghost.ghost = ghost_ (trees1)} in
      let resume_right : (right : {r : lowered | effective_lower_valid lower_heap_witness.Ghost.ghost heads.Ghost.ghost bound r.#edits
      && Pref.own r.#state === lower_heap lower_heap_witness.Ghost.ghost bound r.#edits
      && bound_root r.#tree === b && effective_bounded (Pref.own r.#state) heads.Ghost.ghost bound r.#tree
      && confined r.#edits r.#tree}) @ unique ->
        {r : lowered | effective_lower_valid goal.heap heads.Ghost.ghost goal.bound r.#edits
      && Pref.own r.#state === lower_heap goal.heap goal.bound r.#edits
      && bound_root r.#tree === goal.root && effective_bounded (Pref.own r.#state) heads.Ghost.ghost goal.bound r.#tree
      && confined r.#edits r.#tree} @ unique = fun right ->
        let d2 = ghost_ right.#edits in let tb = ghost_ right.#tree in
      let h2 = ghost_ (Pref.own (borrow_ right.#state)) in
      ghost_ (lowering_at h.Ghost.ghost heads.Ghost.ghost bound d1 p (); lower_active h.Ghost.ghost heads.Ghost.ghost bound d1 p ();
        lowering_at h1 heads.Ghost.ghost bound d2 p (); lower_active h1 heads.Ghost.ghost bound d2 p ());
      ghost_ (lower_bounded_at h1 heads.Ghost.ghost bound ta a ();
        lowering_at h1 heads.Ghost.ghost bound d2 a (); lower_below h1 heads.Ghost.ghost bound d2 a bound ();
        lower_bounded_at h2 heads.Ghost.ghost bound tb b ();
        lower_frame_def h.Ghost.ghost h1 p; lower_frame_def h1 h2 p; effective_children_below_def h2 heads.Ghost.ghost old.desc bound);
      ghost_ (witness.Ghost.ghost p;
        lower_head h.Ghost.ghost heads.Ghost.ghost bound d1 p ();
        lower_head h1 heads.Ghost.ghost bound d2 p ();
        U.terminal_def h2 p; U.observe_def h2 p);
      let t = right.#state in let t : {t : Pref.token | Pref.own t === h2 && bound >= 0 && E.effective_active h2 heads.Ghost.ghost p
        && match H.at h2 p with None -> false | Some v -> effective_children_below h2 heads.Ghost.ghost v.desc bound} = t in
      let write_heap : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (h2)} in
      let result = write_level write_heap heads bound p (t) in
      let w = ghost_ result.#edits in let after = ghost_ (Pref.own (borrow_ result.#state)) in
      let suffix = ghost_ (Sequence (d2, w)) in let edits = ghost_ (Sequence (d1, suffix)) in
      let frame2 : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame h2 after x}) @ total ghost = ghost_ (fun x ->
        let () = lowering_at h2 heads.Ghost.ghost bound w x () in ()) in
      let frame1 : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame h1 after x}) @ total ghost = ghost_ (fun x ->
        lowering_at h1 heads.Ghost.ghost bound d2 x (); frame2 x;
        let () = frame_trans h1 h2 after x () in ()) in
      let tree = ghost_ (Fork (p, ta, tb)) in
      ghost_ (bounded_frame h1 after heads.Ghost.ghost frame1 bound ta (); bounded_frame h2 after heads.Ghost.ghost frame2 bound tb ();
        lowering_at h.Ghost.ghost heads.Ghost.ghost bound d1 p (); frame1 p; lower_frame_def h.Ghost.ghost h1 p; lower_frame_def h1 after p;
        lower_heap_def h1 bound suffix; effective_lower_valid_def h1 heads.Ghost.ghost bound suffix;
        lower_heap_def h.Ghost.ghost bound edits; effective_lower_valid_def h.Ghost.ghost heads.Ghost.ghost bound edits;
        bound_root_def tree; effective_bounded_def after heads.Ghost.ghost bound tree;
        confined_fork d1 d2 p ta tb ();
        confined_root w tree p ();
        confined_def suffix tree; confined_def edits tree);
      let r = #{state = result.#state; edits; tree} in use (r)
 in
      let lower_witness : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head lower_heap_witness.Ghost.ghost heads.Ghost.ghost x})) Ghost.t =
        {Ghost.ghost = ghost_ (witness1)} in
      run_lower goal lower_heap_witness heads lower_witness lower_scope_witness lower_order_witness lower_trees_witness bound b (t) resume_right in
      let lower_witness : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head lower_heap_witness.Ghost.ghost heads.Ghost.ghost x})) Ghost.t =
        {Ghost.ghost = ghost_ (refine_ witness.Ghost.ghost)} in
      run_lower goal lower_heap_witness heads lower_witness lower_scope_witness lower_order_witness lower_trees_witness bound a (t) resume_left

let lower : (h : Pref.heap Ghost.t) @ immutable -> (heads : E.heads Ghost.t) @ total ->
    (witness : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h.Ghost.ghost x) || E.effective_scope h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (order : (((x : node Pref.t) @ immutable -> {u : unit | E.effective_ordered h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else Level_unifier_spec.observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total ->
    (bound : int) -> (p : node Pref.t) @ immutable ->
    (t : {t : Pref.token | Pref.own t === h.Ghost.ghost && bound >= 0 && E.effective_active h.Ghost.ghost heads.Ghost.ghost p}) @ unique ->
    {r : lowered | effective_lower_valid h.Ghost.ghost heads.Ghost.ghost bound r.#edits
      && Pref.own r.#state === lower_heap h.Ghost.ghost bound r.#edits
      && bound_root r.#tree === p && effective_bounded (Pref.own r.#state) heads.Ghost.ghost bound r.#tree
      && confined r.#edits r.#tree
      && Terminal_lower_spec.completed h.Ghost.ghost bound p (Pref.own r.#state) r.#edits r.#tree} @ unique =
  fun h heads witness scope order trees bound p t ->
    let goal = {heap = h.Ghost.ghost; bound; root = p} in
    let use : (r : {r : lowered | effective_lower_valid h.Ghost.ghost heads.Ghost.ghost bound r.#edits
      && Pref.own r.#state === lower_heap h.Ghost.ghost bound r.#edits
      && bound_root r.#tree === p && effective_bounded (Pref.own r.#state) heads.Ghost.ghost bound r.#tree
      && confined r.#edits r.#tree}) @ unique -> {r : lowered | effective_lower_valid goal.heap heads.Ghost.ghost goal.bound r.#edits
      && Pref.own r.#state === lower_heap goal.heap goal.bound r.#edits
      && bound_root r.#tree === goal.root && effective_bounded (Pref.own r.#state) heads.Ghost.ghost goal.bound r.#tree
      && confined r.#edits r.#tree} @ unique = fun r ->
      r in
    let out = run_lower goal h heads witness scope order trees bound p t use in
    let after = ghost_ (Pref.own (borrow_ out.#state)) in let edits = ghost_ out.#edits in let tree = ghost_ out.#tree in
    ghost_ (let next : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head after heads.Ghost.ghost x}) @ total = fun x ->
      witness.Ghost.ghost x; lower_head h.Ghost.ghost heads.Ghost.ghost bound edits x (); () in
      Terminal_lower_proofs.valid h.Ghost.ghost heads.Ghost.ghost bound edits ();
      Terminal_lower_proofs.bounded after heads.Ghost.ghost next bound tree ();
      Terminal_lower_spec.completed_def h.Ghost.ghost bound p after edits tree);
    let result = #{state = out.#state; edits; tree} in result
