open Copy_spec
open Level_spec
open Level_proofs
open Lower_locality_spec
open Lower_locality_proofs
open Level_finite_spec
open Level_lower

type lower_goal = { heap : node Pref.heap @@ ghost; bound : int @@ ghost;
  root : node Pref.t @@ ghost }

let rec run_lower : (goal : lower_goal) @ immutable -> (h : node Pref.heap Ghost.t) @ immutable ->
    (scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h.Ghost.ghost x) || finite_scope h.Ghost.ghost x})) Ghost.t) @ total ->
    (order : (((x : node Pref.t) @ immutable -> {u : unit | ordered h.Ghost.ghost x})) Ghost.t) @ total ->
    (trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else Level_unifier_spec.observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total ->
    (bound : int) -> (p : node Pref.t) @ immutable ->
    (t : {t : node Pref.token | Pref.own t === h.Ghost.ghost && bound >= 0 && active h.Ghost.ghost p}) @ unique ->
    (use : ((r : {r : lowered | lower_valid h.Ghost.ghost bound r.#edits
      && Pref.own r.#state === lower_heap h.Ghost.ghost bound r.#edits
      && bound_root r.#tree === p && bounded (Pref.own r.#state) bound r.#tree
      && confined r.#edits r.#tree}) @ unique -> {r : lowered | lower_valid goal.heap goal.bound r.#edits
      && Pref.own r.#state === lower_heap goal.heap goal.bound r.#edits
      && bound_root r.#tree === goal.root && bounded (Pref.own r.#state) goal.bound r.#tree
      && confined r.#edits r.#tree} @ unique)) ->
    {r : lowered | lower_valid goal.heap goal.bound r.#edits
      && Pref.own r.#state === lower_heap goal.heap goal.bound r.#edits
      && bound_root r.#tree === goal.root && bounded (Pref.own r.#state) goal.bound r.#tree
      && confined r.#edits r.#tree} @ unique =
  fun goal h scope order trees bound p t use ->
    let scope = ghost_ scope.Ghost.ghost in
    let order = ghost_ order.Ghost.ghost in
    let trees = ghost_ trees.Ghost.ghost in
    let refine_ t = t in
    ghost_ (active_def h.Ghost.ghost p; scope p; finite_scope_def h.Ghost.ghost p; source_ok_def h.Ghost.ghost p);
    let t : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ t in
    let refine_ old = Pref.read p (borrow_ t) in let refine_ t = t in
    let t : {t : node Pref.token | Pref.own t === h.Ghost.ghost && bound >= 0 && active h.Ghost.ghost p} = refine_ t in
    let refine_ t = t in
    if (match old.level with Generic -> false | Finite n -> n <= bound) then (
      let tree : {b : bounded | bound_root b === p && bounded h.Ghost.ghost bound b} @ immutable ghost = ghost_ (
        let refine_ ft = trees p in
        below_def h.Ghost.ghost p bound; at_level_def h.Ghost.ghost p;
        let u = () in let refine_ b = Pruned_lower_proofs.bounded_tree h.Ghost.ghost order bound ft (refine_ u) in refine_ b) in
      let refine_ tree = tree in
      let edits = ghost_ Keep in
      ghost_ (lower_valid_def h.Ghost.ghost bound edits; lower_heap_def h.Ghost.ghost bound edits; confined_def edits tree);
      let r = #{state = t; edits; tree} in use (refine_ r)
    ) else match old.desc with
    | Var | Bool | Word ->
      ghost_ (children_below_def h.Ghost.ghost old.desc bound);
      let t : {t : node Pref.token | Pref.own t === h.Ghost.ghost && bound >= 0 && active h.Ghost.ghost p
        && match H.at h.Ghost.ghost p with None -> false | Some v -> children_below h.Ghost.ghost v.desc bound} = refine_ t in
      let write_heap : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
      let refine_ t = t in
      let refine_ r = write_level write_heap bound p (refine_ t) in
      let tree = ghost_ (Tip p) in let edits = ghost_ r.#edits in
      let after = ghost_ (Pref.own (borrow_ r.#state)) in
      ghost_ (let u = () in lowering_at h.Ghost.ghost bound edits p (refine_ u);
        lower_frame_def h.Ghost.ghost after p; bound_root_def tree; bounded_def after bound tree);
      let r = #{state = r.#state; edits; tree} in use (refine_ r)
    | Link q | List q ->
      let t : {t : node Pref.token | Pref.own t === h.Ghost.ghost && bound >= 0 && active h.Ghost.ghost q} = refine_ t in
      let lower_heap_witness : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
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
      let resume_child : (child : {r : lowered | lower_valid lower_heap_witness.Ghost.ghost bound r.#edits
      && Pref.own r.#state === lower_heap lower_heap_witness.Ghost.ghost bound r.#edits
      && bound_root r.#tree === q && bounded (Pref.own r.#state) bound r.#tree
      && confined r.#edits r.#tree}) @ unique ->
        {r : lowered | lower_valid goal.heap goal.bound r.#edits
      && Pref.own r.#state === lower_heap goal.heap goal.bound r.#edits
      && bound_root r.#tree === goal.root && bounded (Pref.own r.#state) goal.bound r.#tree
      && confined r.#edits r.#tree} @ unique = fun child ->
        let refine_ child = child in
      let d = ghost_ child.#edits in let tree_child = ghost_ child.#tree in
      let mid = ghost_ (Pref.own (borrow_ child.#state)) in
      ghost_ (let u = () in lowering_at h.Ghost.ghost bound d p (refine_ u); frame_active h.Ghost.ghost mid p (refine_ u));
      ghost_ (let u = () in lower_bounded_at mid bound tree_child q (refine_ u);
        lower_frame_def h.Ghost.ghost mid p; children_below_def mid old.desc bound);
      let t = child.#state in let t : {t : node Pref.token | Pref.own t === mid && bound >= 0 && active mid p
        && match H.at mid p with None -> false | Some v -> children_below mid v.desc bound} = refine_ t in
      let write_heap : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (mid)} in
      let refine_ t = t in
      let refine_ result = write_level write_heap bound p (refine_ t) in
      let w = ghost_ result.#edits in let after = ghost_ (Pref.own (borrow_ result.#state)) in
      let frame : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame mid after x}) @ total ghost = ghost_ (fun x ->
        let u = () in let refine_ u = lowering_at mid bound w x (refine_ u) in refine_ u) in
      let edits = ghost_ (Sequence (d, w)) in let tree = ghost_ (Through (p, tree_child)) in
      ghost_ (let u = () in bounded_frame mid after frame bound tree_child (refine_ u);
        lowering_at h.Ghost.ghost bound d p (refine_ u); frame p; lower_frame_def h.Ghost.ghost mid p; lower_frame_def mid after p;
        lower_valid_def h.Ghost.ghost bound edits; lower_heap_def h.Ghost.ghost bound edits;
        bound_root_def tree; bounded_def after bound tree;
        confined_through d p tree_child (refine_ u);
        confined_root w tree p (refine_ u); confined_def edits tree);
      let r = #{state = result.#state; edits; tree} in use (refine_ r) in
      run_lower goal lower_heap_witness lower_scope_witness lower_order_witness lower_trees_witness bound q (refine_ t) resume_child
    | Arrow (a, b) ->
      let t : {t : node Pref.token | Pref.own t === h.Ghost.ghost && bound >= 0 && active h.Ghost.ghost a} = refine_ t in
      let lower_heap_witness : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
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
      let resume_left : (left : {r : lowered | lower_valid lower_heap_witness.Ghost.ghost bound r.#edits
      && Pref.own r.#state === lower_heap lower_heap_witness.Ghost.ghost bound r.#edits
      && bound_root r.#tree === a && bounded (Pref.own r.#state) bound r.#tree
      && confined r.#edits r.#tree}) @ unique ->
        {r : lowered | lower_valid goal.heap goal.bound r.#edits
      && Pref.own r.#state === lower_heap goal.heap goal.bound r.#edits
      && bound_root r.#tree === goal.root && bounded (Pref.own r.#state) goal.bound r.#tree
      && confined r.#edits r.#tree} @ unique = fun left ->
        let refine_ left = left in
      let d1 = ghost_ left.#edits in let ta = ghost_ left.#tree in
      let h1 = ghost_ (Pref.own (borrow_ left.#state)) in
      let scope1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || finite_scope h1 x}) @ total ghost = ghost_ (fun x ->
        let u = () in let refine_ u = Level_proofs.lower_scope h.Ghost.ghost scope bound d1 x (refine_ u) in refine_ u) in
      ghost_ (let u = () in lowering_at h.Ghost.ghost bound d1 b (refine_ u); frame_active h.Ghost.ghost h1 b (refine_ u));
      let t = left.#state in let t : {t : node Pref.token | Pref.own t === h1 && bound >= 0 && active h1 b} = refine_ t in
      let order1 : ((x : node Pref.t) @ immutable -> {u : unit | ordered h1 x}) @ total ghost = ghost_ (fun x ->
        order x; let u = () in let refine_ u = lowering_ordered h.Ghost.ghost bound d1 x (refine_ u) in refine_ u) in
      let trees1 : ((x : node Pref.t) @ immutable ->
        {t : tree | tree_root t === x && (if H.mem h1 x then finite h1 t else Level_unifier_spec.observe h1 x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
          let refine_ ft = trees x in let u = () in
          lowering_at h.Ghost.ghost bound d1 x (refine_ u); lower_frame_def h.Ghost.ghost h1 x;
          if H.mem h.Ghost.ghost x then (Level_finite_proofs.lower_finite h.Ghost.ghost bound d1 ft (refine_ u); refine_ ft)
          else (Level_unifier_spec.observe_def h.Ghost.ghost x; Level_unifier_spec.observe_def h1 x; refine_ ft)) in
      let lower_heap_witness : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (h1)} in
      let lower_scope_witness : (((x : node Pref.t) @ immutable ->
        {u : unit | not (H.mem lower_heap_witness.Ghost.ghost x) || Level_spec.finite_scope lower_heap_witness.Ghost.ghost x})) Ghost.t =
        {Ghost.ghost = ghost_ (refine_ scope1)} in
      let lower_order_witness : (((x : node Pref.t) @ immutable ->
        {u : unit | Level_spec.ordered lower_heap_witness.Ghost.ghost x})) Ghost.t =
        {Ghost.ghost = ghost_ (refine_ order1)} in
      let lower_trees_witness : (((x : node Pref.t) @ immutable ->
        {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
          (if H.mem lower_heap_witness.Ghost.ghost x then Level_finite_spec.finite lower_heap_witness.Ghost.ghost t else Level_unifier_spec.observe lower_heap_witness.Ghost.ghost x === None)} @ immutable)) Ghost.t =
        {Ghost.ghost = ghost_ (refine_ trees1)} in
      let refine_ t = t in
      let resume_right : (right : {r : lowered | lower_valid lower_heap_witness.Ghost.ghost bound r.#edits
      && Pref.own r.#state === lower_heap lower_heap_witness.Ghost.ghost bound r.#edits
      && bound_root r.#tree === b && bounded (Pref.own r.#state) bound r.#tree
      && confined r.#edits r.#tree}) @ unique ->
        {r : lowered | lower_valid goal.heap goal.bound r.#edits
      && Pref.own r.#state === lower_heap goal.heap goal.bound r.#edits
      && bound_root r.#tree === goal.root && bounded (Pref.own r.#state) goal.bound r.#tree
      && confined r.#edits r.#tree} @ unique = fun right ->
        let refine_ right = right in
      let d2 = ghost_ right.#edits in let tb = ghost_ right.#tree in
      let h2 = ghost_ (Pref.own (borrow_ right.#state)) in
      ghost_ (let u = () in lowering_at h.Ghost.ghost bound d1 p (refine_ u); frame_active h.Ghost.ghost h1 p (refine_ u);
        lowering_at h1 bound d2 p (refine_ u); frame_active h1 h2 p (refine_ u));
      ghost_ (let u = () in lower_bounded_at h1 bound ta a (refine_ u);
        lowering_at h1 bound d2 a (refine_ u); frame_below h1 h2 a bound (refine_ u);
        lower_bounded_at h2 bound tb b (refine_ u);
        lower_frame_def h.Ghost.ghost h1 p; lower_frame_def h1 h2 p; children_below_def h2 old.desc bound);
      let t = right.#state in let t : {t : node Pref.token | Pref.own t === h2 && bound >= 0 && active h2 p
        && match H.at h2 p with None -> false | Some v -> children_below h2 v.desc bound} = refine_ t in
      let write_heap : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (h2)} in
      let refine_ t = t in
      let refine_ result = write_level write_heap bound p (refine_ t) in
      let w = ghost_ result.#edits in let after = ghost_ (Pref.own (borrow_ result.#state)) in
      let suffix = ghost_ (Sequence (d2, w)) in let edits = ghost_ (Sequence (d1, suffix)) in
      let frame2 : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame h2 after x}) @ total ghost = ghost_ (fun x ->
        let u = () in let refine_ u = lowering_at h2 bound w x (refine_ u) in refine_ u) in
      let frame1 : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame h1 after x}) @ total ghost = ghost_ (fun x ->
        let u = () in lowering_at h1 bound d2 x (refine_ u); frame2 x;
        let refine_ u = frame_trans h1 h2 after x (refine_ u) in refine_ u) in
      let tree = ghost_ (Fork (p, ta, tb)) in
      ghost_ (let u = () in bounded_frame h1 after frame1 bound ta (refine_ u); bounded_frame h2 after frame2 bound tb (refine_ u);
        lowering_at h.Ghost.ghost bound d1 p (refine_ u); frame1 p; lower_frame_def h.Ghost.ghost h1 p; lower_frame_def h1 after p;
        lower_heap_def h1 bound suffix; lower_valid_def h1 bound suffix;
        lower_heap_def h.Ghost.ghost bound edits; lower_valid_def h.Ghost.ghost bound edits;
        bound_root_def tree; bounded_def after bound tree;
        confined_fork d1 d2 p ta tb (refine_ u);
        confined_root w tree p (refine_ u);
        confined_def suffix tree; confined_def edits tree);
      let r = #{state = result.#state; edits; tree} in use (refine_ r)
 in
      run_lower goal lower_heap_witness lower_scope_witness lower_order_witness lower_trees_witness bound b (refine_ t) resume_right in
      run_lower goal lower_heap_witness lower_scope_witness lower_order_witness lower_trees_witness bound a (refine_ t) resume_left

let lower : (h : node Pref.heap Ghost.t) @ immutable ->
    (scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h.Ghost.ghost x) || finite_scope h.Ghost.ghost x})) Ghost.t) @ total ->
    (order : (((x : node Pref.t) @ immutable -> {u : unit | ordered h.Ghost.ghost x})) Ghost.t) @ total ->
    (trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else Level_unifier_spec.observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total ->
    (bound : int) -> (p : node Pref.t) @ immutable ->
    (t : {t : node Pref.token | Pref.own t === h.Ghost.ghost && bound >= 0 && active h.Ghost.ghost p}) @ unique ->
    {r : lowered | lower_valid h.Ghost.ghost bound r.#edits
      && Pref.own r.#state === lower_heap h.Ghost.ghost bound r.#edits
      && bound_root r.#tree === p && bounded (Pref.own r.#state) bound r.#tree
      && confined r.#edits r.#tree} @ unique =
  fun h scope order trees bound p t ->
    let goal = {heap = h.Ghost.ghost; bound; root = p} in
    let use : (r : {r : lowered | lower_valid h.Ghost.ghost bound r.#edits
      && Pref.own r.#state === lower_heap h.Ghost.ghost bound r.#edits
      && bound_root r.#tree === p && bounded (Pref.own r.#state) bound r.#tree
      && confined r.#edits r.#tree}) @ unique -> {r : lowered | lower_valid goal.heap goal.bound r.#edits
      && Pref.own r.#state === lower_heap goal.heap goal.bound r.#edits
      && bound_root r.#tree === goal.root && bounded (Pref.own r.#state) goal.bound r.#tree
      && confined r.#edits r.#tree} @ unique = fun r ->
      let refine_ r = r in refine_ r in
    let refine_ out = run_lower goal h scope order trees bound p t use in refine_ out
