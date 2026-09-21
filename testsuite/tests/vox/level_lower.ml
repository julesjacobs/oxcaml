open Copy_spec
open Level_spec
open Level_proofs

type written = #{state : node Pref.token; edits : lowering @@ ghost}

let write_level : (h : node Pref.heap) @ immutable ghost -> (bound : int) ->
    (p : node Pref.t) @ immutable ->
    (t : {t : node Pref.token | Pref.own t === h && bound >= 0 && active h p
      && match H.at h p with None -> false | Some v -> children_below h v.desc bound}) @ unique ->
    {r : written | lower_valid h bound r.#edits
      && Pref.own r.#state === lower_heap h bound r.#edits
      && below (Pref.own r.#state) p bound} @ unique = fun h bound p t ->
  let refine_ t = t in
  ghost_ (active_def h p; at_level_def h p);
  let t : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ t in
  let refine_ old = Pref.read p (borrow_ t) in let refine_ t = t in
  let v = lower_cell old bound in
  let t : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ t in
  let refine_ t = Pref.write p v t in
  let edits = ghost_ (Lower (p, old, Keep)) in
  let after = ghost_ (lower_heap h bound edits) in
  ghost_ (let empty = Keep in lower_heap_def h bound empty; lower_valid_def h bound empty;
    lower_heap_def h bound edits; lower_valid_def h bound edits; lower_cell_def old bound;
    below_def after p bound; at_level_def after p);
  let r = #{state = t; edits} in refine_ r

let rec lower : (h : node Pref.heap) @ immutable ghost ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || finite_scope h x})) @ total ghost ->
    (bound : int) -> (p : node Pref.t) @ immutable ->
    (t : {t : node Pref.token | Pref.own t === h && bound >= 0 && active h p}) @ unique ->
    {r : lowered | lower_valid h bound r.#edits
      && Pref.own r.#state === lower_heap h bound r.#edits
      && bound_root r.#tree === p && bounded (Pref.own r.#state) bound r.#tree} @ unique =
  fun h scope bound p t ->
    let refine_ t = t in
    ghost_ (active_def h p; scope p; finite_scope_def h p; source_ok_def h p);
    let t : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ t in
    let refine_ old = Pref.read p (borrow_ t) in let refine_ t = t in
    let t : {t : node Pref.token | Pref.own t === h && bound >= 0 && active h p} = refine_ t in
    let refine_ t = t in
    match old.desc with
    | Var | Bool ->
      ghost_ (children_below_def h old.desc bound);
      let t : {t : node Pref.token | Pref.own t === h && bound >= 0 && active h p
        && match H.at h p with None -> false | Some v -> children_below h v.desc bound} = refine_ t in
      let refine_ r = write_level h bound p t in
      let tree = ghost_ (Tip p) in let edits = ghost_ r.#edits in
      let after = ghost_ (Pref.own (borrow_ r.#state)) in
      ghost_ (let u = () in lowering_at h bound edits p (refine_ u);
        lower_frame_def h after p; bound_root_def tree; bounded_def after bound tree);
      let r = #{state = r.#state; edits; tree} in refine_ r
    | Link q ->
      let t : {t : node Pref.token | Pref.own t === h && bound >= 0 && active h q} = refine_ t in
      let refine_ child = lower h scope bound q t in
      let d = ghost_ child.#edits in let tree_child = ghost_ child.#tree in
      let mid = ghost_ (Pref.own (borrow_ child.#state)) in
      ghost_ (let u = () in lowering_at h bound d p (refine_ u); frame_active h mid p (refine_ u));
      ghost_ (let u = () in lower_bounded_at mid bound tree_child q (refine_ u);
        lower_frame_def h mid p; children_below_def mid old.desc bound);
      let t = child.#state in let t : {t : node Pref.token | Pref.own t === mid && bound >= 0 && active mid p
        && match H.at mid p with None -> false | Some v -> children_below mid v.desc bound} = refine_ t in
      let refine_ result = write_level mid bound p t in
      let w = ghost_ result.#edits in let after = ghost_ (Pref.own (borrow_ result.#state)) in
      let frame : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame mid after x}) @ total ghost = ghost_ (fun x ->
        let u = () in let refine_ u = lowering_at mid bound w x (refine_ u) in refine_ u) in
      let edits = ghost_ (Sequence (d, w)) in let tree = ghost_ (Through (p, tree_child)) in
      ghost_ (let u = () in bounded_frame mid after frame bound tree_child (refine_ u);
        lowering_at h bound d p (refine_ u); frame p; lower_frame_def h mid p; lower_frame_def mid after p;
        lower_valid_def h bound edits; lower_heap_def h bound edits;
        bound_root_def tree; bounded_def after bound tree);
      let r = #{state = result.#state; edits; tree} in refine_ r
    | Arrow (a, b) ->
      let t : {t : node Pref.token | Pref.own t === h && bound >= 0 && active h a} = refine_ t in
      let refine_ left = lower h scope bound a t in
      let d1 = ghost_ left.#edits in let ta = ghost_ left.#tree in
      let h1 = ghost_ (Pref.own (borrow_ left.#state)) in
      let scope1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || finite_scope h1 x}) @ total ghost = ghost_ (fun x ->
        let u = () in let refine_ u = lower_scope h scope bound d1 x (refine_ u) in refine_ u) in
      ghost_ (let u = () in lowering_at h bound d1 b (refine_ u); frame_active h h1 b (refine_ u));
      let t = left.#state in let t : {t : node Pref.token | Pref.own t === h1 && bound >= 0 && active h1 b} = refine_ t in
      let refine_ right = lower h1 scope1 bound b t in
      let d2 = ghost_ right.#edits in let tb = ghost_ right.#tree in
      let h2 = ghost_ (Pref.own (borrow_ right.#state)) in
      ghost_ (let u = () in lowering_at h bound d1 p (refine_ u); frame_active h h1 p (refine_ u);
        lowering_at h1 bound d2 p (refine_ u); frame_active h1 h2 p (refine_ u));
      ghost_ (let u = () in lower_bounded_at h1 bound ta a (refine_ u);
        lowering_at h1 bound d2 a (refine_ u); frame_below h1 h2 a bound (refine_ u);
        lower_bounded_at h2 bound tb b (refine_ u);
        lower_frame_def h h1 p; lower_frame_def h1 h2 p; children_below_def h2 old.desc bound);
      let t = right.#state in let t : {t : node Pref.token | Pref.own t === h2 && bound >= 0 && active h2 p
        && match H.at h2 p with None -> false | Some v -> children_below h2 v.desc bound} = refine_ t in
      let refine_ result = write_level h2 bound p t in
      let w = ghost_ result.#edits in let after = ghost_ (Pref.own (borrow_ result.#state)) in
      let suffix = ghost_ (Sequence (d2, w)) in let edits = ghost_ (Sequence (d1, suffix)) in
      let frame2 : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame h2 after x}) @ total ghost = ghost_ (fun x ->
        let u = () in let refine_ u = lowering_at h2 bound w x (refine_ u) in refine_ u) in
      let frame1 : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame h1 after x}) @ total ghost = ghost_ (fun x ->
        let u = () in lowering_at h1 bound d2 x (refine_ u); frame2 x;
        let refine_ u = frame_trans h1 h2 after x (refine_ u) in refine_ u) in
      let tree = ghost_ (Fork (p, ta, tb)) in
      ghost_ (let u = () in bounded_frame h1 after frame1 bound ta (refine_ u); bounded_frame h2 after frame2 bound tb (refine_ u);
        lowering_at h bound d1 p (refine_ u); frame1 p; lower_frame_def h h1 p; lower_frame_def h1 after p;
        lower_heap_def h1 bound suffix; lower_valid_def h1 bound suffix;
        lower_heap_def h bound edits; lower_valid_def h bound edits;
        bound_root_def tree; bounded_def after bound tree);
      let r = #{state = result.#state; edits; tree} in refine_ r
