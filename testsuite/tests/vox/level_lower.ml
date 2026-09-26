open Copy_spec
open Level_spec
open Level_proofs
open Lower_locality_spec
open Lower_locality_proofs

type written = Level_spec.written

let write_level : (h : (node Pref.heap) Ghost.t) @ immutable -> (bound : int) ->
    (p : node Pref.t) @ immutable ->
    (t : {t : node Pref.token | Pref.own t === h.Ghost.ghost && bound >= 0 && active h.Ghost.ghost p
      && match H.at h.Ghost.ghost p with None -> false | Some v -> children_below h.Ghost.ghost v.desc bound}) @ unique ->
    {r : written | lower_valid h.Ghost.ghost bound r.#edits
      && Pref.own r.#state === lower_heap h.Ghost.ghost bound r.#edits
      && below (Pref.own r.#state) p bound && confined r.#edits (Tip p)} @ unique = fun h bound p t ->
  ghost_ (active_def h.Ghost.ghost p; at_level_def h.Ghost.ghost p);
  let old = Pref.read p (borrow_ t) in let v = lower_cell old bound in
  let t = Pref.write p v t in
  let edits = ghost_ (Lower (p, old, Keep)) in
  let after = ghost_ (lower_heap h.Ghost.ghost bound edits) in
  ghost_ (let empty = Keep in lower_heap_def h.Ghost.ghost bound empty; lower_valid_def h.Ghost.ghost bound empty;
    lower_heap_def h.Ghost.ghost bound edits; lower_valid_def h.Ghost.ghost bound edits; lower_cell_def old bound;
    Copy_heap_proofs.put_frame h.Ghost.ghost p v p;
    below_def after p bound; at_level_def after p;
    let tree = Tip p in contains_def tree p; confined_def edits tree;
    let empty = Keep in confined_def empty tree; ());
  let r = #{state = t; edits} in r

let rec lower : (h : node Pref.heap) @ immutable ghost ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || finite_scope h x})) @ total ghost ->
    (bound : int) -> (p : node Pref.t) @ immutable ->
    (t : {t : node Pref.token | Pref.own t === h && bound >= 0 && active h p}) @ unique ->
    {r : lowered | lower_valid h bound r.#edits
      && Pref.own r.#state === lower_heap h bound r.#edits
      && bound_root r.#tree === p && bounded (Pref.own r.#state) bound r.#tree
      && confined r.#edits r.#tree} @ unique =
  fun h scope bound p t ->
    ghost_ (active_def h p; scope p; finite_scope_def h p; source_ok_def h p);
    let old = Pref.read p (borrow_ t) in let t : {t : node Pref.token | Pref.own t === h && bound >= 0 && active h p} = t in
    match old.desc with
    | Var | Bool | Word ->
      ghost_ (children_below_def h old.desc bound);
      let t : {t : node Pref.token | Pref.own t === h && bound >= 0 && active h p
        && match H.at h p with None -> false | Some v -> children_below h v.desc bound} = t in
      let h_witness1 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h)} in
      let state_argument2 = t in
      let r = write_level h_witness1 bound p (state_argument2) in
      let tree = ghost_ (Tip p) in let edits = ghost_ r.#edits in
      let after = ghost_ (Pref.own (borrow_ r.#state)) in
      ghost_ (lowering_at h bound edits p ();
        lower_frame_def h after p; bound_root_def tree; bounded_def after bound tree);
      let r = #{state = r.#state; edits; tree} in r
    | Link q | List q ->
      let t : {t : node Pref.token | Pref.own t === h && bound >= 0 && active h q} = t in
      let child = lower h scope bound q t in
      let d = ghost_ child.#edits in let tree_child = ghost_ child.#tree in
      let mid = ghost_ (Pref.own (borrow_ child.#state)) in
      ghost_ (lowering_at h bound d p (); frame_active h mid p ());
      ghost_ (lower_bounded_at mid bound tree_child q ();
        lower_frame_def h mid p; children_below_def mid old.desc bound);
      let t = child.#state in let t : {t : node Pref.token | Pref.own t === mid && bound >= 0 && active mid p
        && match H.at mid p with None -> false | Some v -> children_below mid v.desc bound} = t in
      let h_witness3 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (mid)} in
      let state_argument4 = t in
      let result = write_level h_witness3 bound p (state_argument4) in
      let w = ghost_ result.#edits in let after = ghost_ (Pref.own (borrow_ result.#state)) in
      let frame : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame mid after x}) @ total ghost = ghost_ (fun x ->
        let () = lowering_at mid bound w x () in ()) in
      let edits = ghost_ (Sequence (d, w)) in let tree = ghost_ (Through (p, tree_child)) in
      ghost_ (bounded_frame mid after frame bound tree_child ();
        lowering_at h bound d p (); frame p; lower_frame_def h mid p; lower_frame_def mid after p;
        lower_valid_def h bound edits; lower_heap_def h bound edits;
        bound_root_def tree; bounded_def after bound tree;
        confined_through d p tree_child ();
        confined_root w tree p (); confined_def edits tree);
      let r = #{state = result.#state; edits; tree} in r
    | Arrow (a, b) ->
      let t : {t : node Pref.token | Pref.own t === h && bound >= 0 && active h a} = t in
      let left = lower h scope bound a t in
      let d1 = ghost_ left.#edits in let ta = ghost_ left.#tree in
      let h1 = ghost_ (Pref.own (borrow_ left.#state)) in
      let scope1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || finite_scope h1 x}) @ total ghost = ghost_ (fun x ->
        let () = lower_scope h scope bound d1 x () in ()) in
      ghost_ (lowering_at h bound d1 b (); frame_active h h1 b ());
      let t = left.#state in let t : {t : node Pref.token | Pref.own t === h1 && bound >= 0 && active h1 b} = t in
      let right = lower h1 scope1 bound b t in
      let d2 = ghost_ right.#edits in let tb = ghost_ right.#tree in
      let h2 = ghost_ (Pref.own (borrow_ right.#state)) in
      ghost_ (lowering_at h bound d1 p (); frame_active h h1 p ();
        lowering_at h1 bound d2 p (); frame_active h1 h2 p ());
      ghost_ (lower_bounded_at h1 bound ta a ();
        lowering_at h1 bound d2 a (); frame_below h1 h2 a bound ();
        lower_bounded_at h2 bound tb b ();
        lower_frame_def h h1 p; lower_frame_def h1 h2 p; children_below_def h2 old.desc bound);
      let t = right.#state in let t : {t : node Pref.token | Pref.own t === h2 && bound >= 0 && active h2 p
        && match H.at h2 p with None -> false | Some v -> children_below h2 v.desc bound} = t in
      let h_witness5 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h2)} in
      let state_argument6 = t in
      let result = write_level h_witness5 bound p (state_argument6) in
      let w = ghost_ result.#edits in let after = ghost_ (Pref.own (borrow_ result.#state)) in
      let suffix = ghost_ (Sequence (d2, w)) in let edits = ghost_ (Sequence (d1, suffix)) in
      let frame2 : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame h2 after x}) @ total ghost = ghost_ (fun x ->
        let () = lowering_at h2 bound w x () in ()) in
      let frame1 : ((x : node Pref.t) @ immutable -> {u : unit | lower_frame h1 after x}) @ total ghost = ghost_ (fun x ->
        lowering_at h1 bound d2 x (); frame2 x;
        let () = frame_trans h1 h2 after x () in ()) in
      let tree = ghost_ (Fork (p, ta, tb)) in
      ghost_ (bounded_frame h1 after frame1 bound ta (); bounded_frame h2 after frame2 bound tb ();
        lowering_at h bound d1 p (); frame1 p; lower_frame_def h h1 p; lower_frame_def h1 after p;
        lower_heap_def h1 bound suffix; lower_valid_def h1 bound suffix;
        lower_heap_def h bound edits; lower_valid_def h bound edits;
        bound_root_def tree; bounded_def after bound tree;
        confined_fork d1 d2 p ta tb ();
        confined_root w tree p ();
        confined_def suffix tree; confined_def edits tree);
      let r = #{state = result.#state; edits; tree} in r
