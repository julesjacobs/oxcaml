open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec
open Optimized_unifier_spec
open Level_unifier_metadata
open Optimized_metadata
type unify_goal = {heap : node Pref.heap @@ ghost; left : node Pref.t @@ ghost; right : node Pref.t @@ ghost}

let rec raw_work : (goal : unify_goal) @ immutable ->
    (h : (node Pref.heap) Ghost.t) @ immutable  ->(scope : (((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h.Ghost.ghost q) || finite_scope h.Ghost.ghost q})) Ghost.t) @ total  ->(unmarked : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t) @ total  ->(order : (((x : node Pref.t) @ immutable -> {u : unit | ordered h.Ghost.ghost x})) Ghost.t) @ total  ->(trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total  ->
    (p : node Pref.t) @ immutable  -> (q : node Pref.t) @ immutable  ->
    (rp : {r : resolved | H.mem h.Ghost.ghost r.#value && active h.Ghost.ghost r.#value && terminal h.Ghost.ghost r.#value
      && resolves h.Ghost.ghost p r.#value r.#path})  ->
    (sq : {r : resolved | H.mem h.Ghost.ghost r.#value && active h.Ghost.ghost r.#value && terminal h.Ghost.ghost r.#value
      && resolves h.Ghost.ghost q r.#value r.#path})  ->
    (t : {t : node Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p && H.mem h.Ghost.ghost q && active h.Ghost.ghost p && active h.Ghost.ghost q}) @ unique  ->
    (use : ((answer : {r : result | unified h.Ghost.ghost p q r.#ok (Pref.own r.#state) r.#derivation}) @ unique -> {answer : result | unified goal.heap goal.left goal.right answer.#ok (Pref.own answer.#state) answer.#derivation} @ unique)) ->
  {answer : result | unified goal.heap goal.left goal.right answer.#ok (Pref.own answer.#state) answer.#derivation} @ unique = fun goal h scope unmarked order trees p q rp sq t use ->
  let refine_ rp = rp in let refine_ sq = sq in
  let refine_ t = t in
  let r = rp.#value in
  let s = sq.#value in
  let finish_roots : (result : {answer : result |
      unified h.Ghost.ghost r s answer.#ok (Pref.own answer.#state) answer.#derivation}) @ unique -> {answer : result | unified goal.heap goal.left goal.right answer.#ok (Pref.own answer.#state) answer.#derivation} @ unique = fun result ->
let refine_ result = result in
  let ok = result.#ok in
  let t = result.#state in
  let after = ghost_ (Pref.own (borrow_ t)) in
  let d = ghost_ (Resolve (r, s, rp.#path, sq.#path, result.#derivation)) in
  ghost_ (unified_def h.Ghost.ghost p q ok after d);
  let answer = #{ok; state = t; derivation = d} in
  use (refine_ answer) in
    let refine_ equal = Pref.equal r s in
    if equal then
      let old = ghost_ Level_unifier_spec.Same in
      ghost_ (let ok = true in Level_unifier_spec.unified_def h.Ghost.ghost r s ok h.Ghost.ghost old; ());
      let d = ghost_ (Base old) in
      let ok = true in
      ghost_ (unified_def h.Ghost.ghost r s ok h.Ghost.ghost d);
      finish_roots (refine_ #{ok; state = t; derivation = d})
    else
      let n : {n : desc | Some n === observe h.Ghost.ghost r} =
        let b = borrow_ t in
        let b : {b : node Pref.token | H.mem (Pref.own b) r} = refine_ b in
        let refine_ old = Pref.read r b in ghost_ (observe_def h.Ghost.ghost r);
        let n = old.desc in refine_ n in
      let refine_ n = n in
      let m : {m : desc | Some m === observe h.Ghost.ghost s} =
        let b = borrow_ t in
        let b : {b : node Pref.token | H.mem (Pref.own b) s} = refine_ b in
        let refine_ old = Pref.read s b in ghost_ (observe_def h.Ghost.ghost s);
        let m = old.desc in refine_ m in
      let refine_ m = m in
      ghost_ (terminal_def h.Ghost.ghost r);
      ghost_ (terminal_def h.Ghost.ghost s);
      match n, m with
      | Var, _ ->
        let t : {t : node Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost r && H.mem h.Ghost.ghost s && active h.Ghost.ghost r && active h.Ghost.ghost s
          && observe h.Ghost.ghost r === Some Var && terminal h.Ghost.ghost s && not (r === s)} = refine_ t in
        let h_witness13 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
        let scope_witness14 : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness13.Ghost.ghost x) || finite_scope h_witness13.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
        let unmarked_witness15 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h_witness13.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t = {Ghost.ghost = ghost_ (refine_ unmarked.Ghost.ghost)} in
        let order_witness16 : (((x : node Pref.t) @ immutable -> {u : unit | ordered h_witness13.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ order.Ghost.ghost)} in
        let trees_witness17 : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h_witness13.Ghost.ghost x then finite h_witness13.Ghost.ghost t else observe h_witness13.Ghost.ghost x === None)} @ immutable)) Ghost.t = {Ghost.ghost = ghost_ (refine_ trees.Ghost.ghost)} in
        let refine_ state_argument18 = t in
        let refine_ answer = Pruned_bind.bind h_witness13 scope_witness14 unmarked_witness15 order_witness16 trees_witness17 r s (refine_ state_argument18) in
        let after = ghost_ (Pref.own (borrow_ answer.#state)) in
        let d = ghost_ (Base answer.#derivation) in let ok = answer.#ok in
        ghost_ (unified_def h.Ghost.ghost r s ok after d);
        finish_roots (refine_ #{ok; state = answer.#state; derivation = d})
      | _, Var ->
        let t : {t : node Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost s && H.mem h.Ghost.ghost r && active h.Ghost.ghost s && active h.Ghost.ghost r
          && observe h.Ghost.ghost s === Some Var && terminal h.Ghost.ghost r && not (s === r)} = refine_ t in
        let h_witness19 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
        let scope_witness20 : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness19.Ghost.ghost x) || finite_scope h_witness19.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
        let unmarked_witness21 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h_witness19.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t = {Ghost.ghost = ghost_ (refine_ unmarked.Ghost.ghost)} in
        let order_witness22 : (((x : node Pref.t) @ immutable -> {u : unit | ordered h_witness19.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ order.Ghost.ghost)} in
        let trees_witness23 : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h_witness19.Ghost.ghost x then finite h_witness19.Ghost.ghost t else observe h_witness19.Ghost.ghost x === None)} @ immutable)) Ghost.t = {Ghost.ghost = ghost_ (refine_ trees.Ghost.ghost)} in
        let refine_ state_argument24 = t in
        let refine_ answer = Pruned_bind.bind h_witness19 scope_witness20 unmarked_witness21 order_witness22 trees_witness23 s r (refine_ state_argument24) in
        let ok = answer.#ok in
        let t = answer.#state in
        let after = ghost_ (Pref.own (borrow_ t)) in
        let old = ghost_ (Level_unifier_spec.Swap answer.#derivation) in
        ghost_ (Level_unifier_spec.unified_def h.Ghost.ghost r s ok after old);
        let d = ghost_ (Base old) in
        ghost_ (unified_def h.Ghost.ghost r s ok after d);
        finish_roots (refine_ #{ok; state = t; derivation = d})
      | Bool, Bool | Word, Word ->
        let ok = true in
        let old = ghost_ Level_unifier_spec.Constants in
      ghost_ (let ok = true in Level_unifier_spec.unified_def h.Ghost.ghost r s ok h.Ghost.ghost old; ());
      let d = ghost_ (Base old) in
        ghost_ (unified_def h.Ghost.ghost r s ok h.Ghost.ghost d);
        finish_roots (refine_ #{ok; state = t; derivation = d})
      | List a, List b ->
        ghost_ (scope.Ghost.ghost r; scope.Ghost.ghost s;
          finite_scope_def h.Ghost.ghost r; source_ok_def h.Ghost.ghost r; observe_def h.Ghost.ghost r;
          finite_scope_def h.Ghost.ghost s; source_ok_def h.Ghost.ghost s; observe_def h.Ghost.ghost s);
        let t : {t : node Pref.token | Pref.own t === h.Ghost.ghost
          && H.mem h.Ghost.ghost a && H.mem h.Ghost.ghost b
          && active h.Ghost.ghost a && active h.Ghost.ghost b} = refine_ t in
        let resume_child : (child : {out : result |
          unified h.Ghost.ghost a b out.#ok (Pref.own out.#state) out.#derivation}) @ unique ->
          {answer : result | unified goal.heap goal.left goal.right answer.#ok
            (Pref.own answer.#state) answer.#derivation} @ unique = fun child ->
          let refine_ child = child in let ok = child.#ok in let t = child.#state in
          let after = ghost_ (Pref.own (borrow_ t)) in
          let d = ghost_ (List_children (a, b, child.#derivation)) in
          ghost_ (unified_def h.Ghost.ghost r s ok after d);
          if not ok then finish_roots (refine_ #{ok; state = t; derivation = d}) else (
            let trees_after : ((x : node Pref.t) @ immutable ->
              {t : tree | tree_root t === x && (if H.mem after x then finite after t else observe after x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
                let u = () in let refine_ tree = Optimized_finite_proofs.unified_finite_at h.Ghost.ghost trees.Ghost.ghost r s ok after d x (refine_ u) in refine_ tree) in
            let scope_after : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem after x) || finite_scope after x}) @ total ghost = ghost_ (fun x ->
              let u = () in let refine_ u = unified_scope h.Ghost.ghost scope.Ghost.ghost r s ok after d x (refine_ u) in refine_ u) in
            ghost_ (let u = () in unified_frame h.Ghost.ghost r s ok after d r (refine_ u); unified_frame h.Ghost.ghost r s ok after d s (refine_ u);
              unified_active h.Ghost.ghost r s ok after d r (refine_ u); unified_active h.Ghost.ghost r s ok after d s (refine_ u); ());
            let t : {t : node Pref.token | Pref.own t === after && unified h.Ghost.ghost r s true after d
              && H.mem after r && H.mem after s && active after r && active after s} = refine_ t in
            let before_witness7 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
            let h_witness8 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (after)} in
            let d_witness9 : (derivation) Ghost.t = {Ghost.ghost = ghost_ (d)} in
            let trees_witness10 : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h_witness8.Ghost.ghost x then finite h_witness8.Ghost.ghost t else observe h_witness8.Ghost.ghost x === None)} @ immutable)) Ghost.t = {Ghost.ghost = ghost_ (refine_ trees_after)} in
            let scope_witness11 : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h_witness8.Ghost.ghost x) || finite_scope h_witness8.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope_after)} in
            let refine_ state_argument12 = t in
            let refine_ linked = Optimized_link.finish before_witness7 r s h_witness8 d_witness9 trees_witness10 scope_witness11 (refine_ state_argument12) in finish_roots (refine_ linked)) in
        unify_work goal h scope unmarked order trees a b (refine_ t) resume_child
      | Arrow (a, b), Arrow (c, e) ->
        ghost_ (scope.Ghost.ghost r);
        ghost_ (scope.Ghost.ghost s);
        ghost_ (finite_scope_def h.Ghost.ghost r; source_ok_def h.Ghost.ghost r; observe_def h.Ghost.ghost r);
        ghost_ (finite_scope_def h.Ghost.ghost s; source_ok_def h.Ghost.ghost s; observe_def h.Ghost.ghost s);
        let t : {t : node Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost a && H.mem h.Ghost.ghost c && active h.Ghost.ghost a && active h.Ghost.ghost c} = refine_ t in
        let h_witness25 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
        let scope_witness26 : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h_witness25.Ghost.ghost x) || finite_scope h_witness25.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
        let unmarked_witness27 : (((x : node Pref.t) @ immutable -> {u : unit | match H.at h_witness25.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t = {Ghost.ghost = ghost_ (refine_ unmarked.Ghost.ghost)} in
        let order_witness28 : (((x : node Pref.t) @ immutable -> {u : unit | ordered h_witness25.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ order.Ghost.ghost)} in
        let trees_witness29 : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h_witness25.Ghost.ghost x then finite h_witness25.Ghost.ghost t else observe h_witness25.Ghost.ghost x === None)} @ immutable)) Ghost.t = {Ghost.ghost = ghost_ (refine_ trees.Ghost.ghost)} in
        let refine_ state_argument30 = t in
        let resume_left : (left : {out : result | unified h_witness25.Ghost.ghost a c out.#ok (Pref.own out.#state) out.#derivation}) @ unique -> {answer : result | unified goal.heap goal.left goal.right answer.#ok (Pref.own answer.#state) answer.#derivation} @ unique = fun left ->
        let refine_ left = left in
        let left_ok = left.#ok in
        let ld = ghost_ left.#derivation in
        let t = left.#state in
        let middle = ghost_ (Pref.own (borrow_ t)) in
        if left_ok then
          let scope_middle : ((x : node Pref.t) @ immutable ->
              {u : unit | not (H.mem middle x) || finite_scope middle x}) @ total ghost =
            ghost_ (fun x ->
              scope.Ghost.ghost x;
              let u = () in
              unified_scope h.Ghost.ghost scope.Ghost.ghost a c left_ok middle ld x (refine_ u);
              refine_ u) in
          let proof = ghost_ (
            let u = () in
            unified_frame h.Ghost.ghost a c left_ok middle ld b (refine_ u);
            unified_frame h.Ghost.ghost a c left_ok middle ld e (refine_ u);
            unified_active h.Ghost.ghost a c left_ok middle ld b (refine_ u);
            unified_active h.Ghost.ghost a c left_ok middle ld e (refine_ u);
            let proof : {u : unit | H.mem middle b && H.mem middle e && active middle b && active middle e} = refine_ u in proof) in
          let refine_ proof = proof in
          let t : {t : node Pref.token | Pref.own t === middle && H.mem middle b && H.mem middle e && active middle b && active middle e} = refine_ t in
          let unmarked_middle : ((x : node Pref.t) @ immutable ->
            {u : unit | match H.at middle x with None -> true | Some v -> not v.visited}) @ total ghost = ghost_ (fun x ->
            unmarked.Ghost.ghost x; let u = () in unified_scratch h.Ghost.ghost a c left_ok middle ld x (refine_ u);
            scratch_frame_def h.Ghost.ghost middle x; refine_ u) in
          let trees_middle : ((x : node Pref.t) @ immutable ->
            {t : tree | tree_root t === x && (if H.mem middle x then finite middle t else observe middle x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
              let u = () in let refine_ tree = Optimized_finite_proofs.unified_finite_at h.Ghost.ghost trees.Ghost.ghost a c left_ok middle ld x (refine_ u) in refine_ tree) in
          let order_middle : ((x : node Pref.t) @ immutable -> {u : unit | ordered middle x}) @ total ghost = ghost_ (fun x ->
            let u = () in let refine_ u = unified_ordered h.Ghost.ghost order.Ghost.ghost a c left_ok middle ld x (refine_ u) in refine_ u) in
          let h_witness31 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (middle)} in
          let scope_witness32 : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h_witness31.Ghost.ghost x) || finite_scope h_witness31.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope_middle)} in
          let unmarked_witness33 : (((x : node Pref.t) @ immutable -> {u : unit | match H.at h_witness31.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t = {Ghost.ghost = ghost_ (refine_ unmarked_middle)} in
          let order_witness34 : (((x : node Pref.t) @ immutable -> {u : unit | ordered h_witness31.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ order_middle)} in
          let trees_witness35 : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h_witness31.Ghost.ghost x then finite h_witness31.Ghost.ghost t else observe h_witness31.Ghost.ghost x === None)} @ immutable)) Ghost.t = {Ghost.ghost = ghost_ (refine_ trees_middle)} in
          let refine_ state_argument36 = t in
          let resume_right : (right : {out : result | unified h_witness31.Ghost.ghost b e out.#ok (Pref.own out.#state) out.#derivation}) @ unique -> {answer : result | unified goal.heap goal.left goal.right answer.#ok (Pref.own answer.#state) answer.#derivation} @ unique = fun right ->
          let refine_ right = right in
          let ok = right.#ok in
          let t = right.#state in
          let after = ghost_ (Pref.own (borrow_ t)) in
          let d = ghost_ (Children (a, b, c, e, middle, left_ok, ld, right.#derivation)) in
          ghost_ (unified_def h.Ghost.ghost r s ok after d);
          if not ok then finish_roots (refine_ #{ok; state = t; derivation = d}) else (
            let trees_after : ((x : node Pref.t) @ immutable ->
              {t : tree | tree_root t === x && (if H.mem after x then finite after t else observe after x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
                let u = () in let refine_ tree = Optimized_finite_proofs.unified_finite_at h.Ghost.ghost trees.Ghost.ghost r s ok after d x (refine_ u) in refine_ tree) in
            let scope_after : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem after x) || finite_scope after x}) @ total ghost = ghost_ (fun x ->
              let u = () in let refine_ u = unified_scope h.Ghost.ghost scope.Ghost.ghost r s ok after d x (refine_ u) in refine_ u) in
            ghost_ (let u = () in unified_frame h.Ghost.ghost r s ok after d r (refine_ u); unified_frame h.Ghost.ghost r s ok after d s (refine_ u);
              unified_active h.Ghost.ghost r s ok after d r (refine_ u); unified_active h.Ghost.ghost r s ok after d s (refine_ u); ());
            let t : {t : node Pref.token | Pref.own t === after && unified h.Ghost.ghost r s true after d
              && H.mem after r && H.mem after s && active after r && active after s} = refine_ t in
            let before_witness7 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
            let h_witness8 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (after)} in
            let d_witness9 : (derivation) Ghost.t = {Ghost.ghost = ghost_ (d)} in
            let trees_witness10 : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h_witness8.Ghost.ghost x then finite h_witness8.Ghost.ghost t else observe h_witness8.Ghost.ghost x === None)} @ immutable)) Ghost.t = {Ghost.ghost = ghost_ (refine_ trees_after)} in
            let scope_witness11 : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h_witness8.Ghost.ghost x) || finite_scope h_witness8.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope_after)} in
            let refine_ state_argument12 = t in
            let refine_ linked = Optimized_link.finish before_witness7 r s h_witness8 d_witness9 trees_witness10 scope_witness11 (refine_ state_argument12) in finish_roots (refine_ linked)) in
          unify_work goal h_witness31 scope_witness32 unmarked_witness33 order_witness34 trees_witness35 b e (refine_ state_argument36) resume_right
        else
          let ok = false in
          let d = ghost_ (Children (a, b, c, e, middle, left_ok, ld, Base Same)) in
          ghost_ (unified_def h.Ghost.ghost r s ok middle d);
          finish_roots (refine_ #{ok; state = t; derivation = d}) in
        unify_work goal h_witness25 scope_witness26 unmarked_witness27 order_witness28 trees_witness29 a c (refine_ state_argument30) resume_left
      | Bool, (Word | Arrow _ | List _)
      | Word, (Bool | Arrow _ | List _)
      | Arrow _, (Bool | Word | List _)
      | List _, (Bool | Word | Arrow _) ->
        let ok = false in
        let old = ghost_ Level_unifier_spec.Clash in
      ghost_ (let ok = false in Level_unifier_spec.unified_def h.Ghost.ghost r s ok h.Ghost.ghost old; ());
      let d = ghost_ (Base old) in
        ghost_ (unified_def h.Ghost.ghost r s ok h.Ghost.ghost d);
        finish_roots (refine_ #{ok; state = t; derivation = d})
      | Link _, _ | _, Link _ ->
        let proof = ghost_ (let u = () in (refine_ u : {u : unit | false})) in
        let refine_ proof = proof in assert false

and unify_work : (goal : unify_goal) @ immutable -> (h : (node Pref.heap) Ghost.t) @ immutable  ->(scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h.Ghost.ghost x) || finite_scope h.Ghost.ghost x})) Ghost.t) @ total  ->(unmarked : (((x : node Pref.t) @ immutable -> {u : unit | match H.at h.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t) @ total  ->(order : (((x : node Pref.t) @ immutable -> {u : unit | ordered h.Ghost.ghost x})) Ghost.t) @ total  ->(trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total  ->
    (p : node Pref.t) @ immutable  -> (q : node Pref.t) @ immutable  ->
    (state : {t : node Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p && H.mem h.Ghost.ghost q && active h.Ghost.ghost p && active h.Ghost.ghost q}) @ unique  ->
    (use : ((answer : {out : result | unified h.Ghost.ghost p q out.#ok (Pref.own out.#state) out.#derivation}) @ unique -> {answer : result | unified goal.heap goal.left goal.right answer.#ok (Pref.own answer.#state) answer.#derivation} @ unique)) ->
  {answer : result | unified goal.heap goal.left goal.right answer.#ok (Pref.own answer.#state) answer.#derivation} @ unique = fun goal h scope unmarked order trees p q state use ->
    let refine_ state = state in
    let state : {t : node Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p && active h.Ghost.ghost p} = refine_ state in
    let h_witness1 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
    let scope_witness2 : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h_witness1.Ghost.ghost x) || finite_scope h_witness1.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
    let refine_ state_argument3 = state in
    let refine_ first = Compressed_representative.representative h_witness1 scope_witness2 p (refine_ state_argument3) in
    let h1 = ghost_ (Pref.own (borrow_ first.#state)) in let edits1 = ghost_ first.#edits in
    let scope1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || finite_scope h1 x}) @ total ghost = ghost_ (fun x ->
      let u = () in let refine_ u = Compression_proofs.scope h.Ghost.ghost h1 edits1 scope.Ghost.ghost x (refine_ u) in refine_ u) in
    ghost_ (let u = () in Compression_proofs.frame h.Ghost.ghost h1 edits1 q (refine_ u);
      scratch_frame_def h.Ghost.ghost h1 q; active_def h.Ghost.ghost q; active_def h1 q; at_level_def h.Ghost.ghost q; at_level_def h1 q; ());
    let state : {t : node Pref.token | Pref.own t === h1 && H.mem h1 q && active h1 q} = refine_ first.#state in
    let h_witness4 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h1)} in
    let scope_witness5 : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h_witness4.Ghost.ghost x) || finite_scope h_witness4.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope1)} in
    let refine_ state_argument6 = state in
    let refine_ second = Compressed_representative.representative h_witness4 scope_witness5 q (refine_ state_argument6) in
    let h2 = ghost_ (Pref.own (borrow_ second.#state)) in let edits2 = ghost_ second.#edits in
    let scope2 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h2 x) || finite_scope h2 x}) @ total ghost = ghost_ (fun x ->
      let u = () in let refine_ u = Compression_proofs.scope h1 h2 edits2 scope1 x (refine_ u) in refine_ u) in
    let unmarked2 : ((x : node Pref.t) @ immutable -> {u : unit | match H.at h2 x with None -> true | Some v -> not v.visited}) @ total ghost = ghost_ (fun x ->
      unmarked.Ghost.ghost x; let u = () in Compression_proofs.frame h.Ghost.ghost h1 edits1 x (refine_ u); Compression_proofs.frame h1 h2 edits2 x (refine_ u);
      scratch_frame_def h.Ghost.ghost h1 x; scratch_frame_def h1 h2 x; refine_ u) in
    let trees2 : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h2 x then finite h2 t else observe h2 x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
        let refine_ tree = trees.Ghost.ghost x in let u = () in
        Compression_proofs.frame h.Ghost.ghost h1 edits1 x (refine_ u); Compression_proofs.frame h1 h2 edits2 x (refine_ u);
        if H.mem h.Ghost.ghost x then (
          let refine_ tree1 = Compression_proofs.finite h.Ghost.ghost h1 edits1 tree (refine_ u) in
          let refine_ tree2 = Compression_proofs.finite h1 h2 edits2 tree1 (refine_ u) in refine_ tree2)
        else (scratch_frame_def h.Ghost.ghost h1 x; scratch_frame_def h1 h2 x;
          observe_def h.Ghost.ghost x; observe_def h1 x; observe_def h2 x; refine_ tree)) in
    ghost_ (let u = () in Compression_proofs.frame h.Ghost.ghost h1 edits1 p (refine_ u); Compression_proofs.frame h1 h2 edits2 p (refine_ u);
      Compression_proofs.frame h1 h2 edits2 q (refine_ u);
      active_def h.Ghost.ghost p; active_def h1 p; active_def h2 p; active_def h2 q;
      at_level_def h.Ghost.ghost p; at_level_def h1 p; at_level_def h2 p;
      at_level_def h1 q; at_level_def h2 q; ());
    let state : {t : node Pref.token | Pref.own t === h2 && H.mem h2 p && H.mem h2 q && active h2 p && active h2 q} = refine_ second.#state in
    let order1 : ((x : node Pref.t) @ immutable -> {u : unit | ordered h1 x}) @ total ghost = ghost_ (fun x ->
      let u = () in let refine_ u = Compression_proofs.ordered h.Ghost.ghost h1 edits1 order.Ghost.ghost x (refine_ u) in refine_ u) in
    let order2 : ((x : node Pref.t) @ immutable -> {u : unit | ordered h2 x}) @ total ghost = ghost_ (fun x ->
      let u = () in let refine_ u = Compression_proofs.ordered h1 h2 edits2 order1 x (refine_ u) in refine_ u) in
    let r = first.#value in let s = second.#value in
    let rp : {out : resolved | H.mem h2 out.#value && active h2 out.#value && terminal h2 out.#value
      && resolves h2 p out.#value out.#path} =
      let path : {d : resolution | resolves h2 p r d} @ immutable ghost = ghost_ (let u = () in
        let refine_ path1 = Compression_proofs.resolution h.Ghost.ghost h1 edits1 p r first.#path (refine_ u) in
        let refine_ path2 = Compression_proofs.resolution h1 h2 edits2 p r path1 (refine_ u) in refine_ path2) in
      let refine_ path = path in
      ghost_ (let u = () in Compression_proofs.frame h.Ghost.ghost h1 edits1 r (refine_ u);
        Compression_proofs.frame h1 h2 edits2 r (refine_ u);
        active_def h.Ghost.ghost r; active_def h1 r; active_def h2 r;
        Compression_path_proofs.resolution_terminal h2 p r path (refine_ u); ());
      let out = #{value = r; path} in refine_ out in
    let sq : {out : resolved | H.mem h2 out.#value && active h2 out.#value && terminal h2 out.#value
      && resolves h2 q out.#value out.#path} =
      let path : {d : resolution | resolves h2 q s d} @ immutable ghost = ghost_ (let u = () in
        let refine_ path = Compression_proofs.resolution h1 h2 edits2 q s second.#path (refine_ u) in refine_ path) in
      let refine_ path = path in let out = #{value = s; path} in refine_ out in
    let refine_ rp = rp in let refine_ sq = sq in
    let h_witness37 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h2)} in
    let scope_witness38 : (((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness37.Ghost.ghost q) || finite_scope h_witness37.Ghost.ghost q})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope2)} in
    let unmarked_witness39 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h_witness37.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t = {Ghost.ghost = ghost_ (refine_ unmarked2)} in
    let order_witness40 : (((x : node Pref.t) @ immutable -> {u : unit | ordered h_witness37.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ order2)} in
    let trees_witness41 : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h_witness37.Ghost.ghost x then finite h_witness37.Ghost.ghost t else observe h_witness37.Ghost.ghost x === None)} @ immutable)) Ghost.t = {Ghost.ghost = ghost_ (refine_ trees2)} in
    let rp_argument42 : {r : resolved | H.mem h_witness37.Ghost.ghost r.#value && active h_witness37.Ghost.ghost r.#value && terminal h_witness37.Ghost.ghost r.#value
      && resolves h_witness37.Ghost.ghost p r.#value r.#path} = refine_ rp in
    let sq_argument43 : {r : resolved | H.mem h_witness37.Ghost.ghost r.#value && active h_witness37.Ghost.ghost r.#value && terminal h_witness37.Ghost.ghost r.#value
      && resolves h_witness37.Ghost.ghost q r.#value r.#path} = refine_ sq in
    let refine_ state_argument44 = state in
    let resume_raw : (out : {r : result | unified h_witness37.Ghost.ghost p q r.#ok (Pref.own r.#state) r.#derivation}) @ unique -> {answer : result | unified goal.heap goal.left goal.right answer.#ok (Pref.own answer.#state) answer.#derivation} @ unique = fun out ->
    let refine_ out = out in
    let after = ghost_ (Pref.own (borrow_ out.#state)) in let ok = out.#ok in
    let d2 = ghost_ (Pre_compress (h2, edits2, out.#derivation)) in
    let d1 = ghost_ (Pre_compress (h1, edits1, d2)) in
    ghost_ (unified_def h1 p q ok after d2; unified_def h.Ghost.ghost p q ok after d1);
    let result = #{ok; state = out.#state; derivation = d1} in use (refine_ result)
 in
    raw_work goal h_witness37 scope_witness38 unmarked_witness39 order_witness40 trees_witness41 p q rp_argument42 sq_argument43 (refine_ state_argument44) resume_raw

let raw :
    (h : (node Pref.heap) Ghost.t) @ immutable  ->(scope : (((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h.Ghost.ghost q) || finite_scope h.Ghost.ghost q})) Ghost.t) @ total  ->(unmarked : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t) @ total  ->(order : (((x : node Pref.t) @ immutable -> {u : unit | ordered h.Ghost.ghost x})) Ghost.t) @ total  ->(trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total  ->
    (p : node Pref.t) @ immutable  -> (q : node Pref.t) @ immutable  ->
    (rp : {r : resolved | H.mem h.Ghost.ghost r.#value && active h.Ghost.ghost r.#value && terminal h.Ghost.ghost r.#value
      && resolves h.Ghost.ghost p r.#value r.#path})  ->
    (sq : {r : resolved | H.mem h.Ghost.ghost r.#value && active h.Ghost.ghost r.#value && terminal h.Ghost.ghost r.#value
      && resolves h.Ghost.ghost q r.#value r.#path})  ->
    (t : {t : node Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p && H.mem h.Ghost.ghost q && active h.Ghost.ghost p && active h.Ghost.ghost q}) @ unique  ->
    {r : result | unified h.Ghost.ghost p q r.#ok (Pref.own r.#state) r.#derivation} @ unique  = fun h scope unmarked order trees p q rp sq t ->
  let goal = {heap = h.Ghost.ghost; left = p; right = q} in
  let use : (answer : {r : result | unified h.Ghost.ghost p q r.#ok (Pref.own r.#state) r.#derivation}) @ unique -> {answer : result | unified goal.heap goal.left goal.right answer.#ok (Pref.own answer.#state) answer.#derivation} @ unique = fun answer ->
    let refine_ answer = answer in refine_ answer in
  let refine_ answer = raw_work goal h scope unmarked order trees p q rp sq t use in refine_ answer

let unify : (h : (node Pref.heap) Ghost.t) @ immutable  ->(scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h.Ghost.ghost x) || finite_scope h.Ghost.ghost x})) Ghost.t) @ total  ->(unmarked : (((x : node Pref.t) @ immutable -> {u : unit | match H.at h.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t) @ total  ->(order : (((x : node Pref.t) @ immutable -> {u : unit | ordered h.Ghost.ghost x})) Ghost.t) @ total  ->(trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total  ->
    (p : node Pref.t) @ immutable  -> (q : node Pref.t) @ immutable  ->
    (state : {t : node Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p && H.mem h.Ghost.ghost q && active h.Ghost.ghost p && active h.Ghost.ghost q}) @ unique  ->
    {out : result | unified h.Ghost.ghost p q out.#ok (Pref.own out.#state) out.#derivation} @ unique  = fun h scope unmarked order trees p q state ->
  let goal = {heap = h.Ghost.ghost; left = p; right = q} in
  let use : (answer : {out : result | unified h.Ghost.ghost p q out.#ok (Pref.own out.#state) out.#derivation}) @ unique -> {answer : result | unified goal.heap goal.left goal.right answer.#ok (Pref.own answer.#state) answer.#derivation} @ unique = fun answer ->
    let refine_ answer = answer in refine_ answer in
  let refine_ answer = unify_work goal h scope unmarked order trees p q state use in refine_ answer
