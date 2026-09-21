open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec
open Optimized_unifier_spec
open Level_unifier_metadata
open Optimized_metadata
type unify_goal = {heap : Pref.heap @@ ghost; left : node Pref.t @@ ghost; right : node Pref.t @@ ghost}

let rec raw_work : (goal : unify_goal) @ immutable ->
    (h : (Pref.heap) Ghost.t) @ immutable  ->(scope : (((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h.Ghost.ghost q) || finite_scope h.Ghost.ghost q})) Ghost.t) @ total  ->(unmarked : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t) @ total  ->(order : (((x : node Pref.t) @ immutable -> {u : unit | ordered h.Ghost.ghost x})) Ghost.t) @ total  ->(trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total  ->
    (p : node Pref.t) @ immutable  -> (q : node Pref.t) @ immutable  ->
    (rp : {r : resolved | H.mem h.Ghost.ghost r.#value && active h.Ghost.ghost r.#value && terminal h.Ghost.ghost r.#value
      && resolves h.Ghost.ghost p r.#value r.#path})  ->
    (sq : {r : resolved | H.mem h.Ghost.ghost r.#value && active h.Ghost.ghost r.#value && terminal h.Ghost.ghost r.#value
      && resolves h.Ghost.ghost q r.#value r.#path})  ->
    (t : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p && H.mem h.Ghost.ghost q && active h.Ghost.ghost p && active h.Ghost.ghost q}) @ unique  ->
    (use : ((answer : {r : result | unified h.Ghost.ghost p q r.#ok (Pref.own r.#state) r.#derivation}) @ unique -> {answer : result | unified goal.heap goal.left goal.right answer.#ok (Pref.own answer.#state) answer.#derivation} @ unique)) ->
  {answer : result | unified goal.heap goal.left goal.right answer.#ok (Pref.own answer.#state) answer.#derivation} @ unique = fun goal h scope unmarked order trees p q rp sq t use ->
  let r = rp.#value in
  let s = sq.#value in
  let finish_roots : (result : {answer : result |
      unified h.Ghost.ghost r s answer.#ok (Pref.own answer.#state) answer.#derivation}) @ unique -> {answer : result | unified goal.heap goal.left goal.right answer.#ok (Pref.own answer.#state) answer.#derivation} @ unique = fun result ->
let ok = result.#ok in
  let t = result.#state in
  let after = ghost_ (Pref.own (borrow_ t)) in
  let d = ghost_ (Resolve (r, s, rp.#path, sq.#path, result.#derivation)) in
  ghost_ (unified_def h.Ghost.ghost p q ok after d);
  let answer = #{ok; state = t; derivation = d} in
  use (answer) in
    let equal = Pref.equal r s in
    if equal then
      let old = ghost_ Level_unifier_spec.Same in
      ghost_ (let ok = true in Level_unifier_spec.unified_def h.Ghost.ghost r s ok h.Ghost.ghost old; ());
      let d = ghost_ (Base old) in
      let ok = true in
      ghost_ (unified_def h.Ghost.ghost r s ok h.Ghost.ghost d);
      finish_roots (#{ok; state = t; derivation = d})
    else
      let n : {n : desc | Some n === observe h.Ghost.ghost r} =
        let b = borrow_ t in
        let b : {b : Pref.token | H.mem (Pref.own b) r} = b in
        let old = Pref.read r b in ghost_ (observe_def h.Ghost.ghost r);
        let n = old.desc in n in
      let m : {m : desc | Some m === observe h.Ghost.ghost s} =
        let b = borrow_ t in
        let b : {b : Pref.token | H.mem (Pref.own b) s} = b in
        let old = Pref.read s b in ghost_ (observe_def h.Ghost.ghost s);
        let m = old.desc in m in
      ghost_ (terminal_def h.Ghost.ghost r);
      ghost_ (terminal_def h.Ghost.ghost s);
      match n, m with
      | Var, _ ->
        let t : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost r && H.mem h.Ghost.ghost s && active h.Ghost.ghost r && active h.Ghost.ghost s
          && observe h.Ghost.ghost r === Some Var && terminal h.Ghost.ghost s && not (r === s)} = t in
        let h_witness13 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
        let scope_witness14 : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness13.Ghost.ghost x) || finite_scope h_witness13.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
        let unmarked_witness15 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h_witness13.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t = {Ghost.ghost = ghost_ (refine_ unmarked.Ghost.ghost)} in
        let order_witness16 : (((x : node Pref.t) @ immutable -> {u : unit | ordered h_witness13.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ order.Ghost.ghost)} in
        let trees_witness17 : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h_witness13.Ghost.ghost x then finite h_witness13.Ghost.ghost t else observe h_witness13.Ghost.ghost x === None)} @ immutable)) Ghost.t = {Ghost.ghost = ghost_ (refine_ trees.Ghost.ghost)} in
        let state_argument18 = t in
        let answer = Pruned_bind.bind h_witness13 scope_witness14 unmarked_witness15 order_witness16 trees_witness17 r s (state_argument18) in
        let after = ghost_ (Pref.own (borrow_ answer.#state)) in
        let d = ghost_ (Base answer.#derivation) in let ok = answer.#ok in
        ghost_ (unified_def h.Ghost.ghost r s ok after d);
        finish_roots (#{ok; state = answer.#state; derivation = d})
      | _, Var ->
        let t : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost s && H.mem h.Ghost.ghost r && active h.Ghost.ghost s && active h.Ghost.ghost r
          && observe h.Ghost.ghost s === Some Var && terminal h.Ghost.ghost r && not (s === r)} = t in
        let h_witness19 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
        let scope_witness20 : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness19.Ghost.ghost x) || finite_scope h_witness19.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
        let unmarked_witness21 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h_witness19.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t = {Ghost.ghost = ghost_ (refine_ unmarked.Ghost.ghost)} in
        let order_witness22 : (((x : node Pref.t) @ immutable -> {u : unit | ordered h_witness19.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ order.Ghost.ghost)} in
        let trees_witness23 : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h_witness19.Ghost.ghost x then finite h_witness19.Ghost.ghost t else observe h_witness19.Ghost.ghost x === None)} @ immutable)) Ghost.t = {Ghost.ghost = ghost_ (refine_ trees.Ghost.ghost)} in
        let state_argument24 = t in
        let answer = Pruned_bind.bind h_witness19 scope_witness20 unmarked_witness21 order_witness22 trees_witness23 s r (state_argument24) in
        let ok = answer.#ok in
        let t = answer.#state in
        let after = ghost_ (Pref.own (borrow_ t)) in
        let old = ghost_ (Level_unifier_spec.Swap answer.#derivation) in
        ghost_ (Level_unifier_spec.unified_def h.Ghost.ghost r s ok after old);
        let d = ghost_ (Base old) in
        ghost_ (unified_def h.Ghost.ghost r s ok after d);
        finish_roots (#{ok; state = t; derivation = d})
      | Bool, Bool ->
        let ok = true in
        let old = ghost_ Level_unifier_spec.Constants in
      ghost_ (let ok = true in Level_unifier_spec.unified_def h.Ghost.ghost r s ok h.Ghost.ghost old; ());
      let d = ghost_ (Base old) in
        ghost_ (unified_def h.Ghost.ghost r s ok h.Ghost.ghost d);
        finish_roots (#{ok; state = t; derivation = d})
      | Arrow (a, b), Arrow (c, e) ->
        ghost_ (scope.Ghost.ghost r);
        ghost_ (scope.Ghost.ghost s);
        ghost_ (finite_scope_def h.Ghost.ghost r; source_ok_def h.Ghost.ghost r; observe_def h.Ghost.ghost r);
        ghost_ (finite_scope_def h.Ghost.ghost s; source_ok_def h.Ghost.ghost s; observe_def h.Ghost.ghost s);
        let t : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost a && H.mem h.Ghost.ghost c && active h.Ghost.ghost a && active h.Ghost.ghost c} = t in
        let h_witness25 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
        let scope_witness26 : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h_witness25.Ghost.ghost x) || finite_scope h_witness25.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
        let unmarked_witness27 : (((x : node Pref.t) @ immutable -> {u : unit | match H.at h_witness25.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t = {Ghost.ghost = ghost_ (refine_ unmarked.Ghost.ghost)} in
        let order_witness28 : (((x : node Pref.t) @ immutable -> {u : unit | ordered h_witness25.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ order.Ghost.ghost)} in
        let trees_witness29 : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h_witness25.Ghost.ghost x then finite h_witness25.Ghost.ghost t else observe h_witness25.Ghost.ghost x === None)} @ immutable)) Ghost.t = {Ghost.ghost = ghost_ (refine_ trees.Ghost.ghost)} in
        let state_argument30 = t in
        let resume_left : (left : {out : result | unified h_witness25.Ghost.ghost a c out.#ok (Pref.own out.#state) out.#derivation}) @ unique -> {answer : result | unified goal.heap goal.left goal.right answer.#ok (Pref.own answer.#state) answer.#derivation} @ unique = fun left ->
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
              unified_scope h.Ghost.ghost scope.Ghost.ghost a c left_ok middle ld x (u);
              u) in
          let _proof = ghost_ (
            let u = () in
            unified_frame h.Ghost.ghost a c left_ok middle ld b (u);
            unified_frame h.Ghost.ghost a c left_ok middle ld e (u);
            unified_active h.Ghost.ghost a c left_ok middle ld b (u);
            unified_active h.Ghost.ghost a c left_ok middle ld e (u);
            let proof : {u : unit | H.mem middle b && H.mem middle e && active middle b && active middle e} = u in proof) in
          let t : {t : Pref.token | Pref.own t === middle && H.mem middle b && H.mem middle e && active middle b && active middle e} = t in
          let unmarked_middle : ((x : node Pref.t) @ immutable ->
            {u : unit | match H.at middle x with None -> true | Some v -> not v.visited}) @ total ghost = ghost_ (fun x ->
            unmarked.Ghost.ghost x; let u = () in unified_scratch h.Ghost.ghost a c left_ok middle ld x (u);
            scratch_frame_def h.Ghost.ghost middle x; u) in
          let trees_middle : ((x : node Pref.t) @ immutable ->
            {t : tree | tree_root t === x && (if H.mem middle x then finite middle t else observe middle x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
              let u = () in let tree = Optimized_finite_proofs.unified_finite_at h.Ghost.ghost trees.Ghost.ghost a c left_ok middle ld x (u) in tree) in
          let order_middle : ((x : node Pref.t) @ immutable -> {u : unit | ordered middle x}) @ total ghost = ghost_ (fun x ->
            let u = () in let u = unified_ordered h.Ghost.ghost order.Ghost.ghost a c left_ok middle ld x (u) in u) in
          let h_witness31 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (middle)} in
          let scope_witness32 : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h_witness31.Ghost.ghost x) || finite_scope h_witness31.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (scope_middle)} in
          let unmarked_witness33 : (((x : node Pref.t) @ immutable -> {u : unit | match H.at h_witness31.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t = {Ghost.ghost = ghost_ (unmarked_middle)} in
          let order_witness34 : (((x : node Pref.t) @ immutable -> {u : unit | ordered h_witness31.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (order_middle)} in
          let trees_witness35 : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h_witness31.Ghost.ghost x then finite h_witness31.Ghost.ghost t else observe h_witness31.Ghost.ghost x === None)} @ immutable)) Ghost.t = {Ghost.ghost = ghost_ (trees_middle)} in
          let state_argument36 = t in
          let resume_right : (right : {out : result | unified h_witness31.Ghost.ghost b e out.#ok (Pref.own out.#state) out.#derivation}) @ unique -> {answer : result | unified goal.heap goal.left goal.right answer.#ok (Pref.own answer.#state) answer.#derivation} @ unique = fun right ->
          let ok = right.#ok in
          let t = right.#state in
          let after = ghost_ (Pref.own (borrow_ t)) in
          let d = ghost_ (Children (a, b, c, e, middle, left_ok, ld, right.#derivation)) in
          ghost_ (unified_def h.Ghost.ghost r s ok after d);
          if not ok then finish_roots (#{ok; state = t; derivation = d}) else (
            let trees_after : ((x : node Pref.t) @ immutable ->
              {t : tree | tree_root t === x && (if H.mem after x then finite after t else observe after x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
                let u = () in let tree = Optimized_finite_proofs.unified_finite_at h.Ghost.ghost trees.Ghost.ghost r s ok after d x (u) in tree) in
            let scope_after : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem after x) || finite_scope after x}) @ total ghost = ghost_ (fun x ->
              let u = () in let u = unified_scope h.Ghost.ghost scope.Ghost.ghost r s ok after d x (u) in u) in
            ghost_ (let u = () in unified_frame h.Ghost.ghost r s ok after d r (u); unified_frame h.Ghost.ghost r s ok after d s (u);
              unified_active h.Ghost.ghost r s ok after d r (u); unified_active h.Ghost.ghost r s ok after d s (u); ());
            let t : {t : Pref.token | Pref.own t === after && unified h.Ghost.ghost r s true after d
              && H.mem after r && H.mem after s && active after r && active after s} = t in
            let before_witness7 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
            let h_witness8 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (after)} in
            let d_witness9 : (derivation) Ghost.t = {Ghost.ghost = ghost_ (d)} in
            let trees_witness10 : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h_witness8.Ghost.ghost x then finite h_witness8.Ghost.ghost t else observe h_witness8.Ghost.ghost x === None)} @ immutable)) Ghost.t = {Ghost.ghost = ghost_ (trees_after)} in
            let scope_witness11 : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h_witness8.Ghost.ghost x) || finite_scope h_witness8.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (scope_after)} in
            let state_argument12 = t in
            let linked = Optimized_link.finish before_witness7 r s h_witness8 d_witness9 trees_witness10 scope_witness11 (state_argument12) in finish_roots (linked)) in
          unify_work goal h_witness31 scope_witness32 unmarked_witness33 order_witness34 trees_witness35 b e (state_argument36) resume_right
        else
          let ok = false in
          let d = ghost_ (Children (a, b, c, e, middle, left_ok, ld, Base Same)) in
          ghost_ (unified_def h.Ghost.ghost r s ok middle d);
          finish_roots (#{ok; state = t; derivation = d}) in
        unify_work goal h_witness25 scope_witness26 unmarked_witness27 order_witness28 trees_witness29 a c (state_argument30) resume_left
      | Bool, Arrow _ | Arrow _, Bool ->
        let ok = false in
        let old = ghost_ Level_unifier_spec.Clash in
      ghost_ (let ok = false in Level_unifier_spec.unified_def h.Ghost.ghost r s ok h.Ghost.ghost old; ());
      let d = ghost_ (Base old) in
        ghost_ (unified_def h.Ghost.ghost r s ok h.Ghost.ghost d);
        finish_roots (#{ok; state = t; derivation = d})
      | Link _, _ | _, Link _ ->
        let _proof = ghost_ (let u = () in (u : {u : unit | false})) in
        assert false

and unify_work : (goal : unify_goal) @ immutable -> (h : (Pref.heap) Ghost.t) @ immutable  ->(scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h.Ghost.ghost x) || finite_scope h.Ghost.ghost x})) Ghost.t) @ total  ->(unmarked : (((x : node Pref.t) @ immutable -> {u : unit | match H.at h.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t) @ total  ->(order : (((x : node Pref.t) @ immutable -> {u : unit | ordered h.Ghost.ghost x})) Ghost.t) @ total  ->(trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total  ->
    (p : node Pref.t) @ immutable  -> (q : node Pref.t) @ immutable  ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p && H.mem h.Ghost.ghost q && active h.Ghost.ghost p && active h.Ghost.ghost q}) @ unique  ->
    (use : ((answer : {out : result | unified h.Ghost.ghost p q out.#ok (Pref.own out.#state) out.#derivation}) @ unique -> {answer : result | unified goal.heap goal.left goal.right answer.#ok (Pref.own answer.#state) answer.#derivation} @ unique)) ->
  {answer : result | unified goal.heap goal.left goal.right answer.#ok (Pref.own answer.#state) answer.#derivation} @ unique = fun goal h scope unmarked order trees p q state use ->
    let state : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p && active h.Ghost.ghost p} = state in
    let h_witness1 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
    let scope_witness2 : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h_witness1.Ghost.ghost x) || finite_scope h_witness1.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
    let state_argument3 = state in
    let first = Compressed_representative.representative h_witness1 scope_witness2 p (state_argument3) in
    let h1 = ghost_ (Pref.own (borrow_ first.#state)) in let edits1 = ghost_ first.#edits in
    let scope1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || finite_scope h1 x}) @ total ghost = ghost_ (fun x ->
      let u = () in let u = Compression_proofs.scope h.Ghost.ghost h1 edits1 scope.Ghost.ghost x (u) in u) in
    ghost_ (let u = () in Compression_proofs.frame h.Ghost.ghost h1 edits1 q (u);
      scratch_frame_def h.Ghost.ghost h1 q; active_def h.Ghost.ghost q; active_def h1 q; at_level_def h.Ghost.ghost q; at_level_def h1 q; ());
    let state : {t : Pref.token | Pref.own t === h1 && H.mem h1 q && active h1 q} = first.#state in
    let h_witness4 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h1)} in
    let scope_witness5 : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h_witness4.Ghost.ghost x) || finite_scope h_witness4.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (scope1)} in
    let state_argument6 = state in
    let second = Compressed_representative.representative h_witness4 scope_witness5 q (state_argument6) in
    let h2 = ghost_ (Pref.own (borrow_ second.#state)) in let edits2 = ghost_ second.#edits in
    let scope2 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h2 x) || finite_scope h2 x}) @ total ghost = ghost_ (fun x ->
      let u = () in let u = Compression_proofs.scope h1 h2 edits2 scope1 x (u) in u) in
    let unmarked2 : ((x : node Pref.t) @ immutable -> {u : unit | match H.at h2 x with None -> true | Some v -> not v.visited}) @ total ghost = ghost_ (fun x ->
      unmarked.Ghost.ghost x; let u = () in Compression_proofs.frame h.Ghost.ghost h1 edits1 x (u); Compression_proofs.frame h1 h2 edits2 x (u);
      scratch_frame_def h.Ghost.ghost h1 x; scratch_frame_def h1 h2 x; u) in
    let trees2 : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h2 x then finite h2 t else observe h2 x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
        let tree = trees.Ghost.ghost x in let u = () in
        Compression_proofs.frame h.Ghost.ghost h1 edits1 x (u); Compression_proofs.frame h1 h2 edits2 x (u);
        if H.mem h.Ghost.ghost x then (
          let tree1 = Compression_proofs.finite h.Ghost.ghost h1 edits1 tree (u) in
          let tree2 = Compression_proofs.finite h1 h2 edits2 tree1 (u) in tree2)
        else (scratch_frame_def h.Ghost.ghost h1 x; scratch_frame_def h1 h2 x;
          observe_def h.Ghost.ghost x; observe_def h1 x; observe_def h2 x; tree)) in
    ghost_ (let u = () in Compression_proofs.frame h.Ghost.ghost h1 edits1 p (u); Compression_proofs.frame h1 h2 edits2 p (u);
      Compression_proofs.frame h1 h2 edits2 q (u);
      active_def h.Ghost.ghost p; active_def h1 p; active_def h2 p; active_def h2 q;
      at_level_def h.Ghost.ghost p; at_level_def h1 p; at_level_def h2 p;
      at_level_def h1 q; at_level_def h2 q; ());
    let state : {t : Pref.token | Pref.own t === h2 && H.mem h2 p && H.mem h2 q && active h2 p && active h2 q} = second.#state in
    let order1 : ((x : node Pref.t) @ immutable -> {u : unit | ordered h1 x}) @ total ghost = ghost_ (fun x ->
      let u = () in let u = Compression_proofs.ordered h.Ghost.ghost h1 edits1 order.Ghost.ghost x (u) in u) in
    let order2 : ((x : node Pref.t) @ immutable -> {u : unit | ordered h2 x}) @ total ghost = ghost_ (fun x ->
      let u = () in let u = Compression_proofs.ordered h1 h2 edits2 order1 x (u) in u) in
    let r = first.#value in let s = second.#value in
    let rp : {out : resolved | H.mem h2 out.#value && active h2 out.#value && terminal h2 out.#value
      && resolves h2 p out.#value out.#path} =
      let path : {d : resolution | resolves h2 p r d} @ immutable ghost = ghost_ (let u = () in
        let path1 = Compression_proofs.resolution h.Ghost.ghost h1 edits1 p r first.#path (u) in
        let path2 = Compression_proofs.resolution h1 h2 edits2 p r path1 (u) in path2) in
      ghost_ (let u = () in Compression_proofs.frame h.Ghost.ghost h1 edits1 r (u);
        Compression_proofs.frame h1 h2 edits2 r (u);
        active_def h.Ghost.ghost r; active_def h1 r; active_def h2 r;
        Compression_path_proofs.resolution_terminal h2 p r path (u); ());
      let out = #{value = r; path} in out in
    let sq : {out : resolved | H.mem h2 out.#value && active h2 out.#value && terminal h2 out.#value
      && resolves h2 q out.#value out.#path} =
      let path : {d : resolution | resolves h2 q s d} @ immutable ghost = ghost_ (let u = () in
        let path = Compression_proofs.resolution h1 h2 edits2 q s second.#path (u) in path) in
      let out = #{value = s; path} in out in
    let h_witness37 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h2)} in
    let scope_witness38 : (((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness37.Ghost.ghost q) || finite_scope h_witness37.Ghost.ghost q})) Ghost.t = {Ghost.ghost = ghost_ (scope2)} in
    let unmarked_witness39 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h_witness37.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t = {Ghost.ghost = ghost_ (unmarked2)} in
    let order_witness40 : (((x : node Pref.t) @ immutable -> {u : unit | ordered h_witness37.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (order2)} in
    let trees_witness41 : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h_witness37.Ghost.ghost x then finite h_witness37.Ghost.ghost t else observe h_witness37.Ghost.ghost x === None)} @ immutable)) Ghost.t = {Ghost.ghost = ghost_ (trees2)} in
    let rp_argument42 : {r : resolved | H.mem h_witness37.Ghost.ghost r.#value && active h_witness37.Ghost.ghost r.#value && terminal h_witness37.Ghost.ghost r.#value
      && resolves h_witness37.Ghost.ghost p r.#value r.#path} = rp in
    let sq_argument43 : {r : resolved | H.mem h_witness37.Ghost.ghost r.#value && active h_witness37.Ghost.ghost r.#value && terminal h_witness37.Ghost.ghost r.#value
      && resolves h_witness37.Ghost.ghost q r.#value r.#path} = sq in
    let state_argument44 = state in
    let resume_raw : (out : {r : result | unified h_witness37.Ghost.ghost p q r.#ok (Pref.own r.#state) r.#derivation}) @ unique -> {answer : result | unified goal.heap goal.left goal.right answer.#ok (Pref.own answer.#state) answer.#derivation} @ unique = fun out ->
    let after = ghost_ (Pref.own (borrow_ out.#state)) in let ok = out.#ok in
    let d2 = ghost_ (Pre_compress (h2, edits2, out.#derivation)) in
    let d1 = ghost_ (Pre_compress (h1, edits1, d2)) in
    ghost_ (unified_def h1 p q ok after d2; unified_def h.Ghost.ghost p q ok after d1);
    let result = #{ok; state = out.#state; derivation = d1} in use (result)
 in
    raw_work goal h_witness37 scope_witness38 unmarked_witness39 order_witness40 trees_witness41 p q rp_argument42 sq_argument43 (state_argument44) resume_raw

let raw :
    (h : (Pref.heap) Ghost.t) @ immutable  ->(scope : (((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h.Ghost.ghost q) || finite_scope h.Ghost.ghost q})) Ghost.t) @ total  ->(unmarked : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t) @ total  ->(order : (((x : node Pref.t) @ immutable -> {u : unit | ordered h.Ghost.ghost x})) Ghost.t) @ total  ->(trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total  ->
    (p : node Pref.t) @ immutable  -> (q : node Pref.t) @ immutable  ->
    (rp : {r : resolved | H.mem h.Ghost.ghost r.#value && active h.Ghost.ghost r.#value && terminal h.Ghost.ghost r.#value
      && resolves h.Ghost.ghost p r.#value r.#path})  ->
    (sq : {r : resolved | H.mem h.Ghost.ghost r.#value && active h.Ghost.ghost r.#value && terminal h.Ghost.ghost r.#value
      && resolves h.Ghost.ghost q r.#value r.#path})  ->
    (t : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p && H.mem h.Ghost.ghost q && active h.Ghost.ghost p && active h.Ghost.ghost q}) @ unique  ->
    {r : result | unified h.Ghost.ghost p q r.#ok (Pref.own r.#state) r.#derivation} @ unique  = fun h scope unmarked order trees p q rp sq t ->
  let goal = {heap = h.Ghost.ghost; left = p; right = q} in
  let use : (answer : {r : result | unified h.Ghost.ghost p q r.#ok (Pref.own r.#state) r.#derivation}) @ unique -> {answer : result | unified goal.heap goal.left goal.right answer.#ok (Pref.own answer.#state) answer.#derivation} @ unique = fun answer ->
    answer in
  let answer = raw_work goal h scope unmarked order trees p q rp sq t use in answer

let unify : (h : (Pref.heap) Ghost.t) @ immutable  ->(scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h.Ghost.ghost x) || finite_scope h.Ghost.ghost x})) Ghost.t) @ total  ->(unmarked : (((x : node Pref.t) @ immutable -> {u : unit | match H.at h.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t) @ total  ->(order : (((x : node Pref.t) @ immutable -> {u : unit | ordered h.Ghost.ghost x})) Ghost.t) @ total  ->(trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total  ->
    (p : node Pref.t) @ immutable  -> (q : node Pref.t) @ immutable  ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p && H.mem h.Ghost.ghost q && active h.Ghost.ghost p && active h.Ghost.ghost q}) @ unique  ->
    {out : result | unified h.Ghost.ghost p q out.#ok (Pref.own out.#state) out.#derivation} @ unique  = fun h scope unmarked order trees p q state ->
  let goal = {heap = h.Ghost.ghost; left = p; right = q} in
  let use : (answer : {out : result | unified h.Ghost.ghost p q out.#ok (Pref.own out.#state) out.#derivation}) @ unique -> {answer : result | unified goal.heap goal.left goal.right answer.#ok (Pref.own answer.#state) answer.#derivation} @ unique = fun answer ->
    answer in
  let answer = unify_work goal h scope unmarked order trees p q state use in answer
