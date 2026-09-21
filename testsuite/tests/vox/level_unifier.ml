open Marked_occurs_proofs
open Copy_spec
open Level_unifier_spec
open Level_unifier_proofs
open Level_spec
open Level_proofs
open Level_unifier_metadata

let rec representative_loop :
    (start : (node Pref.t) Ghost.t) @ immutable ->
    (h : (Pref.heap) Ghost.t) @ immutable  ->(scope : (((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h.Ghost.ghost q) || finite_scope h.Ghost.ghost q})) Ghost.t) @ total  ->
    (p : node Pref.t) @ immutable  ->
    (t : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p && active h.Ghost.ghost p}) @ local read  ->
    (lift : (((value : node Pref.t) @ immutable ->
      (path : {d : resolution | resolves h.Ghost.ghost p value d}) @ immutable ->
      {d : resolution | resolves h.Ghost.ghost start.Ghost.ghost value d} @ immutable)) Ghost.t) @ total ->
    {r : resolved | H.mem h.Ghost.ghost r.#value && active h.Ghost.ghost r.#value && terminal h.Ghost.ghost r.#value
      && resolves h.Ghost.ghost start.Ghost.ghost r.#value r.#path} @ immutable = fun start h scope p t lift ->
    ghost_ (scope.Ghost.ghost p);
    ghost_ (finite_scope_def h.Ghost.ghost p; source_ok_def h.Ghost.ghost p; observe_def h.Ghost.ghost p);
    let t : {t : Pref.token | H.mem (Pref.own t) p} = t in
    let old = Pref.read p t in
    ghost_ (observe_def h.Ghost.ghost p);
    let n = old.desc in
    match n with
    | Link q ->
      let t : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost q && active h.Ghost.ghost q} = t in
      let h_witness5 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
      let scope_witness6 : (((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness5.Ghost.ghost q) || finite_scope h_witness5.Ghost.ghost q})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
      let state_argument8 = t in
      let next_lift : (((value : node Pref.t) @ immutable ->
        (path : {d : resolution | resolves h_witness5.Ghost.ghost q value d}) @ immutable ->
        {d : resolution | resolves h_witness5.Ghost.ghost start.Ghost.ghost value d} @ immutable)) Ghost.t =
        {Ghost.ghost = ghost_ (fun value path ->
          let joined = Via (q, path) in
          resolves_def h.Ghost.ghost p value joined;
          let joined : {d : resolution | resolves h.Ghost.ghost p value d} = joined in
          let result = lift.Ghost.ghost value joined in result)} in
      let out = representative_loop start h_witness5 scope_witness6 q (state_argument8) next_lift in out

    | Var | Bool | Arrow _ ->
      let path = ghost_ Here in
      ghost_ (terminal_def h.Ghost.ghost p);
      ghost_ (resolves_def h.Ghost.ghost p p path);
      let path : {d : resolution | resolves h.Ghost.ghost start.Ghost.ghost p d} @ immutable ghost = ghost_ (
        let path : {d : resolution | resolves h.Ghost.ghost p p d} = path in
        let out = lift.Ghost.ghost p path in out) in
      let result = #{value = p; path = path} in result

let representative :
    (h : (Pref.heap) Ghost.t) @ immutable  ->(scope : (((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h.Ghost.ghost q) || finite_scope h.Ghost.ghost q})) Ghost.t) @ total  ->
    (p : {p : node Pref.t | H.mem h.Ghost.ghost p && active h.Ghost.ghost p}) @ immutable  ->
    (t : {t : Pref.token | Pref.own t === h.Ghost.ghost}) @ local read  ->
    {r : resolved | H.mem h.Ghost.ghost r.#value && active h.Ghost.ghost r.#value && terminal h.Ghost.ghost r.#value
      && resolves h.Ghost.ghost p r.#value r.#path} @ immutable = fun h scope p t ->
    let start = {Ghost.ghost = ghost_ p} in
    let lift : (((value : node Pref.t) @ immutable ->
      (path : {d : resolution | resolves h.Ghost.ghost p value d}) @ immutable ->
      {d : resolution | resolves h.Ghost.ghost start.Ghost.ghost value d} @ immutable)) Ghost.t =
      {Ghost.ghost = ghost_ (fun value path -> path)} in
    let t : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p && active h.Ghost.ghost p} = t in
    let result = representative_loop start h scope p t lift in result

let bind_searched :
    (h : Pref.heap) @ immutable ghost ->
    (scope : ((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h q) || finite_scope h q})) @ total ghost ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (found : bool) -> (search : search) @ immutable ghost ->
    (t : {t : Pref.token | Pref.own t === h && H.mem h p && H.mem h q && active h p && active h q
      && observe h p === Some Var && terminal h q && not (p === q)
      && searched h p q found search}) @ unique ->
    {r : result | unified h p q r.#ok (Pref.own r.#state) r.#derivation} @ unique =
  fun h scope p q found search t ->
  if found then
    let d = ghost_ (Occurs_left search) in
    let ok = false in
    ghost_ (unified_def h p q ok h d);
    let r = #{ok; state = t; derivation = d} in
    (r : {r : result | unified h p q r.#ok (Pref.own r.#state) r.#derivation})
  else
    let t : {t : Pref.token | H.mem (Pref.own t) p} = t in
    let old = Pref.read p (borrow_ t) in ghost_ (active_def h p; at_level_def h p);
    match old.level with
    | Generic ->
      let _proof = ghost_ (let u = () in (u : {u : unit | false})) in
      assert false
    | Finite bound ->
      let t : {t : Pref.token | Pref.own t === h && bound >= 0 && active h q} = t in
      let lowered = Level_lower.lower h scope bound q t in
      let levels = ghost_ lowered.#edits in
      let middle = ghost_ (Pref.own (borrow_ lowered.#state)) in
      ghost_ (let u = () in let flag = false in
        lower_searched h bound levels p q flag search (u);
        lower_observe h bound levels p (u); lower_observe h bound levels q (u);
        lowering_at h bound levels p (u); frame_active h middle p (u);
        lowering_at h bound levels q (u); frame_active h middle q (u);
        below_def h p bound; lower_fixed h bound levels p (u); at_level_def middle p;
        let tree = lowered.#tree in lower_bounded_at middle bound tree q (u));
      let t = lowered.#state in
      let t : {t : Pref.token | H.mem (Pref.own t) p} = t in
      let current = Pref.read p (borrow_ t) in let link = {current with desc = Link q} in ghost_ (redirect_def middle p q);
      let t : {t : Pref.token | H.mem (Pref.own t) p} = t in
      let t = Pref.write p link t in
      let after = ghost_ (Pref.own (borrow_ t)) in
      let step = ghost_ (Bind_left search) in let tree = ghost_ lowered.#tree in
      let d = ghost_ (Lowering (bound, levels, tree, step)) in
      let ok = true in
      ghost_ (unified_def middle p q ok after step; unified_def h p q ok after d);
      let r = #{ok; state = t; derivation = d} in r

let bind :
    (h : Pref.heap) @ immutable ghost ->
    (scope : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h x) || finite_scope h x})) @ total ghost ->
    (unmarked : ((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h x with None -> true | Some v -> not v.visited})) @ total ghost ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (t : {t : Pref.token | Pref.own t === h && H.mem h p && H.mem h q
      && active h p && active h q && observe h p === Some Var
      && terminal h q && not (p === q)}) @ unique ->
    {r : result | unified h p q r.#ok (Pref.own r.#state) r.#derivation} @ unique =
  fun h scope unmarked p q t ->
    let t : {t : Pref.token | Pref.own t === h && H.mem h q && active h q} = t in
    let h_witness1 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h)} in
    let scope_witness2 : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness1.Ghost.ghost x) || finite_scope h_witness1.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (scope)} in
    let unmarked_witness3 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h_witness1.Ghost.ghost x with
        None -> true | Some v -> not v.visited})) Ghost.t = {Ghost.ghost = ghost_ (unmarked)} in
    let state_argument4 = t in
    let checked = Marked_occurs.occurs h_witness1 scope_witness2 unmarked_witness3 p q (state_argument4) in
    let marks = ghost_ checked.#marks in let search = ghost_ checked.#search in
    let found = checked.#found in let mid = ghost_ (scan_heap h marks) in
    let scope_mid : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem mid x) || finite_scope mid x}) @ total ghost = ghost_ (fun x ->
      let u = () in let u = scan_scope h scope p marks x (u) in u) in
    ghost_ (let u = () in scan_heap_def h marks;
      scan_observe h p marks p (u); scan_observe h p marks q (u);
      terminal_def h q; terminal_def mid q;
      scan_searched h p marks p q found search (u));
    let t = checked.#state in
    let t : {t : Pref.token | Pref.own t === mid && H.mem mid p && H.mem mid q
      && active mid p && active mid q && observe mid p === Some Var
      && terminal mid q && not (p === q) && searched mid p q found search} = t in
    let out = bind_searched mid scope_mid p q found search t in
    let derivation = ghost_ (Scanned (p, marks, out.#derivation)) in
    let state = out.#state in let after = ghost_ (Pref.own (borrow_ state)) in
    let ok = out.#ok in ghost_ (unified_def h p q ok after derivation);
    let r = #{ok; state; derivation} in r

let rec unify :
    (h : Pref.heap) @ immutable ghost ->
    (scope : ((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h q) || finite_scope h q})) @ total ghost ->
    (unmarked : ((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h x with None -> true | Some v -> not v.visited})) @ total ghost ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (t : {t : Pref.token | Pref.own t === h && H.mem h p && H.mem h q && active h p && active h q}) @ unique ->
    {r : result | unified h p q r.#ok (Pref.own r.#state) r.#derivation} @ unique =
  fun h scope unmarked p q t ->
  let rp : {r : resolved | H.mem h r.#value && active h r.#value && terminal h r.#value
    && resolves h p r.#value r.#path} =
    let p : {p : node Pref.t | H.mem h p && active h p} = p in
    let b = borrow_ t in
    let b : {b : Pref.token | Pref.own b === h} = b in
    let h_witness9 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h)} in
    let scope_witness10 : (((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness9.Ghost.ghost q) || finite_scope h_witness9.Ghost.ghost q})) Ghost.t = {Ghost.ghost = ghost_ (scope)} in
    let p_argument11 : {p : node Pref.t | H.mem h_witness9.Ghost.ghost p && active h_witness9.Ghost.ghost p} = let argument = p in argument in
    let state_argument12 = b in
    let r = representative h_witness9 scope_witness10 p_argument11 (state_argument12) in r in
  let sq : {r : resolved | H.mem h r.#value && active h r.#value && terminal h r.#value
    && resolves h q r.#value r.#path} =
    let q : {q : node Pref.t | H.mem h q && active h q} = q in
    let b = borrow_ t in
    let b : {b : Pref.token | Pref.own b === h} = b in
    let h_witness13 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h)} in
    let scope_witness14 : (((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness13.Ghost.ghost q) || finite_scope h_witness13.Ghost.ghost q})) Ghost.t = {Ghost.ghost = ghost_ (scope)} in
    let p_argument15 : {p : node Pref.t | H.mem h_witness13.Ghost.ghost p && active h_witness13.Ghost.ghost p} = let argument = q in argument in
    let state_argument16 = b in
    let r = representative h_witness13 scope_witness14 p_argument15 (state_argument16) in r in
  let r = rp.#value in
  let s = sq.#value in
  let result : {answer : result |
      unified h r s answer.#ok (Pref.own answer.#state) answer.#derivation} =
    let equal = Pref.equal r s in
    let result = if equal then
      let d = ghost_ Same in
      let ok = true in
      ghost_ (unified_def h r s ok h d);
      #{ok; state = t; derivation = d}
    else
      let n : {n : desc | Some n === observe h r} =
        let b = borrow_ t in
        let b : {b : Pref.token | H.mem (Pref.own b) r} = b in
        let old = Pref.read r b in ghost_ (observe_def h r);
        let n = old.desc in n in
      let m : {m : desc | Some m === observe h s} =
        let b = borrow_ t in
        let b : {b : Pref.token | H.mem (Pref.own b) s} = b in
        let old = Pref.read s b in ghost_ (observe_def h s);
        let m = old.desc in m in
      ghost_ (terminal_def h r);
      ghost_ (terminal_def h s);
      match n, m with
      | Var, _ ->
        let t : {t : Pref.token | Pref.own t === h && H.mem h r && H.mem h s && active h r && active h s
          && observe h r === Some Var && terminal h s && not (r === s)} = t in
        let answer = bind h scope unmarked r s t in answer
      | _, Var ->
        let t : {t : Pref.token | Pref.own t === h && H.mem h s && H.mem h r && active h s && active h r
          && observe h s === Some Var && terminal h r && not (s === r)} = t in
        let answer = bind h scope unmarked s r t in
        let ok = answer.#ok in
        let t = answer.#state in
        let after = ghost_ (Pref.own (borrow_ t)) in
        let d = ghost_ (Swap answer.#derivation) in
        ghost_ (unified_def h r s ok after d);
        #{ok; state = t; derivation = d}
      | Bool, Bool ->
        let ok = true in
        let d = ghost_ Constants in
        ghost_ (unified_def h r s ok h d);
        #{ok; state = t; derivation = d}
      | Arrow (a, b), Arrow (c, e) ->
        ghost_ (scope r);
        ghost_ (scope s);
        ghost_ (finite_scope_def h r; source_ok_def h r; observe_def h r);
        ghost_ (finite_scope_def h s; source_ok_def h s; observe_def h s);
        let t : {t : Pref.token | Pref.own t === h && H.mem h a && H.mem h c && active h a && active h c} = t in
        let left = unify h scope unmarked a c t in
        let left_ok = left.#ok in
        let ld = ghost_ left.#derivation in
        let t = left.#state in
        let middle = ghost_ (Pref.own (borrow_ t)) in
        if left_ok then
          let scope_middle : ((x : node Pref.t) @ immutable ->
              {u : unit | not (H.mem middle x) || finite_scope middle x}) @ total ghost =
            ghost_ (fun x ->
              scope x;
              let u = () in
              unified_scope h scope a c left_ok middle ld x (u);
              u) in
          let _proof = ghost_ (
            let u = () in
            unified_frame h a c left_ok middle ld b (u);
            unified_frame h a c left_ok middle ld e (u);
            unified_active h a c left_ok middle ld b (u);
            unified_active h a c left_ok middle ld e (u);
            let proof : {u : unit | H.mem middle b && H.mem middle e && active middle b && active middle e} = u in proof) in
          let t : {t : Pref.token | Pref.own t === middle && H.mem middle b && H.mem middle e && active middle b && active middle e} = t in
          let unmarked_middle : ((x : node Pref.t) @ immutable ->
            {u : unit | match H.at middle x with None -> true | Some v -> not v.visited}) @ total ghost = ghost_ (fun x ->
            unmarked x; let u = () in unified_scratch h a c left_ok middle ld x (u);
            scratch_frame_def h middle x; u) in
          let right = unify middle scope_middle unmarked_middle b e t in
          let ok = right.#ok in
          let t = right.#state in
          let after = ghost_ (Pref.own (borrow_ t)) in
          let d = ghost_ (Children (a, b, c, e, middle, left_ok, ld, right.#derivation)) in
          ghost_ (unified_def h r s ok after d);
          #{ok; state = t; derivation = d}
        else
          let ok = false in
          let d = ghost_ (Children (a, b, c, e, middle, left_ok, ld, Same)) in
          ghost_ (unified_def h r s ok middle d);
          #{ok; state = t; derivation = d}
      | Bool, Arrow _ | Arrow _, Bool ->
        let ok = false in
        let d = ghost_ Clash in
        ghost_ (unified_def h r s ok h d);
        #{ok; state = t; derivation = d}
      | Link _, _ | _, Link _ ->
        let _proof = ghost_ (let u = () in (u : {u : unit | false})) in
        assert false
    in result in
  let ok = result.#ok in
  let t = result.#state in
  let after = ghost_ (Pref.own (borrow_ t)) in
  let d = ghost_ (Resolve (r, s, rp.#path, sq.#path, result.#derivation)) in
  ghost_ (unified_def h p q ok after d);
  let answer = #{ok; state = t; derivation = d} in
  answer
