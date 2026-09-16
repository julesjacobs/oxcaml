open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec
open Optimized_unifier_spec
open Level_unifier_metadata
open Optimized_metadata
let rec raw :
    (h : node Pref.heap) @ immutable ghost ->
    (scope : ((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h q) || finite_scope h q})) @ total ghost ->
    (unmarked : ((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h x with None -> true | Some v -> not v.visited})) @ total ghost ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ghost ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (t : {t : node Pref.token | Pref.own t === h && H.mem h p && H.mem h q && active h p && active h q}) @ unique ->
    {r : result | unified h p q r.#ok (Pref.own r.#state) r.#derivation} @ unique =
  fun h scope unmarked trees p q t ->
  let refine_ t = t in
  let rp : {r : resolved | H.mem h r.#value && active h r.#value && terminal h r.#value
    && resolves h p r.#value r.#path} =
    let p : {p : node Pref.t | H.mem h p && active h p} = refine_ p in
    let b = borrow_ t in
    let b : {b : node Pref.token | Pref.own b === h} = refine_ b in
    let refine_ r = Level_unifier.representative h scope p b in refine_ r in
  let refine_ rp = rp in
  let sq : {r : resolved | H.mem h r.#value && active h r.#value && terminal h r.#value
    && resolves h q r.#value r.#path} =
    let q : {q : node Pref.t | H.mem h q && active h q} = refine_ q in
    let b = borrow_ t in
    let b : {b : node Pref.token | Pref.own b === h} = refine_ b in
    let refine_ r = Level_unifier.representative h scope q b in refine_ r in
  let refine_ sq = sq in
  let r = rp.#value in
  let s = sq.#value in
  let result : {answer : result |
      unified h r s answer.#ok (Pref.own answer.#state) answer.#derivation} =
    let refine_ equal = Pref.equal r s in
    let result = if equal then
      let old = ghost_ Level_unifier_spec.Same in
      ghost_ (let ok = true in Level_unifier_spec.unified_def h r s ok h old; ());
      let d = ghost_ (Base old) in
      let ok = true in
      ghost_ (unified_def h r s ok h d);
      #{ok; state = t; derivation = d}
    else
      let n : {n : desc | Some n === observe h r} =
        let b = borrow_ t in
        let b : {b : node Pref.token | H.mem (Pref.own b) r} = refine_ b in
        let refine_ old = Pref.read r b in ghost_ (observe_def h r);
        let n = old.desc in refine_ n in
      let refine_ n = n in
      let m : {m : desc | Some m === observe h s} =
        let b = borrow_ t in
        let b : {b : node Pref.token | H.mem (Pref.own b) s} = refine_ b in
        let refine_ old = Pref.read s b in ghost_ (observe_def h s);
        let m = old.desc in refine_ m in
      let refine_ m = m in
      ghost_ (terminal_def h r);
      ghost_ (terminal_def h s);
      match n, m with
      | Var, _ ->
        let t : {t : node Pref.token | Pref.own t === h && H.mem h r && H.mem h s && active h r && active h s
          && observe h r === Some Var && terminal h s && not (r === s)} = refine_ t in
        let refine_ answer = Level_unifier.bind h scope unmarked r s t in
        let after = ghost_ (Pref.own (borrow_ answer.#state)) in
        let d = ghost_ (Base answer.#derivation) in let ok = answer.#ok in
        ghost_ (unified_def h r s ok after d);
        #{ok; state = answer.#state; derivation = d}
      | _, Var ->
        let t : {t : node Pref.token | Pref.own t === h && H.mem h s && H.mem h r && active h s && active h r
          && observe h s === Some Var && terminal h r && not (s === r)} = refine_ t in
        let refine_ answer = Level_unifier.bind h scope unmarked s r t in
        let ok = answer.#ok in
        let t = answer.#state in
        let after = ghost_ (Pref.own (borrow_ t)) in
        let old = ghost_ (Level_unifier_spec.Swap answer.#derivation) in
        ghost_ (Level_unifier_spec.unified_def h r s ok after old);
        let d = ghost_ (Base old) in
        ghost_ (unified_def h r s ok after d);
        #{ok; state = t; derivation = d}
      | Bool, Bool ->
        let ok = true in
        let old = ghost_ Level_unifier_spec.Constants in
      ghost_ (let ok = true in Level_unifier_spec.unified_def h r s ok h old; ());
      let d = ghost_ (Base old) in
        ghost_ (unified_def h r s ok h d);
        #{ok; state = t; derivation = d}
      | Arrow (a, b), Arrow (c, e) ->
        ghost_ (scope r);
        ghost_ (scope s);
        ghost_ (finite_scope_def h r; source_ok_def h r; observe_def h r);
        ghost_ (finite_scope_def h s; source_ok_def h s; observe_def h s);
        let t : {t : node Pref.token | Pref.own t === h && H.mem h a && H.mem h c && active h a && active h c} = refine_ t in
        let refine_ left = unify h scope unmarked trees a c t in
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
              unified_scope h scope a c left_ok middle ld x (refine_ u);
              refine_ u) in
          let proof = ghost_ (
            let u = () in
            unified_frame h a c left_ok middle ld b (refine_ u);
            unified_frame h a c left_ok middle ld e (refine_ u);
            unified_active h a c left_ok middle ld b (refine_ u);
            unified_active h a c left_ok middle ld e (refine_ u);
            let proof : {u : unit | H.mem middle b && H.mem middle e && active middle b && active middle e} = refine_ u in proof) in
          let refine_ proof = proof in
          let t : {t : node Pref.token | Pref.own t === middle && H.mem middle b && H.mem middle e && active middle b && active middle e} = refine_ t in
          let unmarked_middle : ((x : node Pref.t) @ immutable ->
            {u : unit | match H.at middle x with None -> true | Some v -> not v.visited}) @ total ghost = ghost_ (fun x ->
            unmarked x; let u = () in unified_scratch h a c left_ok middle ld x (refine_ u);
            scratch_frame_def h middle x; refine_ u) in
          let trees_middle : ((x : node Pref.t) @ immutable ->
            {t : tree | tree_root t === x && (if H.mem middle x then finite middle t else observe middle x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
              let u = () in let refine_ tree = Optimized_finite_proofs.unified_finite_at h trees a c left_ok middle ld x (refine_ u) in refine_ tree) in
          let refine_ right = unify middle scope_middle unmarked_middle trees_middle b e t in
          let ok = right.#ok in
          let t = right.#state in
          let after = ghost_ (Pref.own (borrow_ t)) in
          let d = ghost_ (Children (a, b, c, e, middle, left_ok, ld, right.#derivation)) in
          ghost_ (unified_def h r s ok after d);
          if not ok then #{ok; state = t; derivation = d} else (
            let trees_after : ((x : node Pref.t) @ immutable ->
              {t : tree | tree_root t === x && (if H.mem after x then finite after t else observe after x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
                let u = () in let refine_ tree = Optimized_finite_proofs.unified_finite_at h trees r s ok after d x (refine_ u) in refine_ tree) in
            let scope_after : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem after x) || finite_scope after x}) @ total ghost = ghost_ (fun x ->
              let u = () in let refine_ u = unified_scope h scope r s ok after d x (refine_ u) in refine_ u) in
            ghost_ (let u = () in unified_frame h r s ok after d r (refine_ u); unified_frame h r s ok after d s (refine_ u);
              unified_active h r s ok after d r (refine_ u); unified_active h r s ok after d s (refine_ u); ());
            let t : {t : node Pref.token | Pref.own t === after && unified h r s true after d
              && H.mem after r && H.mem after s && active after r && active after s} = refine_ t in
            let refine_ linked = Optimized_link.finish h r s after d trees_after scope_after t in linked)
        else
          let ok = false in
          let d = ghost_ (Children (a, b, c, e, middle, left_ok, ld, Base Same)) in
          ghost_ (unified_def h r s ok middle d);
          #{ok; state = t; derivation = d}
      | Bool, Arrow _ | Arrow _, Bool ->
        let ok = false in
        let old = ghost_ Level_unifier_spec.Clash in
      ghost_ (let ok = false in Level_unifier_spec.unified_def h r s ok h old; ());
      let d = ghost_ (Base old) in
        ghost_ (unified_def h r s ok h d);
        #{ok; state = t; derivation = d}
      | Link _, _ | _, Link _ ->
        let proof = ghost_ (let u = () in (refine_ u : {u : unit | false})) in
        let refine_ proof = proof in assert false
    in refine_ result in
  let refine_ result = result in
  let ok = result.#ok in
  let t = result.#state in
  let after = ghost_ (Pref.own (borrow_ t)) in
  let d = ghost_ (Resolve (r, s, rp.#path, sq.#path, result.#derivation)) in
  ghost_ (unified_def h p q ok after d);
  let answer = #{ok; state = t; derivation = d} in
  refine_ answer

and unify : (h : node Pref.heap) @ immutable ghost ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || finite_scope h x})) @ total ghost ->
    (unmarked : ((x : node Pref.t) @ immutable -> {u : unit | match H.at h x with None -> true | Some v -> not v.visited})) @ total ghost ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ghost ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (state : {t : node Pref.token | Pref.own t === h && H.mem h p && H.mem h q && active h p && active h q}) @ unique ->
    {out : result | unified h p q out.#ok (Pref.own out.#state) out.#derivation} @ unique =
  fun h scope unmarked trees p q state ->
    let refine_ state = state in
    let state : {t : node Pref.token | Pref.own t === h && H.mem h p && active h p} = refine_ state in
    let refine_ first = Compressed_representative.representative h scope p state in
    let h1 = ghost_ (Pref.own (borrow_ first.#state)) in let edits1 = ghost_ first.#edits in
    let scope1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || finite_scope h1 x}) @ total ghost = ghost_ (fun x ->
      let u = () in let refine_ u = Compression_proofs.scope h h1 edits1 scope x (refine_ u) in refine_ u) in
    ghost_ (let u = () in Compression_proofs.frame h h1 edits1 q (refine_ u);
      scratch_frame_def h h1 q; active_def h q; active_def h1 q; at_level_def h q; at_level_def h1 q; ());
    let state : {t : node Pref.token | Pref.own t === h1 && H.mem h1 q && active h1 q} = refine_ first.#state in
    let refine_ second = Compressed_representative.representative h1 scope1 q state in
    let h2 = ghost_ (Pref.own (borrow_ second.#state)) in let edits2 = ghost_ second.#edits in
    let scope2 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h2 x) || finite_scope h2 x}) @ total ghost = ghost_ (fun x ->
      let u = () in let refine_ u = Compression_proofs.scope h1 h2 edits2 scope1 x (refine_ u) in refine_ u) in
    let unmarked2 : ((x : node Pref.t) @ immutable -> {u : unit | match H.at h2 x with None -> true | Some v -> not v.visited}) @ total ghost = ghost_ (fun x ->
      unmarked x; let u = () in Compression_proofs.frame h h1 edits1 x (refine_ u); Compression_proofs.frame h1 h2 edits2 x (refine_ u);
      scratch_frame_def h h1 x; scratch_frame_def h1 h2 x; refine_ u) in
    let trees2 : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h2 x then finite h2 t else observe h2 x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
        let refine_ tree = trees x in let u = () in
        Compression_proofs.frame h h1 edits1 x (refine_ u); Compression_proofs.frame h1 h2 edits2 x (refine_ u);
        if H.mem h x then (
          let refine_ tree1 = Compression_proofs.finite h h1 edits1 tree (refine_ u) in
          let refine_ tree2 = Compression_proofs.finite h1 h2 edits2 tree1 (refine_ u) in refine_ tree2)
        else (scratch_frame_def h h1 x; scratch_frame_def h1 h2 x;
          observe_def h x; observe_def h1 x; observe_def h2 x; refine_ tree)) in
    ghost_ (let u = () in Compression_proofs.frame h h1 edits1 p (refine_ u); Compression_proofs.frame h1 h2 edits2 p (refine_ u);
      Compression_proofs.frame h1 h2 edits2 q (refine_ u);
      active_def h p; active_def h1 p; active_def h2 p; active_def h2 q;
      at_level_def h p; at_level_def h1 p; at_level_def h2 p;
      at_level_def h1 q; at_level_def h2 q; ());
    let state : {t : node Pref.token | Pref.own t === h2 && H.mem h2 p && H.mem h2 q && active h2 p && active h2 q} = refine_ second.#state in
    let refine_ out = raw h2 scope2 unmarked2 trees2 p q state in
    let after = ghost_ (Pref.own (borrow_ out.#state)) in let ok = out.#ok in
    let d2 = ghost_ (Pre_compress (h2, edits2, out.#derivation)) in
    let d1 = ghost_ (Pre_compress (h1, edits1, d2)) in
    ghost_ (unified_def h1 p q ok after d2; unified_def h p q ok after d1);
    let result = #{ok; state = out.#state; derivation = d1} in refine_ result
