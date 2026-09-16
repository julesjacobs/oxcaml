open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec
open Optimized_unifier_spec

let finish : (before : Pref.heap) @ immutable ghost ->
    (old_p : node Pref.t) @ immutable -> (old_q : node Pref.t) @ immutable ->
    (h : Pref.heap) @ immutable ghost -> (d : derivation) @ immutable ghost ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ghost ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || finite_scope h x})) @ total ghost ->
    (state : {t : Pref.token | Pref.own t === h && unified before old_p old_q true h d
      && H.mem h old_p && H.mem h old_q && active h old_p && active h old_q}) @ unique ->
    {out : result | out.#ok && unified before old_p old_q true (Pref.own out.#state) out.#derivation} @ unique =
  fun before old_p old_q h d trees scope state ->
    let refine_ state = state in
    ghost_ (let ok = true in unified_def before old_p old_q ok h d; ());
    let rp : {r : resolved | H.mem h r.#value && active h r.#value && terminal h r.#value && resolves h old_p r.#value r.#path} =
      let input : {p : node Pref.t | H.mem h p && active h p} = refine_ old_p in
      let borrowed = borrow_ state in let borrowed : {t : Pref.token | Pref.own t === h} = refine_ borrowed in
      let refine_ out = Level_unifier.representative h scope input borrowed in let refine_ input = input in refine_ out in
    let refine_ rp = rp in
    let rq : {r : resolved | H.mem h r.#value && active h r.#value && terminal h r.#value && resolves h old_q r.#value r.#path} =
      let input : {p : node Pref.t | H.mem h p && active h p} = refine_ old_q in
      let borrowed = borrow_ state in let borrowed : {t : Pref.token | Pref.own t === h} = refine_ borrowed in
      let refine_ out = Level_unifier.representative h scope input borrowed in let refine_ input = input in refine_ out in
    let refine_ rq = rq in let p = rp.#value in let q = rq.#value in
    let refine_ same = Pref.equal p q in
    if same then (let out = #{ok = true; state; derivation = d} in refine_ out)
    else (
      let pv : {v : node | H.at h p === Some v} =
        let borrowed = borrow_ state in let borrowed : {t : Pref.token | H.mem (Pref.own t) p} = refine_ borrowed in
        let refine_ v = Pref.read p borrowed in refine_ v in
      let refine_ pv = pv in
      let qv : {v : node | H.at h q === Some v} =
        let borrowed = borrow_ state in let borrowed : {t : Pref.token | H.mem (Pref.own t) q} = refine_ borrowed in
        let refine_ v = Pref.read q borrowed in refine_ v in
      let refine_ qv = qv in
      match pv.desc, qv.desc, pv.level, qv.level with
      | Arrow _, Arrow _, Finite pn, Finite qn ->
        let source = if pn >= qn then p else q in
        let target = if pn >= qn then q else p in
        let source_tree : {t : tree | finite h t && tree_root t === source} @ immutable ghost = ghost_ (
          let refine_ t = trees source in refine_ t) in
        let target_tree : {t : tree | finite h t && tree_root t === target} @ immutable ghost = ghost_ (
          let refine_ t = trees target in refine_ t) in
        let refine_ source_tree = source_tree in let refine_ target_tree = target_tree in
        let proof : {u : unit | Structure_spec.linkable h source_tree target_tree} @ ghost = ghost_ (let u = () in
          let equality : {u : unit | readback source_tree === readback target_tree} =
          if pn >= qn then (Optimized_link_proofs.equal_roots before old_p old_q h d trees p q rp.#path rq.#path source_tree target_tree (refine_ u); refine_ u)
          else ( Optimized_link_proofs.equal_roots before old_p old_q h d trees p q rp.#path rq.#path target_tree source_tree (refine_ u); refine_ u) in
          let refine_ equality = equality in
          active_def h p; active_def h q;
          terminal_def h source; observe_def h source; terminal_def h target; observe_def h target;
          at_level_def h source; at_level_def h target; below_def h target pn; below_def h target qn;
          Structure_spec.linkable_def h source_tree target_tree; refine_ u) in
        let refine_ proof = proof in
        let state : {t : Pref.token | Pref.own t === h && Structure_spec.linkable h source_tree target_tree
          && tree_root source_tree === source && tree_root target_tree === target} = refine_ state in
        let refine_ linked = Structure_link.link h source_tree target_tree source target state in
        let after = ghost_ (Pref.own (borrow_ linked.#state)) in
        let derivation = ghost_ (Post_link (h, d, source_tree, target_tree)) in
        ghost_ (let ok = true in unified_def before old_p old_q ok after derivation; ());
        let out = #{ok = true; state = linked.#state; derivation} in refine_ out
      | _ -> let out = #{ok = true; state; derivation = d} in refine_ out)
