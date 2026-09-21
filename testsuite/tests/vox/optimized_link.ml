open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec
open Optimized_unifier_spec

let finish : (before : (Pref.heap) Ghost.t) @ immutable ->
    (old_p : node Pref.t) @ immutable -> (old_q : node Pref.t) @ immutable ->
    (h : (Pref.heap) Ghost.t) @ immutable ->
    (d : (derivation) Ghost.t) @ immutable ->
    (trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total ->
    (scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h.Ghost.ghost x) || finite_scope h.Ghost.ghost x})) Ghost.t) @ total ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost && unified before.Ghost.ghost old_p old_q true h.Ghost.ghost d.Ghost.ghost
      && H.mem h.Ghost.ghost old_p && H.mem h.Ghost.ghost old_q && active h.Ghost.ghost old_p && active h.Ghost.ghost old_q}) @ unique ->
    {out : result | out.#ok && unified before.Ghost.ghost old_p old_q true (Pref.own out.#state) out.#derivation} @ unique = fun before old_p old_q h d trees scope state ->
    ghost_ (let ok = true in unified_def before.Ghost.ghost old_p old_q ok h.Ghost.ghost d.Ghost.ghost; ());
    let rp : {r : resolved | H.mem h.Ghost.ghost r.#value && active h.Ghost.ghost r.#value && terminal h.Ghost.ghost r.#value && resolves h.Ghost.ghost old_p r.#value r.#path} =
      let input : {p : node Pref.t | H.mem h.Ghost.ghost p && active h.Ghost.ghost p} = old_p in
      let borrowed = borrow_ state in let borrowed : {t : Pref.token | Pref.own t === h.Ghost.ghost} = borrowed in
      let h_witness1 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
      let scope_witness2 : (((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness1.Ghost.ghost q) || finite_scope h_witness1.Ghost.ghost q})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
      let p_argument3 : {p : node Pref.t | H.mem h_witness1.Ghost.ghost p && active h_witness1.Ghost.ghost p} = let argument = input in argument in
      let state_argument4 = borrowed in
      let out = Level_unifier.representative h_witness1 scope_witness2 p_argument3 (state_argument4) in out in
    let rq : {r : resolved | H.mem h.Ghost.ghost r.#value && active h.Ghost.ghost r.#value && terminal h.Ghost.ghost r.#value && resolves h.Ghost.ghost old_q r.#value r.#path} =
      let input : {p : node Pref.t | H.mem h.Ghost.ghost p && active h.Ghost.ghost p} = old_q in
      let borrowed = borrow_ state in let borrowed : {t : Pref.token | Pref.own t === h.Ghost.ghost} = borrowed in
      let h_witness5 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
      let scope_witness6 : (((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness5.Ghost.ghost q) || finite_scope h_witness5.Ghost.ghost q})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
      let p_argument7 : {p : node Pref.t | H.mem h_witness5.Ghost.ghost p && active h_witness5.Ghost.ghost p} = let argument = input in argument in
      let state_argument8 = borrowed in
      let out = Level_unifier.representative h_witness5 scope_witness6 p_argument7 (state_argument8) in out in
    let p = rp.#value in let q = rq.#value in
    let same = Pref.equal p q in
    if same then (let out = #{ok = true; state; derivation = d.Ghost.ghost} in out)
    else (
      let pv : {v : node | H.at h.Ghost.ghost p === Some v} =
        let borrowed = borrow_ state in let borrowed : {t : Pref.token | H.mem (Pref.own t) p} = borrowed in
        let v = Pref.read p borrowed in v in
      let qv : {v : node | H.at h.Ghost.ghost q === Some v} =
        let borrowed = borrow_ state in let borrowed : {t : Pref.token | H.mem (Pref.own t) q} = borrowed in
        let v = Pref.read q borrowed in v in
      match pv.desc, qv.desc, pv.level, qv.level with
      | Arrow _, Arrow _, Finite pn, Finite qn ->
        let source = if pn >= qn then p else q in
        let target = if pn >= qn then q else p in
        let source_tree : {t : tree | finite h.Ghost.ghost t && tree_root t === source} @ immutable ghost = ghost_ (
          let t = trees.Ghost.ghost source in t) in
        let target_tree : {t : tree | finite h.Ghost.ghost t && tree_root t === target} @ immutable ghost = ghost_ (
          let t = trees.Ghost.ghost target in t) in
        let _proof : {u : unit | Structure_spec.linkable h.Ghost.ghost source_tree target_tree} @ ghost = ghost_ (let u = () in
          let _equality : {u : unit | readback source_tree === readback target_tree} =
          if pn >= qn then (Optimized_link_proofs.equal_roots before.Ghost.ghost old_p old_q h.Ghost.ghost d.Ghost.ghost trees.Ghost.ghost p q rp.#path rq.#path source_tree target_tree (u); u)
          else ( Optimized_link_proofs.equal_roots before.Ghost.ghost old_p old_q h.Ghost.ghost d.Ghost.ghost trees.Ghost.ghost p q rp.#path rq.#path target_tree source_tree (u); u) in
          active_def h.Ghost.ghost p; active_def h.Ghost.ghost q;
          terminal_def h.Ghost.ghost source; observe_def h.Ghost.ghost source; terminal_def h.Ghost.ghost target; observe_def h.Ghost.ghost target;
          at_level_def h.Ghost.ghost source; at_level_def h.Ghost.ghost target; below_def h.Ghost.ghost target pn; below_def h.Ghost.ghost target qn;
          Structure_spec.linkable_def h.Ghost.ghost source_tree target_tree; u) in
        let state : {t : Pref.token | Pref.own t === h.Ghost.ghost && Structure_spec.linkable h.Ghost.ghost source_tree target_tree
          && tree_root source_tree === source && tree_root target_tree === target} = state in
        let h_witness9 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
        let source_witness10 : (tree) Ghost.t = {Ghost.ghost = ghost_ (source_tree)} in
        let target_witness11 : (tree) Ghost.t = {Ghost.ghost = ghost_ (target_tree)} in
        let state_argument12 = state in
        let linked = Structure_link.link h_witness9 source_witness10 target_witness11 source target (state_argument12) in
        let after = ghost_ (Pref.own (borrow_ linked.#state)) in
        let derivation = ghost_ (Post_link (h.Ghost.ghost, d.Ghost.ghost, source_tree, target_tree)) in
        ghost_ (let ok = true in unified_def before.Ghost.ghost old_p old_q ok after derivation; ());
        let out = #{ok = true; state = linked.#state; derivation} in out
      | _ -> let out = #{ok = true; state; derivation = d.Ghost.ghost} in out)
