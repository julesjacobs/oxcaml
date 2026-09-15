open Unifier_spec
open Unifier_finite_spec
open Unifier_finite_proofs
open Stlc_spec
open Stlc_graph_proofs
open Stlc_solve_proofs

let rec solve : (h : Pref.heap) @ immutable ghost ->
    (trees : ((x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
      (if H.mem h x then finite h t else H.at h x === None)} @ immutable)) @ total ghost ->
    (eqs : {eqs : equations | equations_allocated h eqs}) @ immutable ->
    (t : {t : Pref.token | Pref.own t === h}) @ unique ->
    {r : solved_result | let refine_ eqs = eqs in solved h eqs r.#ok (Pref.own r.#state) r.#solving} @ unique =
  fun h trees eqs t ->
    let refine_ eqs = eqs in let refine_ t = t in ghost_ (equations_allocated_def h eqs);
    match eqs with
    | Nothing ->
      let ok = true in let solving = ghost_ Done in ghost_ (solved_def h eqs ok h solving);
      let r = #{ok; state = t; solving} in refine_ r
    | Equal (p, q) ->
      let scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || scoped h x})
          @ total ghost = ghost_ (fun x ->
        let refine_ tree = trees x in let u = () in
        if H.mem h x then (finite_scope_at h tree (refine_ u); refine_ u) else refine_ u) in
      let t : {t : Pref.token | Pref.own t === h && H.mem h p && H.mem h q} = refine_ t in
      let refine_ step = Unifier.unify h scope p q t in
      let ok = step.#ok in let solving = ghost_ (Unified step.#derivation) in
      let t = step.#state in let after = ghost_ (Pref.own (borrow_ t)) in
      ghost_ (solved_def h eqs ok after solving);
      let r = #{ok; state = t; solving} in refine_ r
    | And (a, b) ->
      let a : {a : equations | equations_allocated h a} = refine_ a in
      let t : {t : Pref.token | Pref.own t === h} = refine_ t in
      let refine_ left = solve h trees a t in let refine_ a = a in
      let middle = ghost_ (Pref.own (borrow_ left.#state)) in
      if left.#ok then (
        let middle_trees : ((x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
            (if H.mem middle x then finite middle t else H.at middle x === None)} @ immutable)
            @ total ghost = ghost_ (fun x -> let u = () in
          let refine_ t = solved_finite_at h trees a true middle left.#solving x (refine_ u) in refine_ t) in
        ghost_ (
          let keep : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || H.mem middle x}
              @ total = fun x -> let u = () in
            solved_frame h a true middle left.#solving x (refine_ u); refine_ u in
          let u = () in equations_frame h middle keep b (refine_ u));
        let b : {b : equations | equations_allocated middle b} = refine_ b in
        let t = left.#state in let t : {t : Pref.token | Pref.own t === middle} = refine_ t in
        let refine_ right = solve middle middle_trees b t in let refine_ b = b in
        let ok = right.#ok in let t = right.#state in let after = ghost_ (Pref.own (borrow_ t)) in
        let solving = ghost_ (Sequence (middle, true, left.#solving, right.#solving)) in
        ghost_ (solved_def h eqs ok after solving);
        let r = #{ok; state = t; solving} in refine_ r)
      else (
        let ok = false in let solving = ghost_ (Sequence (middle, false, left.#solving, Done)) in
        ghost_ (solved_def h eqs ok middle solving);
        let r = #{ok; state = left.#state; solving} in refine_ r)
