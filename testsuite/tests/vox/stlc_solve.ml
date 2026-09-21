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
    {r : solved_result | solved h eqs r.#ok (Pref.own r.#state) r.#solving} @ unique =
  fun h trees eqs t ->
    ghost_ (equations_allocated_def h eqs);
    match eqs with
    | Nothing ->
      let ok = true in let solving = ghost_ Done in ghost_ (solved_def h eqs ok h solving);
      let r = #{ok; state = t; solving} in r
    | Equal (p, q) ->
      let scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || scoped h x})
          @ total ghost = ghost_ (fun x ->
        let tree = trees x in let u = () in
        if H.mem h x then (finite_scope_at h tree (u); u) else u) in
      let t : {t : Pref.token | Pref.own t === h && H.mem h p && H.mem h q} = t in
      let step = Unifier.unify h scope p q t in
      let ok = step.#ok in let solving = ghost_ (Unified step.#derivation) in
      let t = step.#state in let after = ghost_ (Pref.own (borrow_ t)) in
      ghost_ (solved_def h eqs ok after solving);
      let r = #{ok; state = t; solving} in r
    | And (a, b) ->
      let a : {a : equations | equations_allocated h a} = a in
      let t : {t : Pref.token | Pref.own t === h} = t in
      let left = solve h trees a t in let middle = ghost_ (Pref.own (borrow_ left.#state)) in
      if left.#ok then (
        let middle_trees : ((x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
            (if H.mem middle x then finite middle t else H.at middle x === None)} @ immutable)
            @ total ghost = ghost_ (fun x -> let u = () in
          let t = solved_finite_at h trees a true middle left.#solving x (u) in t) in
        ghost_ (
          let keep : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || H.mem middle x}
              @ total = fun x -> let u = () in
            solved_frame h a true middle left.#solving x (u); u in
          let u = () in equations_frame h middle keep b (u));
        let b : {b : equations | equations_allocated middle b} = b in
        let t = left.#state in let t : {t : Pref.token | Pref.own t === middle} = t in
        let right = solve middle middle_trees b t in let ok = right.#ok in let t = right.#state in let after = ghost_ (Pref.own (borrow_ t)) in
        let solving = ghost_ (Sequence (middle, true, left.#solving, right.#solving)) in
        ghost_ (solved_def h eqs ok after solving);
        let r = #{ok; state = t; solving} in r)
      else (
        let ok = false in let solving = ghost_ (Sequence (middle, false, left.#solving, Done)) in
        ghost_ (solved_def h eqs ok middle solving);
        let r = #{ok; state = left.#state; solving} in r)
