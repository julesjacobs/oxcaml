open Copy_spec
open Level_unifier_spec
open Level_finite_spec
open Generalize_spec
open Hm_effective_runtime
module E = Effective_level

let unify : (h : node Pref.heap Ghost.t) @ immutable ->
    (heads : E.heads Ghost.t) @ total -> (depth : int Ghost.t) @ immutable ->
    (pool : pool Ghost.t) @ immutable ->
    (facts : (((x : node Pref.t) @ immutable ->
      {u : unit | runtime_at h.Ghost.ghost heads.Ghost.ghost depth.Ghost.ghost pool.Ghost.ghost x})) Ghost.t) @ total ->
    (trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (state : {t : node Pref.token | Pref.own t === h.Ghost.ghost
      && E.effective_active h.Ghost.ghost heads.Ghost.ghost p
      && E.effective_active h.Ghost.ghost heads.Ghost.ghost q}) @ unique ->
    {r : Effective_unifier_spec.result | Effective_unifier_spec.unified h.Ghost.ghost p q r.#ok (Pref.own r.#state) r.#derivation} @ unique =
  fun h heads depth pool facts trees p q state ->
    let valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t =
      {Ghost.ghost = ghost_ (fun x -> facts.Ghost.ghost x;
        runtime_at_def h.Ghost.ghost heads.Ghost.ghost depth.Ghost.ghost pool.Ghost.ghost x;
        safe_def h.Ghost.ghost heads.Ghost.ghost x; let u = () in refine_ u)} in
    let scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h.Ghost.ghost x)
      || E.effective_scope h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t =
      {Ghost.ghost = ghost_ (fun x -> facts.Ghost.ghost x;
        runtime_at_def h.Ghost.ghost heads.Ghost.ghost depth.Ghost.ghost pool.Ghost.ghost x;
        safe_def h.Ghost.ghost heads.Ghost.ghost x; let u = () in
        if H.mem h.Ghost.ghost x then (
          E.ordered_scope h.Ghost.ghost heads.Ghost.ghost valid.Ghost.ghost x (refine_ u); refine_ u)
        else refine_ u)} in
    let marks : (((x : node Pref.t) @ immutable -> {u : unit |
      match H.at h.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t =
      {Ghost.ghost = ghost_ (fun x -> facts.Ghost.ghost x;
        runtime_at_def h.Ghost.ghost heads.Ghost.ghost depth.Ghost.ghost pool.Ghost.ghost x;
        safe_def h.Ghost.ghost heads.Ghost.ghost x; let u = () in refine_ u)} in
    let order : (((x : node Pref.t) @ immutable -> {u : unit | E.effective_ordered h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t =
      {Ghost.ghost = ghost_ (fun x -> facts.Ghost.ghost x;
        runtime_at_def h.Ghost.ghost heads.Ghost.ghost depth.Ghost.ghost pool.Ghost.ghost x;
        safe_def h.Ghost.ghost heads.Ghost.ghost x; let u = () in refine_ u)} in
    let refine_ state = state in
    let refine_ out = Effective_unifier_runtime.unify h heads valid scope marks order trees p q (refine_ state) in
    refine_ out
