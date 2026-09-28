open Copy_spec
open Level_unifier_spec
open Level_finite_spec
open Generalize_spec
open Hm_effective_runtime
module E := Effective_level

val unify :
  (h : node Pref.heap Ghost.t) @ immutable ->
    (heads : E.heads Ghost.t) @ total -> (depth : int Ghost.t) @ immutable ->
    (pool : pool Ghost.t) @ immutable ->
    (facts : (((x : node Pref.t) @ immutable ->
      {u : unit | runtime_at h.Ghost.ghost heads.Ghost.ghost depth.Ghost.ghost
        pool.Ghost.ghost x})) Ghost.t) @ total ->
    (trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost
        t else observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (state : {t : node Pref.token | Pref.own t === h.Ghost.ghost
      && E.effective_active h.Ghost.ghost heads.Ghost.ghost p
      && E.effective_active h.Ghost.ghost heads.Ghost.ghost q}) @ unique ->
    {r : Effective_unifier_spec.result | Effective_unifier_spec.unified h.Ghost.ghost p q
      r.#ok (Pref.own r.#state) r.#derivation} @ unique
