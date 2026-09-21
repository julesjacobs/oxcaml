open Copy_spec
open Level_spec
open Lower_locality_spec
open Level_finite_spec
open Effective_lower_spec
module E := Effective_level

val lower :
  (h : node Pref.heap Ghost.t) @ immutable -> (heads : E.heads Ghost.t) @ total ->
    (witness : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h.Ghost.ghost
      heads.Ghost.ghost x})) Ghost.t) @ total ->
    (scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h.Ghost.ghost x) ||
      E.effective_scope h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (order : (((x : node Pref.t) @ immutable -> {u : unit | E.effective_ordered
      h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost
        t else Level_unifier_spec.observe h.Ghost.ghost x === None)} @ immutable))
        Ghost.t) @ total ->
    (bound : int) -> (p : node Pref.t) @ immutable ->
    (t : {t : node Pref.token | Pref.own t === h.Ghost.ghost && bound >= 0 &&
      E.effective_active h.Ghost.ghost heads.Ghost.ghost p}) @ unique ->
    {r : lowered | effective_lower_valid h.Ghost.ghost heads.Ghost.ghost bound r.#edits
      && Pref.own r.#state === lower_heap h.Ghost.ghost bound r.#edits
      && bound_root r.#tree === p && effective_bounded (Pref.own r.#state)
        heads.Ghost.ghost bound r.#tree
      && confined r.#edits r.#tree
      && Terminal_lower_spec.completed h.Ghost.ghost bound p (Pref.own r.#state) r.#edits
        r.#tree} @ unique
