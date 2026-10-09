open Copy_spec
open Level_spec
open Lower_locality_spec
open Effective_lower_spec
module E := Effective_level
module U := Level_unifier_spec

val write_level :
  (h : node Pref.heap Ghost.t) @ immutable -> (heads : E.heads Ghost.t) @ total ->
    (bound : int) -> (p : node Pref.t) @ immutable ->
    (state : {t : node Pref.token | Pref.own t === h.Ghost.ghost && bound >= 0
      && E.effective_active h.Ghost.ghost heads.Ghost.ghost p && U.terminal h.Ghost.ghost
        p && E.valid_head h.Ghost.ghost heads.Ghost.ghost p
      && match H.at h.Ghost.ghost p with None -> false | Some v ->
        effective_children_below h.Ghost.ghost heads.Ghost.ghost v.desc bound}) @ unique
        ->
    {r : Level_spec.written | effective_lower_valid h.Ghost.ghost heads.Ghost.ghost bound
      r.#edits
      && Pref.own r.#state === lower_heap h.Ghost.ghost bound r.#edits
      && E.effective_below (Pref.own r.#state) heads.Ghost.ghost p bound
      && confined r.#edits (Tip p)} @ unique
