open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec
open Effective_unifier_spec
module E := Effective_level

val unify :
  (h : node Pref.heap Ghost.t) @ immutable -> (heads : E.heads Ghost.t) @ total ->
    (valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h.Ghost.ghost
      heads.Ghost.ghost x})) Ghost.t) @ total ->
    (scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h.Ghost.ghost x) ||
      E.effective_scope h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (unmarked : (((x : node Pref.t) @ immutable -> {u : unit | match H.at h.Ghost.ghost x
      with None -> true | Some v -> not v.visited})) Ghost.t) @ total ->
    (order : (((x : node Pref.t) @ immutable -> {u : unit | E.effective_ordered
      h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (trees : (((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x && (if H.mem
      h.Ghost.ghost x then finite h.Ghost.ghost t else observe h.Ghost.ghost x === None)}
      @ immutable)) Ghost.t) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (state : {t : node Pref.token | Pref.own t === h.Ghost.ghost
      && E.effective_active h.Ghost.ghost heads.Ghost.ghost p && E.effective_active
        h.Ghost.ghost heads.Ghost.ghost q}) @ unique ->
    {out : result | unified h.Ghost.ghost p q out.#ok (Pref.own out.#state)
      out.#derivation} @ unique
