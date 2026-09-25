open Copy_spec
open Level_unifier_spec
open Level_spec
open Effective_unifier_spec
module E := Effective_level

open Level_finite_spec

val bind :
  (h : (Pref.heap) Ghost.t) @ immutable ->
    (heads : E.heads Ghost.t) @ total ->
    (witness : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h.Ghost.ghost
      heads.Ghost.ghost x})) Ghost.t) @ total ->
    (scope : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h.Ghost.ghost x) || E.effective_scope h.Ghost.ghost
        heads.Ghost.ghost x})) Ghost.t) @ total ->
    (unmarked : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h.Ghost.ghost x with None -> true | Some v -> not
        v.visited})) Ghost.t) @ total ->
    (order : (((x : node Pref.t) @ immutable -> {u : unit | E.effective_ordered
      h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost
        t else observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (t : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p && H.mem
      h.Ghost.ghost q
      && active h.Ghost.ghost p && active h.Ghost.ghost q && observe h.Ghost.ghost p ===
        Some Var
      && terminal h.Ghost.ghost q && not (p === q)}) @ unique ->
    {r : result | unified h.Ghost.ghost p q r.#ok (Pref.own r.#state) r.#derivation} @
      unique
