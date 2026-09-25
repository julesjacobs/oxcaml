open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec
open Effective_unifier_spec
module E := Effective_level

val finish :
  (before : (Pref.heap) Ghost.t) @ immutable ->
    (old_p : node Pref.t) @ immutable -> (old_q : node Pref.t) @ immutable ->
    (h : (Pref.heap) Ghost.t) @ immutable ->
    (heads : E.heads Ghost.t) @ total ->
    (valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h.Ghost.ghost
      heads.Ghost.ghost x})) Ghost.t) @ total ->
    (d : (derivation) Ghost.t) @ immutable ->
    (trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost
        t else observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total ->
    (scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h.Ghost.ghost x) ||
      source_ok h.Ghost.ghost x})) Ghost.t) @ total ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost && unified before.Ghost.ghost
      old_p old_q true h.Ghost.ghost d.Ghost.ghost
      && H.mem h.Ghost.ghost old_p && H.mem h.Ghost.ghost old_q && E.effective_active
        h.Ghost.ghost heads.Ghost.ghost old_p && E.effective_active h.Ghost.ghost
        heads.Ghost.ghost old_q}) @ unique ->
    {out : result | out.#ok && unified before.Ghost.ghost old_p old_q true (Pref.own
      out.#state) out.#derivation} @ unique
