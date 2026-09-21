open Copy_spec
open Level_unifier_spec
open Level_spec

val representative :
  (h : (Pref.heap) Ghost.t) @ immutable  ->(scope : (((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h.Ghost.ghost q) || source_ok h.Ghost.ghost q})) Ghost.t) @
        total  ->
    (p : {p : node Pref.t | H.mem h.Ghost.ghost p}) @ immutable  ->
    (t : {t : Pref.token | Pref.own t === h.Ghost.ghost}) @ local read  ->
    {r : resolved | H.mem h.Ghost.ghost r.#value && terminal
      h.Ghost.ghost r.#value
      && resolves h.Ghost.ghost p r.#value r.#path} @ immutable
