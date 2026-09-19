open Copy_spec
open Generalize_spec

val allocate :
  (h : Pref.heap Ghost.t) @ immutable -> (depth : int) ->
    (desc : desc) @ immutable -> (pool : pool) @ immutable ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost
      && pool_scoped h.Ghost.ghost pool && depth >= 0
      && payload_scoped h.Ghost.ghost (cell desc depth)}) @ unique ->
    {r : Pooled_spec.allocated |
      Hm_effective_execution_spec.allocated h.Ghost.ghost depth r.#value desc
      && Pref.own r.#state === H.put h.Ghost.ghost r.#value (cell desc depth)
      && r.#pool === Entry (r.#value, pool)
      && pool_scoped (Pref.own r.#state) r.#pool} @ unique
