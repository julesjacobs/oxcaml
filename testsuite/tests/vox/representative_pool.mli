open Copy_spec
open Generalize_spec
open Representative_level

open Representative_pool_spec

val close_and_transfer :
  (h : node Pref.heap Ghost.t) @ immutable -> (cut : int) ->
    (child : pool) @ immutable -> (parent : pool) @ immutable ->
    (state : {t : node Pref.token | Pref.own t === h.Ghost.ghost &&
      pool_scoped h.Ghost.ghost child && pool_scoped h.Ghost.ghost parent}) @ unique ->
    {r : Nested_pool_spec.closed | Pref.own r.#state === close_heap h.Ghost.ghost cut
      child &&
      r.#parent === transfer_rep (close_heap h.Ghost.ghost cut child) child parent &&
      pool_scoped (Pref.own r.#state) r.#parent} @ unique
