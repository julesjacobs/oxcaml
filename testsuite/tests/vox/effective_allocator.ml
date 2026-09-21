open Copy_spec
open Generalize_spec

let allocate : (h : node Pref.heap Ghost.t) @ immutable -> (depth : int) ->
    (desc : desc) @ immutable -> (pool : pool) @ immutable ->
    (state : {t : node Pref.token | Pref.own t === h.Ghost.ghost
      && pool_scoped h.Ghost.ghost pool && depth >= 0
      && payload_scoped h.Ghost.ghost (cell desc depth)}) @ unique ->
    {r : Pooled_spec.allocated |
      Hm_effective_execution_spec.allocated h.Ghost.ghost depth r.#value desc
      && Pref.own r.#state === H.put h.Ghost.ghost r.#value (cell desc depth)
      && r.#pool === Entry (r.#value, pool)
      && pool_scoped (Pref.own r.#state) r.#pool} @ unique =
  fun h depth desc pool state ->
    let refine_ state = state in let v = cell desc depth in
    let refine_ step = Pref.alloc v state in let p = step.value in
    let next = Entry (p, pool) in
    ghost_ (
      let u = () in
      Pooled_allocation_proofs.allocation_pool h.Ghost.ghost p v pool (refine_ u);
      Pooled_allocation_proofs.allocation_source h.Ghost.ghost p v p (refine_ u);
      let after = H.put h.Ghost.ghost p v in
      Copy_heap_proofs.put_frame h.Ghost.ghost p v p;
      pool_scoped_def after next;
      Hm_effective_execution_spec.allocated_def h.Ghost.ghost depth p desc);
    let out = #{Pooled_spec.value = p; state = step.state; pool = next} in
    refine_ out
