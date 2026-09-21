open Copy_spec
open Copy_heap_proofs
open Level_spec
open Generalize_spec
open Pooled_spec
open Pooled_allocation_proofs

let allocate : (h : Pref.heap Ghost.t) @ immutable -> (depth : int) -> (desc : desc) @ immutable -> (pool : pool) @ immutable ->
    (t : {t : Pref.token | Pref.own t === h.Ghost.ghost && pool_scoped h.Ghost.ghost pool && depth >= 0
      && children_below h.Ghost.ghost desc depth}) @ unique ->
    {r : allocated | not (H.mem h.Ghost.ghost r.#value) && Pref.own r.#state === H.put h.Ghost.ghost r.#value (cell desc depth)
      && below (Pref.own r.#state) r.#value depth && ordered (Pref.own r.#state) r.#value
      && r.#pool === Entry (r.#value, pool) && pool_scoped (Pref.own r.#state) r.#pool} @ unique = fun h depth desc pool t ->
  let v = cell desc depth in
  ghost_ (cell_def desc depth; children_below_def h.Ghost.ghost desc depth; payload_scoped_def h.Ghost.ghost v;
    (match desc with Var | Bool -> () | Link q -> below_def h.Ghost.ghost q depth; ()
    | Arrow (a, b) -> below_def h.Ghost.ghost a depth; below_def h.Ghost.ghost b depth; ()));
  let step = Pref.alloc v t in let p = step.value in let pool_next = Entry (p, pool) in
  ghost_ (let u = () in allocation_pool h.Ghost.ghost p v pool (u);
    allocation_source h.Ghost.ghost p v p (u); let after = H.put h.Ghost.ghost p v in
    pool_scoped_def after pool_next; put_frame h.Ghost.ghost p v p;
    let proof : {u : unit | pool_scoped (H.put h.Ghost.ghost p v) pool_next} = u in proof);
  let after = ghost_ (H.put h.Ghost.ghost p v) in
  ghost_ (let u = () in allocation_children h.Ghost.ghost p v desc depth (u);
    cell_def desc depth; below_def after p depth; at_level_def after p; ordered_def after p; put_frame h.Ghost.ghost p v p);
  let r = #{value = p; state = step.state; pool = pool_next} in r
