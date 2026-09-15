open Copy_spec
open Copy_heap_proofs
open Level_spec
open Generalize_spec
open Pooled_spec
open Pooled_allocation_proofs

let allocate : (h : node Pref.heap) @ immutable ghost -> (depth : int) -> (desc : desc) @ immutable -> (pool : pool) @ immutable ->
    (t : {t : node Pref.token | Pref.own t === h && pool_scoped h pool && depth >= 0
      && children_below h desc depth}) @ unique ->
    {r : allocated | not (H.mem h r.#value) && Pref.own r.#state === H.put h r.#value (cell desc depth)
      && below (Pref.own r.#state) r.#value depth && ordered (Pref.own r.#state) r.#value
      && r.#pool === Entry (r.#value, pool) && pool_scoped (Pref.own r.#state) r.#pool} @ unique = fun h depth desc pool t ->
  let refine_ t = t in let v = cell desc depth in
  ghost_ (cell_def desc depth; children_below_def h desc depth; payload_scoped_def h v;
    (match desc with Var | Bool -> () | Link q -> below_def h q depth; ()
    | Arrow (a, b) -> below_def h a depth; below_def h b depth; ()));
  let refine_ step = Pref.alloc v t in let p = step.value in let pool_next = Entry (p, pool) in
  ghost_ (let u = () in allocation_pool h p v pool (refine_ u);
    allocation_source h p v p (refine_ u); let after = H.put h p v in
    pool_scoped_def after pool_next; put_frame h p v p;
    let proof : {u : unit | pool_scoped (H.put h p v) pool_next} = refine_ u in proof);
  let after = ghost_ (H.put h p v) in
  ghost_ (let u = () in allocation_children h p v desc depth (refine_ u);
    cell_def desc depth; below_def after p depth; at_level_def after p; ordered_def after p; put_frame h p v p);
  let r = #{value = p; state = step.state; pool = pool_next} in refine_ r
