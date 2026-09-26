open Copy_spec
open Copy_heap_proofs
open Level_spec
open Generalize_spec
open Pooled_spec
open Copy_order_proofs

let instantiate : (saved : node Pref.heap) @ immutable ghost ->
    (scope : ((p : node Pref.t) @ immutable ->
      {u : unit | if H.mem saved p then source_ok saved p else H.at saved p === None})) @ total ghost ->
    (base : pool) @ immutable -> (depth : int) ->
    (bounds : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem saved x) || not (finite_node saved x) || below saved x depth})) @ total ghost ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | ordered saved x})) @ total ghost ->
    (p : {p : node Pref.t | H.mem saved p}) @ immutable ->
    (t : {t : node Pref.token | Pref.own t === saved && pool_scoped saved base && depth >= 0}) @ unique ->
    {r : instance | let refine_ p = p in
      valid saved r.#epoch depth r.#history
      && Pref.own r.#state === heap saved r.#epoch depth r.#history
      && r.#pool === registered base r.#epoch r.#history
      && r.#trail === touched r.#history
      && target_for saved r.#history p r.#value
      && pool_scoped (Pref.own r.#state) r.#pool
      && below (Pref.own r.#state) r.#value depth
      && ordered (Pref.own r.#state) r.#value} @ unique =
  fun saved scope base depth bounds order p t ->
    let refine_ t = t in
    let t : {t : node Pref.token | Pref.own t === saved && pool_scoped saved base} = refine_ t in
    let checked_depth : {n : int | n >= 0} = refine_ depth in
    let refine_ out = Pooled_copy.instantiate saved scope base checked_depth p t in
    let refine_ p = p in let refine_ checked_depth = checked_depth in
    let value = out.#value in let state = out.#state in let pool = out.#pool in let trail = out.#trail in
    let epoch = ghost_ out.#epoch in let history = ghost_ out.#history in
    ghost_ (let u = () in extends_def history history;
      target_below_at saved depth bounds epoch history history p value (refine_ u);
      copy_ordered saved depth bounds order epoch history value (refine_ u));
    let out = #{value; state; pool; trail; epoch; history} in refine_ out
