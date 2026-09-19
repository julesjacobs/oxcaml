val route_pools :
  (h : Pref.heap Ghost.t) @ immutable ->
  (pending : Generalize_spec.pool) @ immutable ->
  (pools' : {a : Generalize_spec.pool Borrow_iarray.Owned_array.t
              | Level_pool_routing_spec.routable h.Ghost.ghost
                  (Iarray.length (Borrow_iarray.Owned_array.contents a))
                  pending}) @ unique ->
  {t : Pref.token | (Pref.own t) === h.Ghost.ghost} @ local read ->
  {a : Generalize_spec.pool Borrow_iarray.Owned_array.t
    | let refine_ pools = pools' in
      (Borrow_iarray.Owned_array.contents a) ===
        (Level_pool_routing_spec.route h.Ghost.ghost pending
           (Borrow_iarray.Owned_array.contents pools))} @ unique

type closed = #{
  state : Pref.token;
  pools : Generalize_spec.pool Borrow_iarray.Owned_array.t;
}

val close_and_route :
  (h : Pref.heap Ghost.t) @ immutable ->
  (cut : int) ->
  (child : Generalize_spec.pool) @ immutable ->
  {t : Pref.token
    | ((Pref.own t) === h.Ghost.ghost) &&
        (Generalize_spec.pool_scoped h.Ghost.ghost child)} @ unique ->
  (pools' : {a : Generalize_spec.pool Borrow_iarray.Owned_array.t
              | (0 <= cut) &&
                  (cut <
                     (Iarray.length (Borrow_iarray.Owned_array.contents a)))}) @ unique ->
  {r : closed
    | let refine_ pools = pools' in
      ((Pref.own r.#state) ===
         (Representative_pool_spec.close_heap h.Ghost.ghost cut child))
        &&
        ((Borrow_iarray.Owned_array.contents r.#pools) ===
           (Level_pool_routing_spec.route
              (Representative_pool_spec.close_heap h.Ghost.ghost cut child)
              (Representative_pool_spec.transfer_rep
                 (Representative_pool_spec.close_heap h.Ghost.ghost cut child)
                 child Generalize_spec.Empty)
              (Borrow_iarray.Owned_array.contents pools)))} @ unique

val save :
  (i : int) ->
  (pending : Generalize_spec.pool) @ immutable ->
  (pools' : {a : Generalize_spec.pool Borrow_iarray.Owned_array.t
              | (0 <= i) &&
                  (i < (Iarray.length (Borrow_iarray.Owned_array.contents a)))}) @ unique
                    ->
  {a : Generalize_spec.pool Borrow_iarray.Owned_array.t
    | let refine_ pools = pools' in
      (Borrow_iarray.Owned_array.contents a) ===
        (Vox_iarray.updated (Borrow_iarray.Owned_array.contents pools) i
           pending)} @ unique

type taken = #{
  pending : Generalize_spec.pool @@ aliased;
  pools : Generalize_spec.pool Borrow_iarray.Owned_array.t;
}

val take :
  (i : int) ->
  (pools' : {a : Generalize_spec.pool Borrow_iarray.Owned_array.t
              | (0 <= i) &&
                  (i < (Iarray.length (Borrow_iarray.Owned_array.contents a)))}) @ unique
                    ->
  {r : taken
    | let refine_ pools = pools' in
      (r.#pending ===
         (Level_pool_routing_spec.bucket
            (Borrow_iarray.Owned_array.contents pools) i))
        &&
        ((Borrow_iarray.Owned_array.contents r.#pools) ===
           (Vox_iarray.updated (Borrow_iarray.Owned_array.contents pools) i
              Generalize_spec.Empty))} @ unique
