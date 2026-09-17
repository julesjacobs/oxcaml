open Copy_spec
open Generalize_spec
open Level_pool_routing_spec
open Borrow_iarray

let rec into_slice : (h : Pref.heap Ghost.t) @ immutable ->
    (pending : pool) @ immutable ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost}) @ local read ->
    (pools : {s : pool Slice.t |
      routable h.Ghost.ghost (Iarray.length (Slice.current s)) pending})
      @ local unique ->
    {u : unit | let refine_ s = pools in
      Slice.final s === route h.Ghost.ghost pending (Slice.current s)} =
  fun h pending state pools ->
    let refine_ state = state in
    let refine_ s = pools in
    let before = ghost_ (Slice.current (borrow_ s)) in
    ghost_ (let size = Iarray.length before in
      routable_def h.Ghost.ghost size pending;
      route_def h.Ghost.ghost pending before);
    match pending with
    | Empty -> let refine_ u = Slice.finish s in refine_ u
    | Entry (p, rest) ->
      let readable : {t : Pref.token | H.mem (Pref.own t) p} @ local read =
        refine_ state in
      let refine_ old = Pref.read p readable in
      let state : {t : Pref.token | Pref.own t === h.Ghost.ghost} @ local read =
        refine_ state in
      ghost_ (destination_def h.Ghost.ghost p);
      match old.desc, old.level with
      | Link _, _ | _, Generic ->
        let next : {s : pool Slice.t |
          routable h.Ghost.ghost (Iarray.length (Slice.current s)) rest} =
          refine_ s in
        let refine_ u = into_slice h rest state next in refine_ u
      | (Var | Bool | Arrow _), Finite level ->
        let i = if level < 0 then 0 else level in
        let index : {i : int | 0 <= i
          && i < Iarray.length (Slice.current s)} = refine_ i in
        let refine_ old_pool = Slice.get (borrow_ s) index in
        ghost_ (bucket_def before i; insert_def before i p);
        let next_pool = Entry (p, old_pool) in
        let refine_ next = Slice.set s index next_pool in
        ghost_ (insert_length before i p);
        let next : {s : pool Slice.t |
          routable h.Ghost.ghost (Iarray.length (Slice.current s)) rest} =
          refine_ next in
        let refine_ u = into_slice h rest state next in refine_ u

let route_pools : (h : Pref.heap Ghost.t) @ immutable ->
    (pending : pool) @ immutable ->
    (pools : {a : pool Owned_array.t |
      routable h.Ghost.ghost (Iarray.length (Owned_array.contents a)) pending})
      @ unique ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost}) @ local read ->
    {a : pool Owned_array.t | let refine_ pools = pools in
      Owned_array.contents a ===
        route h.Ghost.ghost pending (Owned_array.contents pools)} @ unique =
  fun h pending pools state ->
    let refine_ pools = pools in
    let before = ghost_ (Owned_array.contents (borrow_ pools)) in
    let post = ghost_ (fun (_ : unit @ immutable)
        (after : pool iarray @ total immutable) ->
      after === route h.Ghost.ghost pending before) in
    let refine_ result = Owned_array.with_mut pools post (fun s ->
      let refine_ s = s in
      let s : {s : pool Slice.t |
        routable h.Ghost.ghost (Iarray.length (Slice.current s)) pending} =
        refine_ s in
      let refine_ u = into_slice h pending state s in refine_ u) in
    let {value = _; state = pools} = result in refine_ pools

type closed = #{state : Pref.token; pools : pool Owned_array.t}

let close_and_route : (h : Pref.heap Ghost.t) @ immutable ->
    (cut : int) -> (child : pool) @ immutable ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost
      && pool_scoped h.Ghost.ghost child}) @ unique ->
    (pools : {a : pool Owned_array.t |
      0 <= cut && cut < Iarray.length (Owned_array.contents a)}) @ unique ->
    {r : closed | let refine_ pools = pools in
      Pref.own r.#state === Representative_pool_spec.close_heap h.Ghost.ghost cut child
      && Owned_array.contents r.#pools ===
        route (Representative_pool_spec.close_heap h.Ghost.ghost cut child)
          (Representative_pool_spec.transfer_rep
            (Representative_pool_spec.close_heap h.Ghost.ghost cut child)
            child Empty) (Owned_array.contents pools)} @ unique =
  fun h cut child state pools ->
    let refine_ state = state in let refine_ pools = pools in
    let empty = Empty in ghost_ (pool_scoped_def h.Ghost.ghost empty);
    let refine_ closed = Representative_pool.close_and_transfer
      h cut child empty (refine_ state) in
    let pending = closed.#parent in let state = closed.#state in
    let after : Pref.heap Ghost.t =
      {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
    ghost_ (let size = Iarray.length (Owned_array.contents (borrow_ pools)) in
      let u = () in closed_routable h.Ghost.ghost cut child size (refine_ u); ());
    let input : {a : pool Owned_array.t |
      routable after.Ghost.ghost (Iarray.length (Owned_array.contents a)) pending} =
      refine_ pools in
    let state : {t : Pref.token | Pref.own t === after.Ghost.ghost} =
      refine_ state in
    let refine_ pools = route_pools after pending input (borrow_ state) in
    let refine_ state = state in
    let out = #{state; pools} in refine_ out

let save : (i : int) -> (pending : pool) @ immutable ->
    (pools : {a : pool Owned_array.t |
      0 <= i && i < Iarray.length (Owned_array.contents a)}) @ unique ->
    {a : pool Owned_array.t | let refine_ pools = pools in
      Owned_array.contents a ===
        Vox_iarray.updated (Owned_array.contents pools) i pending} @ unique =
  fun i pending pools ->
    let refine_ pools = pools in
    let before = ghost_ (Owned_array.contents (borrow_ pools)) in
    let post = ghost_ (fun (_ : unit @ immutable)
        (after : pool iarray @ total immutable) ->
      after === Vox_iarray.updated before i pending) in
    let refine_ result = Owned_array.with_mut pools post (fun s ->
      let refine_ s = s in
      let index : {i : int | 0 <= i && i < Iarray.length (Slice.current s)} =
        refine_ i in
      let refine_ s = Slice.set s index pending in
      let refine_ u = Slice.finish s in refine_ u) in
    let {value = _; state = pools} = result in refine_ pools

type taken = #{pending : pool @@ aliased; pools : pool Owned_array.t}

let take : (i : int) ->
    (pools : {a : pool Owned_array.t |
      0 <= i && i < Iarray.length (Owned_array.contents a)}) @ unique ->
    {r : taken | let refine_ pools = pools in
      r.#pending === bucket (Owned_array.contents pools) i
      && Owned_array.contents r.#pools ===
        Vox_iarray.updated (Owned_array.contents pools) i Empty} @ unique =
  fun i pools ->
    let refine_ pools = pools in
    let before = ghost_ (Owned_array.contents (borrow_ pools)) in
    let empty = Empty in
    let post = ghost_ (fun (pending : pool @ immutable)
        (after : pool iarray @ total immutable) ->
      pending === bucket before i && after === Vox_iarray.updated before i empty) in
    let refine_ result = Owned_array.with_mut pools post (fun s ->
      let refine_ s = s in
      let index : {i : int | 0 <= i && i < Iarray.length (Slice.current s)} =
        refine_ i in
      let refine_ pending = Slice.get (borrow_ s) index in
      ghost_ (bucket_def before i);
      let refine_ s = Slice.set s index empty in
      let refine_ u = Slice.finish s in refine_ pending) in
    let {value = pending; state = pools} = result in
    let out = #{pending; pools} in refine_ out
