module Partition = Vox_partition_classes
module Bindings = Vox_partition
module Group = Vox_partition_classes_group
module Laws = Vox_partition_classes_proof
module E = Vox_union_find_events

module Make (C : Vox_big_credits.S) = struct
  module U = Vox_union_find_online.Make (C)
  type elem = U.elem
  type snapshot = U.snapshot
  type t = #{inner : {state : U.t | U.valid state &&
    Bindings.valid (U.partition (U.snapshot state))}}
  type result = #{value : elem @@ aliased; state : t}
  type 'a observation =
    t @ local immutable total ghost forkable unyielding -> 'a @ ghost

  let[@def] model (state : t @ local immutable total ghost forkable unyielding)
    :
      elem Partition.t @ immutable ghost =
    ghost_ (
      let p = U.partition (U.snapshot state.#inner) in
      Group.group_valid p; Group.group p)

  let (model_size @ total)
      (state : t @ local immutable total ghost forkable unyielding) :
      {u : unit | Partition.size (model state) = U.size state.#inner}
      @ ghost = ghost_ (
    model_def state; U.partition_size state.#inner;
    Group.group_size (U.partition (U.snapshot state.#inner)))

  let (model_contains @ total)
      (state : t @ local immutable total ghost forkable unyielding)
      (x : elem @ immutable) :
      {u : unit | Partition.contains (model state) x =
        U.contains (U.snapshot state.#inner) x} @ ghost = ghost_ (
    model_def state; U.partition_contains (U.snapshot state.#inner) x;
    Group.group_contains (U.partition (U.snapshot state.#inner)) x)

  let (model_root @ total)
      (state : t @ local immutable total ghost forkable unyielding)
      (x : elem @ immutable) :
      {u : unit | Partition.representative (model state) x ===
        U.root (U.snapshot state.#inner) x} @ ghost = ghost_ (
    model_def state; U.partition_root (U.snapshot state.#inner) x;
    Group.group_root (U.partition (U.snapshot state.#inner)) x)

  module Cost = struct
    type snapshot = U.snapshot
    let[@def] snapshot
        (state : t @ local immutable total ghost forkable unyielding) =
      ghost_ (U.snapshot state.#inner)
    let[@def] depth (p : snapshot @ immutable) (x : elem @ immutable) =
      ghost_ (U.depth p x)
    let[@def] compressed (p : snapshot @ immutable) (x : elem @ immutable) =
      ghost_ (U.compressed p x)
    let (depth_law @ total) (p : snapshot @ immutable) (x : elem @ immutable) :
        {u : unit | depth p x >= 0Z} @ ghost = ghost_ (
      depth_def p x; U.depth_law p x)

    let (root_depth @ total)
        (state : t @ local immutable total ghost forkable unyielding)
        (x : elem @ immutable) :
        {u : unit | if Partition.contains (model state) x then
          (depth (snapshot state) x = 0Z) =
            (Partition.representative (model state) x === x) else true} @ ghost
              =
      ghost_ (
        model_contains state x; model_root state x;
        snapshot_def state; depth_def (snapshot state) x;
        U.root_law state.#inner x)

    let[@def] ticks
        (state : t @ local immutable total ghost forkable unyielding) =
      ghost_ (U.ticks state.#inner)
    let[@def] events
        (state : t @ local immutable total ghost forkable unyielding) =
      ghost_ (U.events state.#inner)
    let[@def] account
        (state : t @ local immutable total ghost forkable unyielding) =
      ghost_ (U.account state.#inner)
    let[@def] find_fee
        (state : t @ local immutable total ghost forkable unyielding) =
      ghost_ (U.find_fee state.#inner)
    let[@def] union_fee
        (state : t @ local immutable total ghost forkable unyielding) =
      ghost_ (U.union_fee state.#inner)

    let (event_cost @ total)
        (state : t @ local immutable total ghost forkable unyielding) :
        {u : unit | ticks state = E.total (events state)} @ ghost = ghost_ (
      ticks_def state; events_def state; U.event_cost state.#inner)
    let (account_bounds @ total)
        (state : t @ local immutable total ghost forkable unyielding) :
        {u : unit | ticks state <= account state} @ ghost = ghost_ (
      ticks_def state; account_def state; U.account_bounds state.#inner)
    let (observations @ total)
        (state : t @ local immutable total ghost forkable unyielding) :
        {u : unit | 0Z <= Partition.size (model state) &&
          Partition.size (model state) <= Bigint.of_int max_int &&
          find_fee state >= 12Z && union_fee state >= 36Z} @ ghost =
      ghost_ (model_size state; find_fee_def state; union_fee_def state;
        U.observations state.#inner)
    let (fee_bounds @ total)
        (state : t @ local immutable total ghost forkable unyielding)
        (population : Bigint.t) (a : Bigint.t) :
        {u : unit | if 1Z <= population &&
          Partition.size (model state) <= population &&
          1Z <= a && Vox_ackermann.iter population a 1Z 1Z >= population &&
          Vox_ackermann.below population a
          then find_fee state <= Bigint.add (Bigint.mul 4Z a) 12Z &&
            union_fee state <= Bigint.add (Bigint.mul 12Z a) 36Z else true}
        @ ghost = ghost_ (
      model_size state; find_fee_def state; union_fee_def state;
      U.fee_bounds state.#inner population a)
  end

  let create : (fee : {b : C.token | C.credits b = 1Z}) @ unique total ghost ->
    {s : t | Partition.empty (model s) &&
      Cost.account s = 1Z && Cost.events s === [E.Initialize]} @ unique =
    fun fee ->
    let state = U.create_connectivity fee in
    ghost_ (U.partition_valid (borrow_ state));
    let state : t = #{inner = state} in
    ghost_ (Cost.account_def (borrow_ state);
      Cost.events_def (borrow_ state); model_def (borrow_ state);
      Bindings.empty_def ([] : elem Bindings.bindings);
      Group.group_empty ([] : elem Bindings.bindings));
    state

  let make_set :
    (state : {s : t | Partition.size (model s) < Bigint.of_int max_int})
      @ unique read_write total ->
    (fee : {b : C.token | C.credits b = 11Z}) @ unique total ghost ->
    {r : result | Partition.added (model state) (model r.#state) r.#value &&
      Cost.account r.#state = Bigint.add (Cost.account state) 11Z &&
      Cost.events r.#state === E.Allocate :: Cost.events state} @ unique =
    fun state fee ->
    ghost_ (model_size (borrow_ state));
    ghost_ (Cost.account_def (borrow_ state); Cost.snapshot_def (borrow_ state);
      Cost.events_def (borrow_ state));
    let before = ghost_ (
      model_def (borrow_ state); Cost.snapshot (borrow_ state)) in
    let result = U.make_set_connectivity state.#inner fee in
    let #{U.value; state = next} = result in
    ghost_ (U.partition_valid (borrow_ next));
    let next : t = #{inner = next} in
    ghost_ (Cost.account_def (borrow_ next);
      Cost.snapshot_def (borrow_ next); Cost.events_def (borrow_ next);
      model_def (borrow_ next); U.partition_added before (Cost.snapshot (borrow_
        next)) value;
      Bindings.added_intro (U.partition before) value;
      Group.group_added (U.partition before)
        (U.partition (Cost.snapshot (borrow_ next))) value);
    #{value; state = next}

  let find : (x : elem) @ immutable ->
    (state : {s : t | Partition.contains (model s) x})
      @ unique read_write total ->
    (fee : {b : C.token | C.credits b = Cost.find_fee state})
      @ unique total ghost ->
    {r : result | r.#value === Partition.representative (model state) x &&
      Partition.same (model state) (model r.#state) &&
      Cost.account r.#state =
        Bigint.add (Cost.account state) (Cost.find_fee state) &&
      Cost.snapshot r.#state === Cost.compressed (Cost.snapshot state) x &&
      Cost.events r.#state ===
        E.Find (Cost.depth (Cost.snapshot state) x) :: Cost.events state}
    @ unique =
    fun x state fee ->
    ghost_ (model_contains (borrow_ state) x; model_root (borrow_ state) x);
    ghost_ (Cost.events_def (borrow_ state);
      Cost.account_def (borrow_ state);
      Cost.snapshot_def (borrow_ state);
      Cost.find_fee_def (borrow_ state));
    let before = ghost_ (
      model_def (borrow_ state); Cost.snapshot (borrow_ state)) in
    let result = U.find_connectivity x state.#inner fee in
    let #{U.value; state = next} = result in
    ghost_ (U.partition_valid (borrow_ next));
    let next : t = #{inner = next} in
    ghost_ (Cost.account_def (borrow_ next);
      Cost.snapshot_def (borrow_ next);
      Cost.events_def (borrow_ next); Cost.depth_def before x;
        Cost.compressed_def before x;
      model_def (borrow_ next); U.partition_found before (Cost.snapshot (borrow_
        next)) x;
      Laws.same_refl (model (borrow_ next)));
    #{value; state = next}

  let union : (x : elem) @ immutable -> (y : elem) @ immutable ->
    (state : {s : t | Partition.contains (model s) x &&
      Partition.contains (model s) y}) @ unique read_write total ->
    (fee : {b : C.token | C.credits b = Cost.union_fee state})
      @ unique total ghost ->
    {r : result |
      Partition.joined (model state) (model r.#state) x y r.#value &&
      Cost.account r.#state =
        Bigint.add (Cost.account state) (Cost.union_fee state) &&
      Cost.events r.#state === E.Union :: E.Link ::
        E.Find (Cost.depth (Cost.compressed (Cost.snapshot state) x) y) ::
        E.Find (Cost.depth (Cost.snapshot state) x) :: Cost.events state}
    @ unique =
    fun x y state fee ->
    ghost_ (model_contains (borrow_ state) x; model_contains (borrow_ state) y);
    ghost_ (Cost.events_def (borrow_ state);
      Cost.account_def (borrow_ state);
      Cost.snapshot_def (borrow_ state);
      Cost.union_fee_def (borrow_ state));
    let before = ghost_ (
      model_def (borrow_ state); Cost.snapshot (borrow_ state)) in
    let result = U.union_connectivity x y state.#inner fee in
    let #{U.value; state = next} = result in
    ghost_ (U.partition_valid (borrow_ next));
    let next : t = #{inner = next} in
    ghost_ (Cost.account_def (borrow_ next);
      Cost.snapshot_def (borrow_ next);
      Cost.events_def (borrow_ next); Cost.depth_def before x;
        Cost.compressed_def before x;
      Cost.depth_def (Cost.compressed before x) y;
      model_def (borrow_ next); U.partition_joined before (Cost.snapshot
        (borrow_ next)) x y value;
      U.partition_contains before x; U.partition_contains before y;
      Bindings.joined_intro (U.partition before) x y value;
      Group.group_joined (U.partition before)
        (U.partition (Cost.snapshot (borrow_ next))) x y value);
    #{value; state = next}
end
