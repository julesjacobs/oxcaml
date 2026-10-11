module Partition = Vox_partition_classes
module E = Vox_union_find_events

module Make (C : Vox_big_credits.S) : sig
  type elem : logical_data
  type t : ((void & void & void & void & void & void) & void & void) mod logical
  type result = #{value : elem @@ aliased; state : t}
  type 'a observation =
    t @ local immutable total ghost forkable unyielding -> 'a @ ghost

  (** The pure partition represented by the owned mutable nodes. *)
  val model : elem Partition.t observation @@ total

  (** Cost observations describe the forest, including changes that preserve
      [model]. They are independent of the pure partition operations. *)
  module Cost : sig @@ total
    type snapshot : logical_data
    val snapshot : snapshot observation
    val depth : snapshot -> elem -> Bigint.t @ ghost
    val compressed : snapshot -> elem -> snapshot @ ghost
    val depth_law : (p : snapshot) -> (x : elem) ->
      {u : unit | depth p x >= 0Z} @ ghost
    val root_depth :
      (state : t) @ local immutable total ghost forkable unyielding ->
      (x : elem) ->
      {u : unit | if Partition.contains (model state) x then
        (depth (snapshot state) x = 0Z) =
          (Partition.representative (model state) x === x) else true} @ ghost

    val ticks : Bigint.t observation
    val events : E.event list observation
    val account : Bigint.t observation
    val find_fee : Bigint.t observation
    val union_fee : Bigint.t observation
    val event_cost :
      (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | ticks state = E.total (events state)} @ ghost
    val account_bounds :
      (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | ticks state <= account state} @ ghost
    val observations :
      (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | 0Z <= Partition.size (model state) &&
        Partition.size (model state) <= Bigint.of_int max_int &&
        find_fee state >= 12Z && union_fee state >= 36Z} @ ghost
    val fee_bounds :
      (state : t) @ local immutable total ghost forkable unyielding ->
      (population : Bigint.t) -> (a : Bigint.t) ->
      {u : unit | if 1Z <= population &&
        Partition.size (model state) <= population &&
        1Z <= a && Vox_ackermann.iter population a 1Z 1Z >= population &&
        Vox_ackermann.below population a
        then find_fee state <= Bigint.add (Bigint.mul 4Z a) 12Z &&
          union_fee state <= Bigint.add (Bigint.mul 12Z a) 36Z else true}
      @ ghost
  end

  val create : (fee : {b : C.token | C.credits b = 1Z}) @ unique ghost ->
    {s : t | Partition.empty (model s) &&
      Cost.account s = 1Z && Cost.events s === [E.Initialize]} @ unique

  val make_set :
    (state : {s : t | Partition.size (model s) < Bigint.of_int max_int})
      @ unique ->
    (fee : {b : C.token | C.credits b = 11Z}) @ unique ghost ->
    {r : result | Partition.added (model state) (model r.#state) r.#value &&
      Cost.account r.#state = Bigint.add (Cost.account state) 11Z &&
      Cost.events r.#state === E.Allocate :: Cost.events state} @ unique

  val find : (x : elem) ->
    (state : {s : t | Partition.contains (model s) x})
      @ unique ->
    (fee : {b : C.token | C.credits b = Cost.find_fee state})
      @ unique ghost ->
    {r : result | r.#value === Partition.representative (model state) x &&
      Partition.same (model state) (model r.#state) &&
      Cost.account r.#state =
        Bigint.add (Cost.account state) (Cost.find_fee state) &&
      Cost.snapshot r.#state === Cost.compressed (Cost.snapshot state) x &&
      Cost.events r.#state ===
        E.Find (Cost.depth (Cost.snapshot state) x) :: Cost.events state}
    @ unique

  val union : (x : elem) -> (y : elem) ->
    (state : {s : t | Partition.contains (model s) x &&
      Partition.contains (model s) y}) @ unique ->
    (fee : {b : C.token | C.credits b = Cost.union_fee state})
      @ unique ghost ->
    {r : result |
      Partition.joined (model state) (model r.#state) x y r.#value &&
      Cost.account r.#state =
        Bigint.add (Cost.account state) (Cost.union_fee state) &&
      Cost.events r.#state === E.Union :: E.Link ::
        E.Find (Cost.depth (Cost.compressed (Cost.snapshot state) x) y) ::
        E.Find (Cost.depth (Cost.snapshot state) x) :: Cost.events state}
    @ unique
end
