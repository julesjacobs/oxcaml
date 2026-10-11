(** A mutable hash table specified by its finite map of bindings.
    Reads borrow a permission; mutations consume it and return its replacement.
    Permissions and snapshots are erased. Contracts describe normal return. *)

module type Key = sig
  include Map.EquatableType
  val hash : t -> int @@ total
  val hash_equal : (x : t) -> (y : t) ->
    {u : unit | not (equal x y) || hash x = hash y} @@ total
end

module Make (Key : Key) : sig
  module Model : module type of Map.MakeLogical (Key)
    with type 'a t = 'a Map.MakeLogical(Key).t

  (** An ordinary, aliasable table handle. *)
  type ('a : immutable_data) t : logical_data with 'a

  (** Permission to access one table, carrying its current bindings. *)
  type ('a : immutable_data) permission : void mod logical with 'a
  val owner : 'a permission @ local immutable -> 'a t @ ghost @@ total
  val bindings : 'a permission @ local immutable -> 'a Model.t @ ghost @@ total

  type 'a created = #{
    table : 'a t @@ aliased;
    permission : 'a permission;
  }

  val create : unit ->
    {r : 'a created | owner r.#permission === r.#table &&
      bindings r.#permission === Model.empty ()} @ unique

  val length :
    (table : 'a t) ->
    (p : {p : 'a permission | owner p === table}) @ local read ->
    {n : int | Bigint.of_int n = Model.cardinal (bindings p)}

  val find_opt :
    (table : 'a t) -> (key : Key.t) ->
    (p : {p : 'a permission | owner p === table}) @ local read ->
    {value : 'a option | value === Model.find_opt key (bindings p)}

  (** Raises [Not_found] if [key] has no binding. *)
  val find :
    (table : 'a t) -> (key : Key.t) ->
    (p : {p : 'a permission | owner p === table}) @ local read ->
    {value : 'a | Model.find_opt key (bindings p) === Some value}

  val mem :
    (table : 'a t) -> (key : Key.t) ->
    (p : {p : 'a permission | owner p === table}) @ local read ->
    {present : bool | present = Model.mem key (bindings p)}

  val replace :
    (table : 'a t) -> (key : Key.t) -> (value : 'a) ->
    (p : {p : 'a permission | owner p === table}) @ unique ->
    {q : 'a permission | owner q === table &&
      bindings q === Model.add key value (bindings p)} @ unique

  val remove :
    (table : 'a t) -> (key : Key.t) ->
    (p : {p : 'a permission | owner p === table}) @ unique ->
    {q : 'a permission | owner q === table &&
      bindings q === Model.remove key (bindings p)} @ unique

  val clear :
    (table : 'a t) ->
    (p : {p : 'a permission | owner p === table}) @ unique ->
    {q : 'a permission | owner q === table &&
      bindings q === Model.empty ()} @ unique
end
