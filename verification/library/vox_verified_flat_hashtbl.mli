(** A flat hash table verified against an abstract finite map.

    [Make (Key)] is a mutable open-addressing hash table that probes in
    groups of sixteen slots with SIMD control-byte matching. Each operation's
    contract states its result in terms of [Map], an abstract finite map
    from keys (up to [Key.equal]) to values. Keys and values must be
    [immutable_data]. In contracts, [===] is logical equality at any type
    and [=] is ordinary equality on integers.

    {2 Ghost state}

    A table is a runtime handle [table : 'a t] together with ghost state,
    which the compiler checks and then erases:

    - [location table] is the ghost heap location at which tokens record
      this table's storage. It stays the same for the table's lifetime,
      including when the storage is rebuilt. The handle itself is immutable
      and may be aliased.
    - A view ['a view] is an immutable snapshot of one version of the table.
      [bindings view] is its contents, [capacity view] its slot count and
      [version view] its exact storage state.
    - A token ['a state P.token] is affine ownership of a heap. Reads borrow
      a token whose heap holds [version view] at [location table]. Mutations
      consume the token and return a new view and a token whose heap
      differs only at [location table]. [create] takes only a token.

    A view grants no access by itself: an operation accepts it only with a
    token whose heap holds its [version]. After a mutation, use the returned
    view. Views and tokens carry no runtime data.

    {2 Normal return and exceptions}

    Each postcondition describes a normal return; exceptional behaviour is
    not checked. By the code:

    - [find] raises [Not_found] if the key has no binding.
    - [replace] raises [Invalid_argument] if the table would need more than
      2{^30} slots.
    - Any operation can raise [Out_of_memory] or [Stack_overflow], and
      asynchronous exceptions can arrive as usual.

    A mutation that raises loses the token it consumed, together with any
    other ownership that token carried: the table can no longer be used.
    Split off ownership that must survive before the call. Termination,
    running time, memory reclamation and concurrent use are not specified. *)

module P = Ghost_pref
module H = P.Heap

(** Keys: [equal] must be an equivalence and equal keys must hash equally.
    Both functions are [total], so they terminate without raising or
    touching mutable state. *)
module type Key = sig
  type t : immutable_data
  val equal : t -> t -> bool @@ total
  val hash : t -> int @@ total
  val reflexive : (x : t) -> {u : unit | equal x x} @@ total
  val symmetric : (x : t) -> (y : t) -> {u : unit | equal x y = equal y x}
    @@ total
  val transitive : (x : t) -> (y : t) -> (z : t) ->
    {u : unit | not (equal x y && equal y z) || equal x z} @@ total
  val hash_equal : (x : t) -> (y : t) ->
    {u : unit | not (equal x y) || hash x = hash y} @@ total
end

module Make (Key : Key) : sig
  (** Finite maps from keys to values, specified only by the laws below.
      [count] is the number of bindings. [===] compares maps as values:
      maps with the same bindings built by different updates need not be
      equal, so compare them through [lookup] and [count]. *)
  module Map : sig
    type ('a : immutable_data) t : immutable_data
    val count : ('a : immutable_data). 'a t -> Bigint.t @@ total
    val empty : ('a : immutable_data). {map : 'a t | count map = 0Z} @@ total
    val lookup : ('a : immutable_data). 'a t -> Key.t -> 'a option @@ total
    val put : ('a : immutable_data). 'a t -> Key.t -> 'a -> 'a t @@ total
    val erase : ('a : immutable_data). 'a t -> Key.t -> 'a t @@ total

    val lookup_empty : ('a : immutable_data). (map : 'a t) -> (key : Key.t) ->
      {u : unit | not (map === empty) || lookup map key === None} @ ghost
      @@ total
    val put_get : ('a : immutable_data).
      (map : 'a t) -> (key : Key.t) -> (value : 'a) -> (query : Key.t) ->
      {u : unit | lookup (put map key value) query ===
        (if Key.equal key query then Some value else lookup map query)} @ ghost
      @@ total
    val erase_get : ('a : immutable_data).
      (map : 'a t) -> (key : Key.t) -> (query : Key.t) ->
      {u : unit | lookup (erase map key) query ===
        (if Key.equal key query then None else lookup map query)} @ ghost
      @@ total
    val count_put : ('a : immutable_data).
      (map : 'a t) -> (key : Key.t) -> (value : 'a) ->
      {u : unit | count (put map key value) = (if lookup map key === None
        then Bigint.add (count map) 1Z else count map)} @ ghost @@ total
    val count_erase : ('a : immutable_data).
      (map : 'a t) -> (key : Key.t) ->
      {u : unit | count (erase map key) = (if lookup map key === None
        then count map else Bigint.sub (count map) 1Z)} @ ghost @@ total
  end

  type ('a : immutable_data) t : immutable_data
  type ('a : immutable_data) state : immutable_data
  type ('a : immutable_data) view : void mod total

  val location : ('a : immutable_data). 'a t -> 'a state P.t @ ghost @@ total
  val version : ('a : immutable_data).
    'a view @ immutable -> 'a state @ ghost @@ total
  val bindings : ('a : immutable_data).
    'a view @ immutable -> 'a Map.t @ ghost @@ total
  val capacity : ('a : immutable_data).
    'a view @ immutable -> {n : int | 16 <= n && n <= 1073741824} @ ghost @@ total

  type ('a : immutable_data) created = #{
    table : 'a t @@ aliased;
    view : 'a view @@ aliased immutable;
    token : 'a state P.token @@ ghost;
  }
  type ('a : immutable_data) updated = #{
    view : 'a view @@ aliased immutable;
    token : 'a state P.token @@ ghost;
  }

  (** A new empty table of capacity 16 at a fresh location. *)
  val create : ('a : immutable_data).
    (token : 'a state P.token) @ unique ghost ->
    {r : 'a created | bindings r.#view === Map.empty && capacity r.#view = 16 &&
      not (H.mem (P.own token) (location r.#table)) &&
      P.own r.#token === H.put (P.own token) (location r.#table) (version r.#view)} @ unique

  val length : ('a : immutable_data).
    (table : 'a t) -> (view : 'a view) @ immutable ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (version view)}) @ local read ghost ->
    {n : int | 0 <= n && Bigint.of_int n = Map.count (bindings view)}

  val find_opt : ('a : immutable_data).
    (table : 'a t) -> (view : 'a view) @ immutable -> (key : Key.t) ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (version view)}) @ local read ghost ->
    {value : 'a option | value === Map.lookup (bindings view) key}

  (** Raises [Not_found] if [key] has no binding. *)
  val find : ('a : immutable_data).
    (table : 'a t) -> (view : 'a view) @ immutable -> (key : Key.t) ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (version view)}) @ local read ghost ->
    {value : 'a | Map.lookup (bindings view) key === Some value}

  val mem : ('a : immutable_data).
    (table : 'a t) -> (view : 'a view) @ immutable -> (key : Key.t) ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (version view)}) @ local read ghost ->
    {present : bool | present = (match Map.lookup (bindings view) key with
      | None -> false | Some _ -> true)}

  (** Raises [Invalid_argument] if the table would need more than 2{^30}
      slots. *)
  val replace : ('a : immutable_data).
    (table : 'a t) -> (before : 'a view) @ immutable -> (key : Key.t) ->
    (value : 'a) ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (version before)}) @ unique read_write ghost ->
    {r : 'a updated | bindings r.#view === Map.put (bindings before) key value &&
      P.own r.#token === H.put (P.own token) (location table) (version r.#view)} @ unique

  val remove : ('a : immutable_data).
    (table : 'a t) -> (before : 'a view) @ immutable -> (key : Key.t) ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (version before)}) @ unique read_write ghost ->
    {r : 'a updated | bindings r.#view === Map.erase (bindings before) key &&
      P.own r.#token === H.put (P.own token) (location table) (version r.#view)} @ unique

  (** Removes every binding and drops the table's references to their
      values. The capacity is unchanged. *)
  val clear : ('a : immutable_data).
    (table : 'a t) -> (before : 'a view) @ immutable ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (version before)}) @ unique read_write ghost ->
    {r : 'a updated | bindings r.#view === Map.empty &&
      capacity r.#view = capacity before &&
      P.own r.#token === H.put (P.own token) (location table) (version r.#view)} @ unique
end
