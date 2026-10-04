(** A mutable flat hash table observed as a finite map of bindings.
    Keys have kind [logical_data]; values have kind [immutable_data].

    Handles may be aliased. An erased [view] records the bindings and storage
    version. Reads borrow an ownership token for that version; mutations
    consume it and return a new view and token. [current] defines this access
    requirement. Contracts describe normal return; a mutation that raises
    consumes its token. *)

module P = Ghost_pref
module H = P.Heap

(** Keys: [equal] must be an equivalence and equal keys must hash equally.
    Both functions are [total] and stateless: they terminate without
    raising, and their results depend only on their arguments. *)
module type Key = sig
  type t : logical_data
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
  (** {1 Finite-map model} *)

  (** Finite maps from keys to values, specified only by the laws below.
      [count] is the number of bindings. [===] compares maps as values:
      maps with the same bindings built by different updates need not be
      equal, so compare them through [lookup] and [count]. *)
  module Map : sig
    type ('a : immutable_data) t : logical_data with 'a
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
    val lookup_equal : ('a : immutable_data).
      (map : 'a t) -> (key : Key.t) -> (query : Key.t) ->
      {u : unit | not (Key.equal key query) ||
        lookup map key === lookup map query} @ ghost @@ total
    val count_nonnegative : ('a : immutable_data).
      (map : 'a t) -> {u : unit | 0Z <= count map} @ ghost @@ total
  end

  (** {1 Table snapshots and ownership} *)

  type ('a : immutable_data) t : logical_data with 'a
  type ('a : immutable_data) state : logical_data
    with 'a @@ global many total immutable
  type ('a : immutable_data) view : void mod total logical with 'a

  val location : ('a : immutable_data). 'a t -> 'a state P.t @ ghost @@ total
  val version : ('a : immutable_data).
    'a view @ immutable -> 'a state @ ghost @@ total
  val bindings : ('a : immutable_data).
    'a view @ immutable -> 'a Map.t @ ghost @@ total
  val capacity : ('a : immutable_data).
    'a view @ immutable -> {n : int | 16 <= n && n <= 1073741824} @ ghost @@ total

  (** [heap] holds [version view] at [location table]. Transparent: the
      verifier unfolds it by [current_def] wherever it is applied. *)
  val current : ('a : immutable_data).
    'a t -> 'a view @ immutable -> 'a state P.heap -> bool @ ghost @@ total
    [@@def transparent]
  val current_def : ('a : immutable_data).
    (table : 'a t) -> (view : 'a view) @ immutable ->
    (heap : 'a state P.heap) ->
    {u : unit | current table view heap ===
      ghost_ (H.at heap (location table) === Some (version view))} @@ total

  type ('a : immutable_data) created = #{
    table : 'a t @@ aliased;
    view : 'a view @@ aliased immutable;
    token : 'a state P.token @@ ghost;
  }
  type ('a : immutable_data) updated = #{
    view : 'a view @@ aliased immutable;
    token : 'a state P.token @@ ghost;
  }

  (** {1 Operations} *)

  (** A new empty table of capacity 16 at a fresh location. *)
  val create : ('a : immutable_data).
    (token : 'a state P.token) @ unique ghost ->
    {r : 'a created | bindings r.#view === Map.empty && capacity r.#view = 16 &&
      not (H.mem (P.own token) (location r.#table)) &&
      P.own r.#token === H.put (P.own token) (location r.#table) (version r.#view)} @ unique

  val length : ('a : immutable_data).
    (table : 'a t) -> (view : 'a view) @ immutable ->
    (token : {t : 'a state P.token | current table view (P.own t)})
      @ local read ghost ->
    {n : int | 0 <= n && Bigint.of_int n = Map.count (bindings view)}

  val find_opt : ('a : immutable_data).
    (table : 'a t) -> (view : 'a view) @ immutable -> (key : Key.t) ->
    (token : {t : 'a state P.token | current table view (P.own t)})
      @ local read ghost ->
    {value : 'a option | value === Map.lookup (bindings view) key}

  (** Raises [Not_found] if [key] has no binding. *)
  val find : ('a : immutable_data).
    (table : 'a t) -> (view : 'a view) @ immutable -> (key : Key.t) ->
    (token : {t : 'a state P.token | current table view (P.own t)})
      @ local read ghost ->
    {value : 'a | Map.lookup (bindings view) key === Some value}

  val mem : ('a : immutable_data).
    (table : 'a t) -> (view : 'a view) @ immutable -> (key : Key.t) ->
    (token : {t : 'a state P.token | current table view (P.own t)})
      @ local read ghost ->
    {present : bool | present = (match Map.lookup (bindings view) key with
      | None -> false | Some _ -> true)}

  (** Raises [Invalid_argument] if the table would need more than 2{^30}
      slots. *)
  val replace : ('a : immutable_data).
    (table : 'a t) -> (before : 'a view) @ immutable -> (key : Key.t) ->
    (value : 'a) ->
    (token : {t : 'a state P.token | current table before (P.own t)})
      @ unique read_write ghost ->
    {r : 'a updated | bindings r.#view === Map.put (bindings before) key value &&
      P.own r.#token === H.put (P.own token) (location table) (version r.#view)} @ unique

  val remove : ('a : immutable_data).
    (table : 'a t) -> (before : 'a view) @ immutable -> (key : Key.t) ->
    (token : {t : 'a state P.token | current table before (P.own t)})
      @ unique read_write ghost ->
    {r : 'a updated | bindings r.#view === Map.erase (bindings before) key &&
      P.own r.#token === H.put (P.own token) (location table) (version r.#view)} @ unique

  (** Removes every binding and drops the table's references to their
      values. The capacity is unchanged. *)
  val clear : ('a : immutable_data).
    (table : 'a t) -> (before : 'a view) @ immutable ->
    (token : {t : 'a state P.token | current table before (P.own t)})
      @ unique read_write ghost ->
    {r : 'a updated | bindings r.#view === Map.empty &&
      capacity r.#view = capacity before &&
      P.own r.#token === H.put (P.own token) (location table) (version r.#view)} @ unique
end
