module P = Ghost_pref
module H = P.Heap

module type Key = sig
  type t : immutable_data
  val equal : t @ immutable -> t @ immutable -> bool @@ total
  val hash : t @ immutable -> int @@ total
  val reflexive : (x : t) @ immutable -> {u : unit | equal x x} @@ total
  val symmetric : (x : t) @ immutable -> (y : t) @ immutable ->
    {u : unit | equal x y = equal y x} @@ total
  val transitive : (x : t) @ immutable -> (y : t) @ immutable ->
    (z : t) @ immutable ->
    {u : unit | not (equal x y && equal y z) || equal x z} @@ total
  val hash_equal : (x : t) @ immutable -> (y : t) @ immutable ->
    {u : unit | not (equal x y) || hash x = hash y} @@ total
end

(** Pure, total, stable hash/equality on immutable keys are required.
    All executable contracts describe normal return. Allocation can fail;
    mutation exceptions consume the passed authority after possible mutation.
    Split unrelated ownership before fallible operations to retain that frame.
    There is no time-cost or exception-safe reclamation theorem. *)
module Make (Key : Key) : sig
  module Map : sig
    type ('a : immutable_data) t = (Key.t * 'a) list
    val lookup : ('a : immutable_data).
      'a t @ immutable -> Key.t @ immutable -> 'a option @ immutable @@ total
    val lookup_def : ('a : immutable_data).
      (entries : 'a t) @ immutable -> (key : Key.t) @ immutable ->
      {u : unit | lookup entries key === (match entries with
        | [] -> None
        | (k, value) :: tail ->
          if Key.equal k key then Some value else lookup tail key)} @ ghost @@ total
    val distinct : ('a : immutable_data). 'a t @ immutable -> bool @ ghost @@ total
    val distinct_def : ('a : immutable_data). (bindings : 'a t) @ immutable ->
      {u : unit | distinct bindings === (ghost_ (match bindings with
        | [] -> true | (key, _) :: tail -> lookup tail key === None && distinct tail))} @ ghost @@ total
    val agrees : ('a : immutable_data).
      'a t @ immutable -> 'a t @ immutable -> bool @ ghost @@ total
    val agrees_def : ('a : immutable_data).
      (left : 'a t) @ immutable -> (right : 'a t) @ immutable ->
      {u : unit | agrees left right === (ghost_ (match left with
        | [] -> true
        | (key, value) :: tail ->
          lookup right key === Some value && agrees tail right))} @ ghost @@ total
    val same : ('a : immutable_data).
      'a t @ immutable -> 'a t @ immutable -> bool @ ghost @@ total
    val same_def : ('a : immutable_data).
      (left : 'a t) @ immutable -> (right : 'a t) @ immutable ->
      {u : unit | same left right === (ghost_ (agrees left right && agrees right left))} @ ghost @@ total
    val erase : ('a : immutable_data).
      'a t @ immutable -> Key.t @ immutable -> 'a t @ immutable @@ total
    val erase_def : ('a : immutable_data).
      (entries : 'a t) @ immutable -> (key : Key.t) @ immutable ->
      {u : unit | erase entries key === (match entries with
        | [] -> []
        | (stored, value) :: tail ->
          if Key.equal stored key then erase tail key
          else (stored, value) :: erase tail key)} @ ghost @@ total
    val put : ('a : immutable_data).
      'a t @ immutable -> Key.t @ immutable -> 'a @ immutable -> 'a t @ immutable @@ total
    val put_def : ('a : immutable_data).
      (entries : 'a t) @ immutable -> (key : Key.t) @ immutable ->
      (value : 'a) @ immutable ->
      {u : unit | put entries key value === (key, value) :: erase entries key} @ ghost @@ total
    val same_get : ('a : immutable_data).
      (left : 'a t) @ immutable -> (right : 'a t) @ immutable ->
      (key : Key.t) @ immutable ->
      {u : unit | not (same left right) || lookup left key === lookup right key} @ ghost @@ total
    val erase_get : ('a : immutable_data).
      (entries : 'a t) @ immutable -> (key : Key.t) @ immutable ->
      (query : Key.t) @ immutable ->
      {u : unit | lookup (erase entries key) query ===
        (if Key.equal key query then None else lookup entries query)} @ ghost @@ total
    val put_get : ('a : immutable_data).
      (entries : 'a t) @ immutable -> (key : Key.t) @ immutable ->
      (value : 'a) @ immutable -> (query : Key.t) @ immutable ->
      {u : unit | lookup (put entries key value) query ===
        (if Key.equal key query then Some value else lookup entries query)} @ ghost @@ total
  end

  val count : ('a : immutable_data). 'a Map.t @ immutable -> Bigint.t @@ total
  val count_def : ('a : immutable_data). (entries : 'a Map.t) @ immutable ->
    {u : unit | count entries === (match entries with | [] -> 0Z
      | _ :: tail -> Bigint.add 1Z (count tail))} @ ghost @@ total

  type ('a : immutable_data) t : immutable_data
  type ('a : immutable_data) state : immutable_data
  type ('a : immutable_data) view : void mod total

  (** A view is an immutable snapshot. Its abstract state identifies the exact
      owned storage version; bindings are its finite-map observation. No view
      or saved observation grants ownership. *)
  val location : ('a : immutable_data).
    'a t @ immutable -> 'a state P.t @ immutable ghost @@ total
  val model : ('a : immutable_data).
    'a view @ immutable -> 'a state @ immutable ghost @@ total
  val bindings : ('a : immutable_data).
    'a view @ immutable -> {entries : 'a Map.t | Map.distinct entries} @ immutable ghost @@ total
  val capacity : ('a : immutable_data).
    'a view @ immutable -> {n : int | 16 <= n && n <= 1073741824} @ ghost @@ total

  type ('a : immutable_data) created = {
    table : 'a t @@ aliased;
    view : 'a view @@ aliased immutable;
    state : 'a state P.token @@ ghost;
  }
  type ('a : immutable_data) result = #{
    view : 'a view @@ aliased immutable;
    state : 'a state P.token @@ ghost;
  }

  val create : ('a : immutable_data).
    (token : 'a state P.token) @ unique ghost ->
    {r : 'a created | bindings r.view === [] && capacity r.view = 16 &&
      not (H.mem (P.own token) (location r.table)) &&
      P.own r.state === H.put (P.own token) (location r.table) (model r.view)} @ unique

  val length : ('a : immutable_data).
    (table : 'a t) @ immutable -> (view : 'a view) @ immutable ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (model view)}) @ local read ghost ->
    {n : int | 0 <= n && Bigint.of_int n = count (bindings view)}

  val find_opt : ('a : immutable_data).
    (table : 'a t) @ immutable -> (view : 'a view) @ immutable ->
    (key : Key.t) @ immutable ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (model view)}) @ local read ghost ->
    {value : 'a option | value === Map.lookup (bindings view) key} @ immutable

  (** Raises [Not_found] for an absent key. *)
  val find : ('a : immutable_data).
    (table : 'a t) @ immutable -> (view : 'a view) @ immutable ->
    (key : Key.t) @ immutable ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (model view)}) @ local read ghost ->
    {value : 'a | Map.lookup (bindings view) key === Some value} @ immutable

  val mem : ('a : immutable_data).
    (table : 'a t) @ immutable -> (view : 'a view) @ immutable ->
    (key : Key.t) @ immutable ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (model view)}) @ local read ghost ->
    {present : bool | present = (match Map.lookup (bindings view) key with
      | None -> false | Some _ -> true)}

  (** Mutations preserve the same handle location and every other owned region.
      Rebuilding may replace backing blocks, preserving all other bindings. *)
  val replace : ('a : immutable_data).
    (table : 'a t) @ immutable -> (before : 'a view) @ immutable ->
    (key : Key.t) @ immutable -> (value : 'a) @ immutable ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (model before)}) @ unique read_write ghost ->
    {r : 'a result | Map.same (bindings r.#view) (Map.put (bindings before) key value) &&
      P.own r.#state === H.put (P.own token) (location table) (model r.#view)} @ unique

  val remove : ('a : immutable_data).
    (table : 'a t) @ immutable -> (before : 'a view) @ immutable ->
    (key : Key.t) @ immutable ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (model before)}) @ unique read_write ghost ->
    {r : 'a result | Map.same (bindings r.#view) (Map.erase (bindings before) key) &&
      P.own r.#state === H.put (P.own token) (location table) (model r.#view)} @ unique

  (** Clear bindings and retained payloads, preserving allocated capacity. *)
  val clear : ('a : immutable_data).
    (table : 'a t) @ immutable -> (before : 'a view) @ immutable ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (model before)}) @ unique read_write ghost ->
    {r : 'a result | bindings r.#view === [] &&
      capacity r.#view = capacity before &&
      P.own r.#state === H.put (P.own token) (location table) (model r.#view)} @ unique
end
