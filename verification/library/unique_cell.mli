@@ portable

module type Payload = sig
  type t : value mod portable contended
  type model : immutable_data
  val snapshot : t @ local immutable -> model @ immutable total ghost @@ total
end

module Make (V : Payload) : sig @@ portable
  type t : immutable_data
  type contents : immutable_data = V.model option
  type 'a step = { value : 'a; state : contents Ghost_pref.token @@ ghost }

  external location : t @ local immutable -> contents Ghost_pref.t @ immutable
    ghost
    @@ total = "caml_unique_cell_location"

  val create : (value : V.t) @ unique -> (token : contents Ghost_pref.token) @
    unique ghost ->
    {r : t step | not (Ghost_pref.Heap.mem (Ghost_pref.own token) (location
      r.value))
      && Ghost_pref.own r.state === Ghost_pref.Heap.put (Ghost_pref.own token)
        (location r.value) (Some (V.snapshot value))} @ unique

  val take : (cell : t) ->
    (token : {t : contents Ghost_pref.token | match Ghost_pref.Heap.at
      (Ghost_pref.own t) (location cell) with
      | Some (Some _) -> true | _ -> false}) @ unique ghost ->
    {r : V.t step | Ghost_pref.Heap.at (Ghost_pref.own token) (location cell) === Some (Some
        (V.snapshot r.value))
      && Ghost_pref.own r.state === Ghost_pref.Heap.put (Ghost_pref.own token)
        (location cell) None} @ unique

  val put : (cell : t) -> (value : V.t) @ unique ->
    (token : {t : contents Ghost_pref.token | Ghost_pref.Heap.at (Ghost_pref.own
      t) (location cell) === Some None}) @ unique ghost ->
    {t : contents Ghost_pref.token | Ghost_pref.own t === Ghost_pref.Heap.put (Ghost_pref.own token) (location
        cell) (Some (V.snapshot value))} @ unique ghost

  val replace : (cell : t) -> (value : V.t) @ unique ->
    (token : {t : contents Ghost_pref.token | match Ghost_pref.Heap.at
      (Ghost_pref.own t) (location cell) with
      | Some (Some _) -> true | _ -> false}) @ unique ghost ->
    {r : V.t step | Ghost_pref.Heap.at (Ghost_pref.own token) (location cell) === Some (Some
        (V.snapshot r.value))
      && Ghost_pref.own r.state === Ghost_pref.Heap.put (Ghost_pref.own token)
        (location cell) (Some (V.snapshot value))} @ unique
end

(** Polymorphic ownership transfer with a Boolean occupancy model. Unlike
    [Make], this interface records no payload snapshot; payload refinements
    are retained by the type parameter. These are trusted contracts for the
    same C storage and move operations used by [Make]. *)
module Slot : sig
  type ('a : value mod portable contended) t : immutable_data
  type 'a step = { value : 'a; state : bool Ghost_pref.token @@ ghost }
  external location : 'a t @ local immutable -> bool Ghost_pref.t @ immutable
    ghost
    @@ total = "caml_unique_cell_location"
  val empty : ('a : value mod portable contended).
    unit -> (token : bool Ghost_pref.token) @ unique ghost ->
    {r : 'a t step |
      not (Ghost_pref.Heap.mem (Ghost_pref.own token) (location r.value)) &&
      Ghost_pref.own r.state === Ghost_pref.Heap.put (Ghost_pref.own token)
        (location r.value) false} @ unique
  val put : ('a : value mod portable contended).
    (cell : 'a t) -> 'a @ unique ->
    (token : {t : bool Ghost_pref.token |
      Ghost_pref.Heap.at (Ghost_pref.own t) (location cell) === Some false})
      @ unique ghost ->
    {t : bool Ghost_pref.token | Ghost_pref.own t ===
      Ghost_pref.Heap.put (Ghost_pref.own token) (location cell) true}
      @ unique ghost
  val take : ('a : value mod portable contended).
    (cell : 'a t) ->
    (token : {t : bool Ghost_pref.token |
      Ghost_pref.Heap.at (Ghost_pref.own t) (location cell) === Some true})
      @ unique ghost ->
    {r : 'a step | Ghost_pref.own r.state ===
      Ghost_pref.Heap.put (Ghost_pref.own token) (location cell) false}
      @ unique
end
