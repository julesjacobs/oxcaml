module type Payload = sig
  type t : value mod portable contended
  type model : immutable_data
  val snapshot : t @ local immutable -> model @ immutable total ghost @@ total
end

module Make (V : Payload) = struct
  type t : immutable_data
  type contents : immutable_data = V.model option
  type 'a step = { value : 'a; state : Ghost_pref.token @@ ghost }

  external location : t @ local immutable -> contents Ghost_pref.t @ immutable ghost
    @@ total portable = "caml_unique_cell_location"

  external create : (value : V.t) @ unique -> (token : Ghost_pref.token) @ unique ghost ->
    {r : t step | not (Ghost_pref.Heap.mem (Ghost_pref.own token) (location r.value))
      && Ghost_pref.own r.state === Ghost_pref.Heap.put (Ghost_pref.own token)
        (location r.value) (Some (V.snapshot value))} @ unique @@ portable = "caml_unique_cell_create_bytecode" "caml_unique_cell_create"

  external take : (cell : t) ->
    (token : {t : Ghost_pref.token | match Ghost_pref.Heap.at (Ghost_pref.own t) (location cell) with
      | Some (Some _) -> true | _ -> false}) @ unique ghost ->
    {r : V.t step | Ghost_pref.Heap.at (Ghost_pref.own token) (location cell) === Some (Some (V.snapshot r.value))
      && Ghost_pref.own r.state === Ghost_pref.Heap.put (Ghost_pref.own token) (location cell) None} @ unique @@ portable = "caml_unique_cell_take_bytecode" "caml_unique_cell_take"

  external put : (cell : t) -> (value : V.t) @ unique ->
    (token : {t : Ghost_pref.token | Ghost_pref.Heap.at (Ghost_pref.own t) (location cell) === Some None}) @ unique ghost ->
    {t : Ghost_pref.token | Ghost_pref.own t === Ghost_pref.Heap.put (Ghost_pref.own token) (location cell) (Some (V.snapshot value))} @ unique ghost @@ portable = "caml_unique_cell_put_bytecode" "caml_unique_cell_put"

  external replace : (cell : t) -> (value : V.t) @ unique ->
    (token : {t : Ghost_pref.token | match Ghost_pref.Heap.at (Ghost_pref.own t) (location cell) with
      | Some (Some _) -> true | _ -> false}) @ unique ghost ->
    {r : V.t step | Ghost_pref.Heap.at (Ghost_pref.own token) (location cell) === Some (Some (V.snapshot r.value))
      && Ghost_pref.own r.state === Ghost_pref.Heap.put (Ghost_pref.own token) (location cell) (Some (V.snapshot value))} @ unique @@ portable = "caml_unique_cell_replace_bytecode" "caml_unique_cell_replace"
end

module Slot = struct
  type ('a : value mod portable contended) t : immutable_data
  type 'a step = { value : 'a; state : Ghost_pref.token @@ ghost }
  external location : 'a t @ local immutable -> bool Ghost_pref.t @ immutable ghost
    @@ total = "caml_unique_cell_location"
  external empty : ('a : value mod portable contended).
    unit -> (token : Ghost_pref.token) @ unique ghost ->
    {r : 'a t step |
      not (Ghost_pref.Heap.mem (Ghost_pref.own token) (location r.value)) &&
      Ghost_pref.own r.state === Ghost_pref.Heap.put (Ghost_pref.own token)
        (location r.value) false} @ unique @@ portable = "caml_unique_cell_create_bytecode" "caml_unique_cell_create"
  external put : ('a : value mod portable contended).
    (cell : 'a t) -> 'a @ unique ->
    (token : {t : Ghost_pref.token |
      Ghost_pref.Heap.at (Ghost_pref.own t) (location cell) === Some false})
      @ unique ghost ->
    {t : Ghost_pref.token | Ghost_pref.own t ===
      Ghost_pref.Heap.put (Ghost_pref.own token) (location cell) true}
      @ unique ghost @@ portable = "caml_unique_cell_put_bytecode" "caml_unique_cell_put"
  external take : ('a : value mod portable contended).
    (cell : 'a t) ->
    (token : {t : Ghost_pref.token |
      Ghost_pref.Heap.at (Ghost_pref.own t) (location cell) === Some true})
      @ unique ghost ->
    {r : 'a step | Ghost_pref.own r.state ===
      Ghost_pref.Heap.put (Ghost_pref.own token) (location cell) false}
      @ unique @@ portable = "caml_unique_cell_take_bytecode" "caml_unique_cell_take"
end
