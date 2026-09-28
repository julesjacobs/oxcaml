type ('a : value mod separable) t : immutable_data with 'a

val base : ('a : immutable_data).
  'a t @ immutable -> 'a iarray @ immutable total @@ total
val length : ('a : value mod separable).
  'a t @ immutable -> int @@ total
val lookup : ('a : value mod separable).
  int -> 'a t @ total -> 'a option @ total @@ total
val get : ('a : value mod separable).
  (overlay : 'a t) -> {i : int | 0 <= i && i < length overlay} ->
  'a @ total @@ total
val empty : ('a : value mod separable).
  'a iarray @ total -> 'a t @ total @@ total
val set : ('a : value mod separable).
  int -> 'a @ total -> 'a t @ total -> 'a t @ total @@ total
val clear : ('a : value mod separable).
  int -> 'a t @ total -> 'a t @ total @@ total

module Laws (Element : sig type t : immutable_data end) : sig
  val length_equation : (overlay : Element.t t) ->
    {u : unit | length overlay = Iarray.length (base overlay)} @@ total
  val get_lookup : (overlay : Element.t t) ->
    (index : {i : int | 0 <= i && i < length overlay}) ->
    {u : unit | let i = index in
      lookup i overlay === Some (get overlay index)} @@ total
  val lookup_outside : (overlay : Element.t t) -> (index : int) ->
    {u : unit | if index < 0 || length overlay <= index then
      lookup index overlay === None else true} @@ total
  val empty_base : (values : Element.t iarray) ->
    {u : unit | base (empty values) === values} @@ total
  val empty_lookup : (values : Element.t iarray) -> (index : int) ->
    {u : unit | lookup index (empty values) ===
      Vox_iarray.at values index} @@ total
  val set_base : (overlay : Element.t t) -> (index : int) ->
    (value : Element.t) ->
    {u : unit | base (set index value overlay) === base overlay} @@ total
  val set_lookup : (overlay : Element.t t) -> (index : int) ->
    (value : Element.t) -> (query : int) ->
    {u : unit | lookup query (set index value overlay) ===
      (if 0 <= query && query < length overlay && query = index then
        Some value else lookup query overlay)} @@ total
  val clear_base : (overlay : Element.t t) -> (index : int) ->
    {u : unit | base (clear index overlay) === base overlay} @@ total
  val clear_lookup : (overlay : Element.t t) -> (index : int) ->
    (query : int) ->
    {u : unit | lookup query (clear index overlay) ===
      (if query = index then Vox_iarray.at (base overlay) query
       else lookup query overlay)} @@ total
end
