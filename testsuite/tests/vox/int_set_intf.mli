module type Operations = sig
  type t : immutable_data

  val empty : t
  val lookup : int -> t -> bool @@ total
  val add : int -> t -> t @@ total
  val union : t -> t -> t @@ total
  val size : t -> Bigint.t @@ total

  val lookup_empty :
    (element : int) ->
    {u : unit | lookup element empty === false} @@ total

  val lookup_add :
    (element : int) ->
    (added : int) ->
    (set : t) ->
    {u : unit |
      lookup element (add added set)
      === (element = added || lookup element set)} @@ total

  val lookup_union :
    (element : int) ->
    (left : t) ->
    (right : t) ->
    {u : unit |
      lookup element (union left right)
      === (lookup element left || lookup element right)} @@ total
end

module type Canonical = sig
  include Operations

  val size_zero :
    (set : t) ->
    {u : unit | (size set === 0Z) === (set === empty)} @@ total

  val extensional :
    (left : t) ->
    (right : t) ->
    ((element : int) ->
      {u : unit | lookup element left === lookup element right}) @ total ->
    {u : unit | left === right} @@ total
end

module type Extensional = sig
  include Operations

  val equal : t -> t -> bool @@ total

  val equal_lookup :
    (left : t) ->
    (right : t) ->
    (element : int) ->
    {u : unit |
      if equal left right
      then lookup element left === lookup element right
      else true} @@ total

  val extensional :
    (left : t) ->
    (right : t) ->
    ((element : int) ->
      {u : unit | lookup element left === lookup element right}) @ total ->
    {u : unit | equal left right === true} @@ total

  val size_zero :
    (set : t) ->
    {u : unit | (size set === 0Z) === equal set empty} @@ total
end
