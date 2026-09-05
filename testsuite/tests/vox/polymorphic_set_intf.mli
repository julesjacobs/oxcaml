module type Ordered = sig
  type t : immutable_data mod total

  val compare : t -> t -> int @@ total

  val compare_reflexive :
    (x : t) ->
    {u : unit | compare x x = 0} @@ total

  val compare_reverse :
    (x : t) ->
    (y : t) ->
    {u : unit |
      (compare x y <= 0) === (compare y x >= 0)} @@ total

  val compare_transitive :
    (x : t) ->
    (y : t) ->
    (z : t) ->
    {u : unit |
      if compare x y <= 0 && compare y z <= 0
      then compare x z <= 0
      else true} @@ total
end

module type S = sig
  module Element : Ordered

  type elt = Element.t
  type t : immutable_data

  val empty : t
  val lookup : elt -> t -> bool @@ total
  val add : elt -> t -> t @@ total
  val union : t -> t -> t @@ total

  val size : t -> Bigint.t @@ total
  val equal : t -> t -> bool @@ total

  val lookup_empty :
    (element : elt) ->
    {u : unit | lookup element empty === false} @@ total

  val lookup_add :
    (element : elt) ->
    (added_element : elt) ->
    (set : t) ->
    {u : unit |
      lookup element (add added_element set)
      ===
      (Element.compare element added_element = 0 || lookup element set)}
      @@ total

  val lookup_union :
    (element : elt) ->
    (left : t) ->
    (right : t) ->
    {u : unit |
      lookup element (union left right)
      === (lookup element left || lookup element right)} @@ total

  val size_zero :
    (set : t) ->
    {u : unit | (size set === 0Z) === equal set empty} @@ total

  val equal_lookup :
    (left : t) ->
    (right : t) ->
    (element : elt) ->
    {u : unit |
      if equal left right
      then lookup element left === lookup element right
      else true} @@ total

  val extensional :
    (left : t) ->
    (right : t) ->
    ((element : elt) ->
      {u : unit | lookup element left === lookup element right}) @ total ->
    {u : unit | equal left right === true} @@ total
end
