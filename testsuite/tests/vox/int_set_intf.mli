(** Membership laws shared by the set interfaces. *)
module type Operations = sig
  type t : logical_data

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

(** Sets with the same members compare [equal], independently of representation.
    [size] is nonnegative, increases exactly when [add] inserts a new member,
    and agrees for equal sets. *)
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

  val size_nonnegative : (set : t) ->
    {u : unit | 0Z <= size set} @@ total

  val size_add : (element : int) -> (set : t) ->
    {u : unit | size (add element set) ===
      (if lookup element set then size set else Bigint.add (size set) 1Z)}
    @@ total

  val equal_size : (left : t) -> (right : t) ->
    {u : unit | if equal left right then size left === size right else true}
    @@ total
end

(** Stronger interface for representations whose logical equality is determined
    by membership. AVL sets implement [Extensional]. *)
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
