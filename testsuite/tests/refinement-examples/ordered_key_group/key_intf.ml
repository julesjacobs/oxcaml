(* An arbitrary key type with a comparison, and a set over it.

   For an [int] key the comparison is a primitive the solver interprets: it
   knows on its own that [<] is a strict total order.  Here [compare] is an
   uninterpreted function, and everything the solver may assume about it is
   the three laws below, each of which has to be instantiated by hand at the
   pair or triple where it is needed.  Trichotomy is the one thing that does
   come free, because [compare] returns an [int] and a value is negative,
   zero or positive whatever [compare] means. *)
module type ORDERED_KEY = sig
  type t : immutable_data

  (* A ground value of the key type.  Mentioning it is how a proof tells the
     solver the abstract type is inhabited. *)
  val witness : t

  val compare :
    t @ local logical -> t @ local logical -> int @@ total

  val compare_zero_iff_equal :
    left:t @ logical -> right:t @ logical ->
    unit{ (compare left right = 0) = (left = right) } @@ total

  val compare_sign_reversal :
    left:t @ logical -> right:t @ logical ->
    unit{
      (compare left right < 0) = (compare right left > 0)
    } @@ total

  val compare_negative_transitive :
    first:t @ logical -> second:t @ logical -> third:t @ logical ->
    unit{
      not (compare first second < 0)
      || not (compare second third < 0)
      || compare first third < 0
    } @@ total
end

(* The integer-set interface of [set_group], over an arbitrary key.  The
   representation invariant is abstract: a client learns only that [empty]
   satisfies it and that [insert] preserves it.  [member] descends a single
   spine in the three trees, so [insert_law] is false without it. *)
module type SET = sig
  type key : immutable_data
  type t

  val key_witness : key

  val empty : t @@ total
  val insert : key @ logical -> t @ logical -> t @@ total
  val member :
    key @ local logical -> t @ local logical -> bool @@ total
  val equal :
    t @ local logical -> t @ local logical -> bool @@ total

  val invariant : t @ local logical -> bool @@ total

  val empty_invariant : unit{ invariant empty = true } @@ total

  val insert_invariant :
    inserted:key @ logical ->
    set:t @ logical ->
    well_formed:unit{ invariant set = true } ->
    unit{ invariant (insert inserted set) = true } @@ total

  val empty_law :
    query:key @ logical ->
    unit{ member query empty = false } @@ total

  val insert_law :
    inserted:key @ logical ->
    set:t @ logical ->
    query:key @ logical ->
    well_formed:unit{ invariant set = true } ->
    unit{
      member query (insert inserted set)
      = ((query = inserted) || member query set)
    } @@ total

  val equal_forward_law :
    left:t @ logical ->
    right:t @ logical ->
    equal_sets:unit{ equal left right = true } ->
    query:key @ logical ->
    unit{ member query left = member query right } @@ total

  val equal_backward_law :
    left:t @ logical ->
    right:t @ logical ->
    pointwise:
      (query:key @ logical ->
       unit{ member query left = member query right }) @ total ->
    unit{ equal left right = true } @@ total
end
