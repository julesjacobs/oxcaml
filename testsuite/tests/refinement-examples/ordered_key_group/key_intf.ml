(* An arbitrary key type with a comparison, and a set over it.

   For an [int] key the comparison is a primitive the solver interprets: it
   knows on its own that [<] is a strict total order.  Here [compare] is an
   uninterpreted function, and everything the solver may assume about it is
   the three laws below, each of which has to be instantiated by hand at the
   pair or triple where it is needed.  Trichotomy is the one thing that does
   come free, because [compare] returns an [int] and a value is negative,
   zero or positive whatever [compare] means.

   All three laws are load-bearing.  Weakening any one of them to
   [unit{ true }] stops the four ordered implementations verifying;
   [gen_ulist] needs only [compare_zero_iff_equal], since a unique list
   compares for equality and never for order. *)
module type ORDERED_KEY = sig
  type t : immutable_data

  (* A ground value of the key type.  Mentioning it is how a proof tells the
     solver the abstract type is inhabited, which five proofs below have to
     do by hand.  A functor-parameter type is not treated as inhabited on
     its own, the way [iarray] is; if a declared witness were enough to mark
     it so, those five mentions could go. *)
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
   satisfies it and that [insert] preserves it.

   How much of that invariant the interface actually forces was measured by
   weakening it, one implementation at a time, to [fun _ -> true].  Three of
   the five then stop verifying: [gen_avl], [gen_rbt] and [gen_sorted] all
   stop membership on the comparison, so they cannot prove [insert_law]
   about a set they know nothing about.  The other two still seal.
   [gen_bst]'s membership also descends one spine, but its proof happens not
   to need the ordering, and [gen_ulist] scans the whole list.  So the
   invariant is load-bearing where searching depends on it and decoration
   elsewhere, and it is exported for all five because a search structure
   without its invariant is not that structure, not because the interface
   extracts it. *)
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
