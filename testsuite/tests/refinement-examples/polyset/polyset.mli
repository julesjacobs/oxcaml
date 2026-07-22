module type ORDERED_KEY = sig
  type t : immutable_data

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

  val empty_law :
    query:key @ logical ->
    unit{ member query empty = false } @@ total

  val insert_law :
    inserted:key @ logical -> set:t @ logical -> query:key @ logical ->
    unit{
      member query (insert inserted set)
      = ((query = inserted) || member query set)
    } @@ total

  val equal_left_to_right :
    left:t @ logical -> right:t @ logical -> query:key @ logical ->
    unit{
      equal left right = false
      || member query left = false
      || member query right = true
    } @@ total

  val equal_right_to_left :
    left:t @ logical -> right:t @ logical -> query:key @ logical ->
    unit{
      equal left right = false
      || member query right = false
      || member query left = true
    } @@ total

  val equal_forward_law :
    left:t @ logical -> right:t @ logical ->
    query:key @ logical ->
    equal_sets:unit{ equal left right = true } ->
    unit{ member query left = member query right } @@ total

  val equal_backward_law :
    left:t @ logical -> right:t @ logical ->
    pointwise:
      (query:key @ logical ->
       unit{ member query left = member query right }) @ total ->
    unit{ equal left right = true } @@ total
end

module Make (K : ORDERED_KEY) : SET with type key = K.t
