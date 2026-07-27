type t : immutable_data = { major : int; minor : int }

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
