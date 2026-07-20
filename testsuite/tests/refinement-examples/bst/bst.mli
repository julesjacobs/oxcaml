type t

val empty : t @@ total
val insert : int -> t @ logical -> t @@ total
val member : int -> t @ local logical -> bool @@ total
val equal : t @ local logical -> t @ local logical -> bool @@ total

val empty_law :
  key:int ->
  unit{ member key empty = false } @@ total

val insert_law :
  inserted:int ->
  tree:t @ logical ->
  query:int ->
  unit{
    member query (insert inserted tree)
    = ((inserted = query) || member query tree)
  } @@ total

val equal_implies_member :
  t1:t @ logical ->
  t2:t @ logical ->
  query:int ->
  unit{
    equal t1 t2 = false || member query t1 = member query t2
  } @@ total
