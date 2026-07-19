type t

val empty : t @@ total
val insert : int @ logical -> t @ logical -> t @@ total
val member : int @ logical -> t @ local logical -> bool @@ total

val empty_law :
  key:int @ logical ->
  unit{ member key empty = false } @@ total

val insert_law :
  key:int @ logical ->
  tree:t @ logical ->
  unit{ member key (insert key tree) = true } @@ total

val member_insert_law :
  inserted:int @ logical ->
  tree:t @ logical ->
  query:int @ logical ->
  unit{
    member query (insert inserted tree)
    = ((inserted = query) || member query tree)
  } @@ total
