val member : int -> int list @ local logical -> bool @@ total
val insert : int -> int list @ logical -> int list @@ total
val sorted_unique : int list @ local logical -> bool @@ total

val empty_member_law :
  query:int -> unit{ member query [] = false } @@ total

val member_cons_law :
  query:int ->
  value:int ->
  rest:int list @ logical ->
  unit{
    member query (value :: rest)
    = ((query = value) || member query rest)
  } @@ total

val insert_member_law :
  inserted:int ->
  values:int list @ logical ->
  query:int ->
  unit{
    member query (insert inserted values)
    = ((query = inserted) || member query values)
  } @@ total
