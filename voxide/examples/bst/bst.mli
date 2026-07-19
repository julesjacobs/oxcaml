type t

val empty : t @@ total
val insert : int @ logical -> t @ logical -> t @@ total
val member : int @ logical -> t @ local logical -> bool @@ total

(* These proof tokens state equations about the actual abstract operations.
   They remain usable by a separately compiled client and are checked again at
   the implementation/interface seal. *)
val empty_has_no_zero :
  unit @ logical -> unit{ member 0 empty = false } @@ total
val empty_has_no_one :
  unit @ logical -> unit{ member 1 empty = false } @@ total
val insert_zero_has_zero :
  unit @ logical -> unit{ member 0 (insert 0 empty) = true } @@ total
val insert_zero_has_no_one :
  unit @ logical -> unit{ member 1 (insert 0 empty) = false } @@ total
val insert_one_preserves_zero :
  unit @ logical ->
  unit{ member 0 (insert 1 (insert 0 empty)) = true } @@ total
