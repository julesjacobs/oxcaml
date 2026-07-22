type t

external equal : t @ local logical -> t @ local logical -> bool @@ total
  = "parameter_import_equal"

external member : int -> t @ local logical -> bool @@ total
  = "parameter_import_member"

external consume :
  left:t @ logical -> right:t @ logical ->
  witness:unit{ equal left right = true } -> query:int ->
  unit{ member query left = member query right } @@ total
  = "parameter_import_consume"

let apply ~(left : t @ logical) ~(right : t @ logical)
    ~(witness : unit{ equal left right = true }) ~(query : int) =
  consume ~left ~right ~witness ~query
