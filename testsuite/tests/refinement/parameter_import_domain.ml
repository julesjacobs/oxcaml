type t = int

external equal : t @ local logical -> t @ local logical -> bool @@ total
  = "%equal"

external consume :
  left:t @ logical -> right:t @ logical ->
  witness:unit{ equal left right = true } -> unit @@ total
  = "parameter_import_domain_consume"
