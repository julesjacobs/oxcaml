module type SET = sig
  type t
  val empty  : t @@ total
  val insert : int -> t @ logical -> t @@ total
  val member : int -> t @ local logical -> bool @@ total
  val equal  : t @ local logical -> t @ local logical -> bool @@ total
  val empty_law  : query:int -> unit{ member query empty = false } @@ total
  val insert_law : inserted:int -> tree:t @ logical -> query:int ->
    unit{ member query (insert inserted tree) = ((query = inserted) || member query tree) } @@ total
  val equal_forward_law : t1:t @ logical -> t2:t @ logical ->
    equal_trees:unit{ equal t1 t2 = true } -> query:int ->
    unit{ member query t1 = member query t2 } @@ total
  val equal_backward_law : t1:t @ logical -> t2:t @ logical ->
    pointwise:(query:int -> unit{ member query t1 = member query t2 }) @ total ->
    unit{ equal t1 t2 = true } @@ total
end
