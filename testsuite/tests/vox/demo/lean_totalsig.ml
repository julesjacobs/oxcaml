[@@@warning "-6"]
let apply_step :
      (r : ((int -> int -> bool) [@vox.total])) ->
      (f : ((x : int) -> int{ rHolds r x _ })) ->
      (x : int) -> int{ rHolds r x _ } =
  fun r f x -> ignore r; f x
