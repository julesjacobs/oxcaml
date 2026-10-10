let[@def] rec (initialized @ total) (h : Raw_memory.contents Ghost_pref.heap @ immutable)
    (p : Raw_memory.t @ immutable) (count : int) = ghost_ (
  if count <= 0 then true
  else
    (match Ghost_pref.Heap.at h (Raw_memory.location p (count - 1)) with
     | Some (Some _) -> true
     | _ -> false)
    && initialized h p (count - 1))
[@@decreases count]
