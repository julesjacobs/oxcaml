let[@def] rec (prefix_matches @ total) (values : char iarray @ immutable)
    (heap : Raw_memory.contents Ghost_pref.heap @ immutable) (block : Raw_memory.t @ immutable) (count : int) =
  ghost_ (
    if count <= 0 then true
    else
      (match Vox_iarray.at values (count - 1), Ghost_pref.Heap.at heap (Raw_memory.location block (count - 1)) with
       | Some c, Some (Some byte) -> Vox_lz4_spec_parse.byte_of_char c = byte
       | _ -> false)
      && prefix_matches values heap block (count - 1))
[@@decreases count]
