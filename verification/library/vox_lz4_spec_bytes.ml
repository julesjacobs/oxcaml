let (literal_token @ total) :
    (length : {n : int | 0 <= n && n <= 4194304}) ->
    {token : Raw_memory.byte | token = (if length >= 15 then 240 else length * 16)} =
  fun length ->
  if length >= 15 then 240 else (length * 16)

external same_char : (left : char) -> (right : char) ->
  {same : bool | same = (left === right)} @@ total = "%eq"

let[@def] (source_at @ total) (source : char iarray @ immutable)
    (index : int) =
  if index < 0 || index >= Iarray.length source then None
  else Some (Vox_sequence.iarray_get source index)

let[@def] rec (extension_count @ total) :
    (remaining : {n : int | 0 <= n && n <= 4194304}) ->
    {count : int | 1 <= count && count <= remaining + 1
      && 255 * count <= remaining + 255} =
  fun remaining ->
  if remaining >= 255 then 1 + extension_count (remaining - 255)
  else 1
[@@decreases remaining]

let[@def] rec (extension_bytes @ total) (wire : char iarray @ immutable)
    (cursor : int) (remaining : int) = ghost_ (
  if remaining < 0 then false
  else if remaining >= 255 then
    (match source_at wire cursor with
     | Some c -> Vox_lz4_spec_decode.byte_of_char c = 255
     | None -> false)
    && extension_bytes wire (cursor + 1) (remaining - 255)
  else
    match source_at wire cursor with
    | Some c -> Vox_lz4_spec_decode.byte_of_char c = remaining
    | None -> false)
[@@decreases remaining]

let[@def] rec (literal_bytes @ total) (wire : char iarray @ immutable)
    (cursor : int) (source : char iarray @ immutable) (first : int)
    (remaining : int) = ghost_ (
  if remaining <= 0 then true
  else
    (match source_at wire cursor, source_at source first with
     | Some c, Some s -> Vox_lz4_spec_decode.byte_of_char c = Vox_lz4_spec_decode.byte_of_char s
     | _ -> false)
    && literal_bytes wire (cursor + 1) source (first + 1)
         (remaining - 1))
[@@decreases remaining]

let[@def] (wire_byte @ total) (wire : char iarray @ immutable)
    (position : int) (value : Raw_memory.byte) = ghost_ (
  match source_at wire position with
  | Some c -> Vox_lz4_spec_decode.byte_of_char c = value
  | None -> false)

let[@def] rec (prefix_matches @ total) (values : char iarray @ immutable)
    (heap : Ghost_pref.heap @ immutable) (block : Raw_memory.t @ immutable) (count : int) =
  ghost_ (
    if count <= 0 then true
    else
      (match Vox_iarray.at values (count - 1), Ghost_pref.Heap.at heap (Raw_memory.location block (count - 1)) with
       | Some c, Some (Some byte) -> Vox_lz4_spec_decode.byte_of_char c = byte
       | _ -> false)
      && prefix_matches values heap block (count - 1))
[@@decreases count]
