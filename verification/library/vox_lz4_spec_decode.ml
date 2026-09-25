(* The inverse clause states the byte-preserving semantics of Char.code. *)
external char_of_byte : Raw_memory.byte -> char @@ total = "%identity"
external byte_of_char : (c : char) ->
  {byte : Raw_memory.byte | char_of_byte byte === c} @@ total = "%identity"

let (token_high @ total) :
    (token : Raw_memory.byte) -> {shift : int | shift = 4} ->
  {high : int | 0 <= high && high <= 15
      && 16 * high <= token && token < 16 * (high + 1)} =
  fun token _ ->
    if token < 128 then
      if token < 64 then
        if token < 32 then (if token < 16 then 0 else 1)
        else if token < 48 then 2 else 3
      else if token < 96 then
        if token < 80 then 4 else 5
      else if token < 112 then 6 else 7
    else if token < 192 then
      if token < 160 then (if token < 144 then 8 else 9)
      else if token < 176 then 10 else 11
    else if token < 224 then
      if token < 208 then 12 else 13
    else if token < 240 then 14 else 15

let (high4 @ total) : (token : Raw_memory.byte) ->
    {high : int | 0 <= high && high <= 15
      && 16 * high <= token && token < 16 * (high + 1)} =
  fun token -> token_high token 4

let (token_low @ total) :
    (token : Raw_memory.byte) -> {mask : int | mask = 15} ->
    {low : int | 0 <= low && low <= 15
      && token = 16 * high4 token + low} =
  fun token _ -> token - 16 * high4 token

let (low15 @ total) : (token : Raw_memory.byte) ->
    {low : int | 0 <= low && low <= 15
      && token = 16 * high4 token + low} =
  fun token -> token_low token 15

let[@def] rec (copy_heap @ total) (h : Ghost_pref.heap @ immutable)
    (block : Raw_memory.t @ immutable) (used : int) (distance : int)
    (remaining : int) = ghost_ (
  if remaining <= 0 then h
  else
    match Ghost_pref.Heap.at h (Raw_memory.location block (used - distance)) with
    | Some (Some value) ->
      copy_heap (Ghost_pref.Heap.put h (Raw_memory.location block used) (Some value))
        block (used + 1) distance (remaining - 1)
    | _ -> h)
[@@decreases remaining]

let[@def] rec (literal_heap @ total) (h : Ghost_pref.heap @ immutable)
    (block : Raw_memory.t @ immutable) (used : int)
    (source : char iarray @ immutable) (first : int) (remaining : int) =
  ghost_ (
    if remaining <= 0 || first < 0 || first >= Iarray.length source then h
    else
      let value = byte_of_char (Vox_sequence.iarray_get source first) in
      literal_heap (Ghost_pref.Heap.put h (Raw_memory.location block used) (Some value))
        block (used + 1) source (first + 1) (remaining - 1))
[@@decreases remaining]

type length_result =
  | Length of int * int
  | Length_truncated
  | Length_limit

let[@def] rec (read_extended_length @ total) :
    (source : char iarray) ->
    (cursor : {i : int | 0 <= i && i <= Iarray.length source}) ->
    (length : {n : int | 0 <= n && n <= 4194304}) ->
    (fuel : {f : int | 0 <= f}) ->
    length_result =
  fun source cursor length fuel ->
    if fuel = 0 || cursor = Iarray.length source then Length_truncated
    else
      let extension = byte_of_char (Vox_sequence.iarray_get source cursor) in
      if extension > 4194304 - length then Length_limit
      else
        let length = length + extension in
        let cursor = cursor + 1 in
        if extension = 255 then
          read_extended_length source cursor length (fuel - 1)
        else Length (cursor, length)
[@@decreases fuel]

let[@def] (read_length @ total) : (source : char iarray) ->
    (cursor : {i : int | 0 <= i && i <= Iarray.length source}) ->
    (initial : {n : int | 0 <= n && n <= 15}) -> length_result =
  fun source cursor initial ->
  if initial < 15 then Length (cursor, initial)
  else read_extended_length source cursor 15 (Iarray.length source - cursor)

type status = Done | Malformed | Output_limit

type model_result = {
  kind : status;
  count : int;
  state : Ghost_pref.heap @@ ghost;
}

let[@def] rec (decode_model @ total) (source : char iarray @ immutable)
    (input_pos : int) (last_match_start : int) (fuel : int)
    (capacity : int) (block : Raw_memory.t @ immutable) (used : int)
    (state : Ghost_pref.heap @ immutable) : model_result @ ghost = ghost_ (
  let n = Iarray.length source in
  if input_pos < 0 || input_pos >= n || fuel <= 0 then
    { kind = Malformed; count = used; state }
  else
    let token = byte_of_char (Vox_sequence.iarray_get source input_pos) in
    let after_token = input_pos + 1 in
    match read_length source after_token (high4 token) with
    | Length_truncated -> { kind = Malformed; count = used; state }
    | Length_limit -> { kind = Output_limit; count = used; state }
    | Length (literal_pos, literal_count) ->
      if literal_pos < 0 || literal_pos > n || literal_count < 0
         || literal_count > n - literal_pos then
        { kind = Malformed; count = used; state }
      else if literal_count > capacity - used then
        { kind = Output_limit; count = used; state }
      else
        let after_literals = literal_pos + literal_count in
        if after_literals = n then
          if low15 token <> 0
             || (last_match_start >= 0
                 && (literal_count < 5
                     || last_match_start > used + literal_count - 12))
          then { kind = Malformed; count = used; state }
          else
            { kind = Done; count = used + literal_count;
              state = literal_heap state block used source literal_pos
                        literal_count }
        else if n - after_literals < 2 then
          { kind = Malformed; count = used; state }
        else
          let low = byte_of_char (Vox_sequence.iarray_get source after_literals) in
          let high = byte_of_char (Vox_sequence.iarray_get source (after_literals + 1)) in
          let distance = low + high * 256 in
          if distance <= 0 || distance > used + literal_count then
            { kind = Malformed; count = used; state }
          else
            match read_length source (after_literals + 2)
                    (low15 token) with
            | Length_truncated -> { kind = Malformed; count = used; state }
            | Length_limit -> { kind = Output_limit; count = used; state }
            | Length (next_pos, match_code) ->
              if next_pos < 0 || next_pos > n || match_code < 0
                 || match_code > capacity - used - literal_count - 4 then
                { kind = Output_limit; count = used; state }
              else
                let state = literal_heap state block used source literal_pos
                              literal_count in
                let current = used + literal_count in
                let state = copy_heap state block current distance
                              (match_code + 4) in
                decode_model source next_pos current (fuel - 1) capacity
                  block (current + match_code + 4) state)
[@@decreases fuel]
