type byte = {n : int | 0 <= n && n <= 255}

(* The inverse clause states the byte-preserving semantics of Char.code. *)
external char_of_byte : byte -> char @@ total = "%identity"
external byte_of_char : (c : char) ->
  {byte : byte | char_of_byte byte === c} @@ total = "%identity"

let (high4 @ total) : (token : byte) ->
    {high : int | 0 <= high && high <= 15
      && 16 * high <= token && token < 16 * (high + 1)} =
  fun token -> token lsr 4

let (low15 @ total) : (token : byte) ->
    {low : int | 0 <= low && low <= 15
      && token = 16 * high4 token + low} =
  fun token -> token land 15

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

