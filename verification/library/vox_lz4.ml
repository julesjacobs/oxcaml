module C = Vox_lz4_streaming_codec
module D = Vox_lz4_string_decode
module V = Vox_string_view
module R = Vox_lz4_streaming_roundtrip

let max_block_size : {n : int | n = 4194304} = 4194304

type malformed = D.malformed =
  | Empty_block
  | Truncated_length
  | Truncated_literals
  | Truncated_offset
  | Zero_offset
  | Offset_beyond_output
  | Invalid_terminal_sequence

type decode_error = D.decode_error =
  | Malformed of malformed * int
  | Output_limit
  | Invalid_capacity

let compress : (source : string) ->
    {wire : string | Vox_lz4_spec.compresses source wire} =
  fun source -> C.compress_string source

let decompress_verified : (wire : string) ->
    (capacity : {n : int | 0 <= n && n <= 4194304}) ->
    {decoded : D.decoded | Vox_lz4_spec.matches_model wire capacity decoded} =
  fun wire capacity -> D.decode_string wire capacity

let decompress ?(capacity = max_block_size) wire =
  if capacity < 0 || capacity > max_block_size then Error Invalid_capacity
  else
    let decoded = decompress_verified wire capacity in
    decoded

let compress_decompress : (source : string) ->
    {output : string | V.contents output === V.contents source} =
  fun source ->
    let wire = compress source in
    let capacity = V.length source in
    if capacity > max_block_size then invalid_arg "Vox_lz4: block too large"
    else
      let decoded = decompress_verified wire capacity in
      ghost_ (R.roundtrip source wire capacity decoded);
      match decoded with
      | Ok output -> output
      | Error _ -> assert false

let (roundtrip @ total) = R.roundtrip
