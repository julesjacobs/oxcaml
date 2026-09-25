(** Independent raw LZ4 blocks, with no dictionary or frame header. *)
val max_block_size : int

type malformed = Vox_lz4_string_decode.malformed =
  | Empty_block
  | Truncated_length
  | Truncated_literals
  | Truncated_offset
  | Zero_offset
  | Offset_beyond_output
  | Invalid_terminal_sequence

type decode_error = Vox_lz4_string_decode.decode_error =
  | Malformed of malformed * int
  | Output_limit
  | Invalid_capacity

(** The successful result is the wire format of the total fast-scan model.
    Sources above [max_block_size] raise [Invalid_argument]. Allocation may
    raise [Out_of_memory]. *)
val compress : (source : string) ->
  {wire : string | Vox_lz4_streaming_codec.compresses source wire}

(** The checked decoder with its erased allocation witness. Its status,
    length, and successful output bytes agree with the total decoder model. *)
val decompress_verified : (wire : string) ->
  (capacity : {n : int | 0 <= n && n <= 4194304}) ->
  {decoded : Vox_lz4_string_decode.decoded |
    Vox_lz4_string_decode.matches_model wire capacity decoded}

(** A string/result adapter over [decompress_verified]. Default capacity is
    [max_block_size]. Invalid capacities return [Invalid_capacity]. Allocation
    may raise [Out_of_memory]. *)
val decompress : ?capacity:int -> string -> (string, decode_error) result

(** Executable composition of [compress] and [decompress_verified], with a
    checked normal-return identity. The total-model round-trip theorem is
    [Vox_lz4_streaming_roundtrip.roundtrip]. *)
val compress_decompress : (source : string) ->
  {output : string | Vox_string_view.contents output ===
    Vox_string_view.contents source}
