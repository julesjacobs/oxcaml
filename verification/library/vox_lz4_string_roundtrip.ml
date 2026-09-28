module C = Vox_lz4_string_codec
module D = Vox_lz4_string_decode
module V = Vox_string_view

let (roundtrip @ total) :
    (source : string) -> (wire : string) -> (capacity : int) ->
    (decoded : Vox_lz4_spec.decoded) ->
    {u : unit | not (C.compresses source wire
      && Iarray.length (V.contents source) <= capacity && capacity <= 4194304
      && Vox_lz4_spec.matches_model wire capacity decoded)
      || match decoded with
         | Error _ -> false
         | Ok output -> V.contents source === V.contents output} @ ghost =
  fun source wire capacity decoded -> ghost_ (
    C.compresses_def source wire;
    if C.compresses source wire then
      Vox_lz4_decode_bytes_roundtrip.wire_roundtrip source wire
        (Vox_lz4_fast_plan_model.from_source (V.contents source)) capacity decoded;
    ())

let compress_decompress :
    (source : string) ->
    {output : string | V.contents output === V.contents source} =
  fun source ->
    let wire = C.compress_string source in
    let capacity = V.length source in
    if capacity > 4194304 then invalid_arg "Vox_lz4: block too large"
    else
      let decoded = D.decode_string wire capacity in
      ghost_ (roundtrip source wire capacity decoded);
      match decoded with
      | Ok output -> output
      | Error _ -> assert false
