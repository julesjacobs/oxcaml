module C = Vox_lz4_streaming_codec
module D = Vox_lz4_string_decode
module V = Vox_string_view

let (roundtrip @ total) = Vox_lz4_decode_bytes_roundtrip.roundtrip

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
