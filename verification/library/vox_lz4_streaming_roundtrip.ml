module C = Vox_lz4_streaming_codec
module D = Vox_lz4_string_decode
module V = Vox_string_view
module I = Vox_iarray
module R = Vox_lz4_roundtrip
module S = Vox_lz4_snapshot
module E = Vox_lz4_packed_encode
module P = Ghost_pref
module M = Raw_memory

let (output_equal @ total) :
    (heap : P.heap) -> (block : M.t) ->
    (source : char iarray) -> (output : char iarray) ->
    {u : unit | not (Iarray.length source = Iarray.length output
      && R.output_matches heap block source (Iarray.length source)
      && S.prefix_matches output heap block (Iarray.length source))
      || source === output} @ ghost =
  fun heap block source output -> ghost_ (
    if Iarray.length source = Iarray.length output
       && R.output_matches heap block source (Iarray.length source)
       && S.prefix_matches output heap block (Iarray.length source) then
      I.extensional source output (fun index ->
        if 0 <= index && index < Iarray.length source then begin
          Vox_lz4_general_match.output_matches_get heap block source (Iarray.length source) index;
          S.prefix_matches_get output heap block (Iarray.length source) index;
          E.source_at_def source index;
          I.at_get source index
        end else begin
          I.at_outside source index;
          I.at_outside output index
        end);
    ())

let (roundtrip @ total) :
    (source : string) -> (wire : string) -> (capacity : int) ->
    (decoded : D.decoded) ->
    {u : unit | not (C.compresses source wire
      && Iarray.length (V.contents source) <= capacity && capacity <= 4194304
      && D.matches_model wire capacity decoded)
      || match decoded.D.output with
         | None -> false
         | Some output -> V.contents source === V.contents output} @ ghost =
  fun source wire capacity decoded -> ghost_ (
    if C.compresses source wire
       && Iarray.length (V.contents source) <= capacity && capacity <= 4194304
       && D.matches_model wire capacity decoded then begin
      C.compressed_decodes source wire capacity decoded.D.block;
      D.matches_model_def wire capacity decoded;
      match decoded.D.output with
      | None -> ()
      | Some output ->
        let model = Vox_lz4_packed.decode_model (V.contents wire) 0 (-1)
            (Iarray.length (V.contents wire)) capacity decoded.D.block 0
            (M.footprint decoded.D.block) in
        output_equal model.Vox_lz4_packed.state decoded.D.block
          (V.contents source) (V.contents output)
    end;
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
      match decoded.D.output with
      | Some output -> output
      | None -> assert false
