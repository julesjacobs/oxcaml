module V = Vox_string_view
module F = Vox_lz4_forward_model
module E = Vox_lz4_streaming
module C = Vox_lz4_general_cost
module Bridge = Vox_lz4_general_bridge
module W = Vox_lz4_general_wire
module Copy = Vox_lz4_string_copy
module B = Vox_lz4_encode_buffer
module M = Raw_memory
module G = Ghost_pref
module D = Vox_lz4_packed
module R = Vox_lz4_roundtrip

include Vox_lz4_spec

let compress :
    (model : {m : char iarray | Iarray.length m <= 4194304}) @ ghost ->
    (source : {s : string | V.contents s === model}) ->
    {wire : string |
      Vox_lz4_spec_wire.wire_matches_plan model (V.contents wire) 0 0 (Vox_lz4_spec_scan.from_source model)} =
  fun model source ->
    let plan = ghost_ (Vox_lz4_spec_scan.from_source model) in
    match E.encode model source with
    | None -> raise Out_of_memory
    | Some buffer ->
      let { B.block; permission; used } = buffer in
      let wire : {s : string | Iarray.length (V.contents s) = used
          && Vox_lz4_spec_bytes.prefix_matches (V.contents s)
               (G.own permission) block used} =
        try Copy.copy_prefix block used (borrow_ permission)
        with exn ->
          B.release { B.block; permission; used };
          raise exn
      in
      ghost_ (
        C.encoded_size_capacity model 0 plan;
        C.encode_model_size model 0 plan block 0 (M.footprint block);
        Bridge.model_wire model (V.contents wire) 0 plan block 0
          (M.footprint block));
      B.release { B.block; permission; used };
      wire

let compress_string : (source : string) ->
    {wire : string | Vox_lz4_spec.compresses source wire} =
  fun source ->
    if V.length source > 4194304 then
      invalid_arg "Vox_lz4.compress: block too large"
    else
      let model = ghost_ (V.contents source) in
      let wire = compress model source in
      ghost_ (Vox_lz4_spec.compresses_def source wire);
      wire

let (compressed_decodes @ total) :
    (source : string) -> (wire : string) ->
    (capacity : int) -> (block : M.t) ->
    {u : unit | not (Vox_lz4_spec.compresses source wire
      && Iarray.length (V.contents source) <= capacity && capacity <= 4194304)
      || let refine_ result =
           Vox_lz4_spec_decode.decode_model (V.contents wire) 0 (-1)
             (Iarray.length (V.contents wire)) capacity block 0
             (M.footprint block) in
         result.D.kind === D.Done
         && result.D.count = Iarray.length (V.contents source)
         && R.output_matches result.D.state block
              (V.contents source) result.D.count} @ ghost =
  fun source wire capacity block -> ghost_ (
    Vox_lz4_spec.compresses_def source wire;
    if Vox_lz4_spec.compresses source wire
       && Iarray.length (V.contents source) <= capacity
       && capacity <= 4194304 then begin
      let model = V.contents source in
      let plan = Vox_lz4_spec_scan.from_source model in
      W.decode_wire_matches_source model (V.contents wire) plan capacity block
    end;
    ())
