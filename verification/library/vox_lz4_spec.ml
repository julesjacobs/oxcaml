type malformed =
  | Empty_block
  | Truncated_length
  | Truncated_literals
  | Truncated_offset
  | Zero_offset
  | Offset_beyond_output
  | Invalid_terminal_sequence

type decode_error =
  | Malformed of malformed * int
  | Output_limit
  | Invalid_capacity

type decoded = {
  output : string option;
  status : Vox_lz4_spec_decode.status;
  error : decode_error option;
  block : Raw_memory.t @@ ghost;
}

let[@def] (matches_model @ total) (wire : string @ immutable)
    (capacity : int) (result : decoded @ immutable) = ghost_ (
  let model = Vox_lz4_spec_decode.decode_model (Vox_string_view.contents wire) 0 (-1)
      (Iarray.length (Vox_string_view.contents wire)) capacity result.block 0
      (Raw_memory.footprint result.block) in
  Raw_memory.length result.block = capacity
  && result.status === model.Vox_lz4_spec_decode.kind
  && (match result.error with None -> result.status === Vox_lz4_spec_decode.Done
      | Some _ -> not (result.status === Vox_lz4_spec_decode.Done))
  && match result.output with
     | None -> not (result.status === Vox_lz4_spec_decode.Done)
     | Some output -> result.status === Vox_lz4_spec_decode.Done
         && Iarray.length (Vox_string_view.contents output) = model.Vox_lz4_spec_decode.count
         && Vox_lz4_spec_bytes.prefix_matches (Vox_string_view.contents output) model.Vox_lz4_spec_decode.state
              result.block model.Vox_lz4_spec_decode.count)

let[@def] (compresses @ total) (source : string @ immutable)
    (wire : string @ immutable) = ghost_ (
  let model = Vox_string_view.contents source in
  if Iarray.length model > 4194304 then false
  else Vox_lz4_spec_wire.wire_matches_plan model (Vox_string_view.contents wire) 0 0 (Vox_lz4_spec_scan.from_source model))
