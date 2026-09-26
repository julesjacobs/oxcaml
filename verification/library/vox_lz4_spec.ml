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

type decoded = (string, decode_error) result

let[@def] (matches_model @ total) (wire : string @ immutable)
    (capacity : int) (result : decoded @ immutable) = ghost_ (
  let model = Vox_lz4_spec_decode_bytes.decode_model
    (Vox_string_view.contents wire) 0 (-1)
    (Iarray.length (Vox_string_view.contents wire)) capacity 0 [] in
  0 <= capacity && capacity <= 4194304
  && match result with
     | Error (Malformed _) -> model.kind === Vox_lz4_spec_parse.Malformed
     | Error Output_limit -> model.kind === Vox_lz4_spec_parse.Output_limit
     | Error Invalid_capacity -> false
     | Ok output -> model.kind === Vox_lz4_spec_parse.Done
         && Iarray.length (Vox_string_view.contents output) = model.count
         && Vox_lz4_spec_decode_bytes.matches_bytes
              (Vox_string_view.contents output) model.count model.reversed)

let[@def] (compresses @ total) (source : string @ immutable)
    (wire : string @ immutable) = ghost_ (
  let model = Vox_string_view.contents source in
  if Iarray.length model > 4194304 then false
  else Vox_lz4_spec_wire.wire_matches_plan model (Vox_string_view.contents wire) 0 0 (Vox_lz4_spec_scan.from_source model))
