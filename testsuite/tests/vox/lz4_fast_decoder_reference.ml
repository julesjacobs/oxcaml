(* TEST
 has-z3;
 flags = "-extension refinement_types -principal";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml \
                vox_int_sequence.mli vox_int_sequence.ml \
                vox_iarray.mli vox_iarray.ml \
                vox_string_view.mli vox_string_view.ml \
                borrow_iarray.mli borrow_iarray.ml pref.mli pref.ml \
                ghost_pref.mli ghost_pref.ml raw_memory.mli raw_memory.ml vox_lz4_spec_storage.ml \
                vox_lz4_spec_decode.ml vox_lz4_spec_bytes.ml \
                vox_lz4_spec_match.ml vox_lz4_spec_plan.ml \
                vox_lz4_spec_token.ml vox_lz4_spec_wire.ml \
                vox_lz4_spec_hashes.ml vox_lz4_spec_scan.ml \
                vox_lz4_spec.ml \
                vox_lz4_buffer.ml vox_lz4_packed.ml \
                vox_lz4_encode_buffer.ml vox_lz4_packed_encode.ml \
                vox_lz4_snapshot.ml \
                vox_lz4_string_copy.mli vox_lz4_string_copy.ml \
                vox_lz4_roundtrip.ml \
                vox_lz4_general_match.ml vox_lz4_string_match.ml \
                vox_lz4_general_plan.ml \
                vox_lz4_general_encode.ml vox_lz4_general_wire.ml \
                vox_lz4_general_cost.ml vox_lz4_general_bridge.ml \
                vox_lz4_general_sized.ml vox_lz4_string_encode.ml \
                vox_lz4_general_roundtrip.ml \
                vox_lz4_fast_plan_model.ml vox_lz4_mutable_scan.ml \
                vox_lz4_string_scan.ml vox_lz4_string_decode.ml \
                vox_lz4_string_codec.ml vox_lz4_string_roundtrip.ml \
                vox_lz4_forward_model.ml vox_lz4_streaming.ml \
                vox_lz4_streaming_codec.ml vox_lz4_streaming_roundtrip.ml \
                vox_lz4_fast_plan_roundtrip.ml \
                vox_lz4_fast_hints_reference.ml \
                vox_lz4_checked_api.ml vox_lz4.mli vox_lz4.ml \
                vox_lz4_baseline.ml \
                lz4_fast_decoder_reference.ml";
 { bytecode; }
 { native; }
*)

module D = Vox_lz4_packed
module B = Vox_lz4_buffer

let rec bytes_of_buffer output (index : {i : int | 0 <= i}) buffer =
  if index >= B.used (borrow_ buffer) then begin
    B.release buffer;
    Bytes.unsafe_to_string output
  end else begin
    let byte, buffer = B.get buffer index in
    Bytes.set output index (Char.chr byte);
    bytes_of_buffer output (index + 1) buffer
  end

let checked block capacity =
  let input = Iarray.init (String.length block) (String.get block) in
  match D.decode input capacity with
  | None -> failwith "allocation failed"
  | Some (status, buffer) ->
    let output = Bytes.create (B.used (borrow_ buffer)) in
    status, bytes_of_buffer output 0 buffer

module SD = Vox_lz4_string_decode

let baseline_error = function
  | SD.Invalid_capacity -> Vox_lz4_baseline.Invalid_capacity
  | SD.Output_limit -> Vox_lz4_baseline.Output_limit
  | SD.Malformed (reason, position) ->
    let reason = match reason with
      | SD.Empty_block -> Vox_lz4_baseline.Empty_block
      | SD.Truncated_length -> Vox_lz4_baseline.Truncated_length
      | SD.Truncated_literals -> Vox_lz4_baseline.Truncated_literals
      | SD.Truncated_offset -> Vox_lz4_baseline.Truncated_offset
      | SD.Zero_offset -> Vox_lz4_baseline.Zero_offset
      | SD.Offset_beyond_output -> Vox_lz4_baseline.Offset_beyond_output
      | SD.Invalid_terminal_sequence -> Vox_lz4_baseline.Invalid_terminal_sequence in
    Vox_lz4_baseline.Malformed (reason, position)

let check block capacity =
  let reference, expected = checked block capacity in
  let fast = Vox_lz4_baseline.decompress ~capacity block in
  let public = Vox_lz4.decompress ~capacity block in
  if 0 <= capacity && capacity <= 4194304 then begin
    let decoded = SD.decode_string block capacity in
    assert (decoded.SD.status = reference);
    match decoded.SD.output, decoded.SD.error with
    | Some output, None ->
      assert (reference = D.Done && output = expected);
      assert (fast = Ok output);
      assert (public = Ok output)
    | None, Some error ->
      assert (reference <> D.Done);
      assert (fast = Error (baseline_error error));
      assert (public = Error error)
    | _ -> assert false
  end;
  match fast with
  | Ok got ->
    assert (reference = D.Done);
    assert (got = expected)
  | Error _ -> assert (reference <> D.Done)

let () =
  let random = Random.State.make [|71; 23; 2026|] in
  for _ = 1 to 5000 do
    let length = Random.State.int random 65 in
    let block = String.init length (fun _ ->
      Char.chr (Random.State.int random 256)) in
    check block (Random.State.int random 129)
  done;
  for _ = 1 to 1000 do
    let length = Random.State.int random 1025 in
    let period = 1 + Random.State.int random 40 in
    let pattern = String.init period (fun _ ->
      Char.chr (Random.State.int random 256)) in
    let source = String.init length (fun i -> pattern.[i mod period]) in
    check (Vox_lz4_baseline.compress source) length
  done
