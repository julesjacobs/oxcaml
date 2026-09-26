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
                vox_lz4_spec_parse.ml vox_lz4_spec_decode.ml \
                vox_lz4_spec_decode_bytes.ml vox_lz4_spec_bytes.ml \
                vox_lz4_heap_bytes.ml \
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
                vox_lz4_decode_bytes_proof.ml vox_lz4_decode_bytes_roundtrip.ml \
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
                lz4_fast_reference.ml";
 { bytecode; }
 { native; }
*)

module H = Vox_lz4_fast_hints_reference
module F = Vox_lz4_fast_plan_model
module MS = Vox_lz4_mutable_scan
module P = Vox_lz4_general_plan
module Z = Vox_lz4_general_sized
module S = Vox_lz4_snapshot
module B = Vox_lz4_encode_buffer
module DB = Vox_lz4_buffer
module RT = Vox_lz4_fast_plan_roundtrip

let rec check_decoded source (index : {i : int | 0 <= i}) buffer =
  if index >= DB.used (borrow_ buffer) then DB.release buffer
  else
    let byte, buffer = DB.get buffer index in
    assert (byte = Char.code source.[index]);
    check_decoded source (index + 1) buffer

let check_mutable_roundtrip source =
  let input = Iarray.init (String.length source) (String.get source) in
  if Iarray.length input <= 4194304 then
    match RT.mutable_scan_roundtrip_capacity input (Iarray.length input) with
    | None -> failwith "allocation failed"
    | Some (status, buffer) ->
      assert (status = Vox_lz4_packed.Done);
      assert (DB.used (borrow_ buffer) = String.length source);
      check_decoded source 0 buffer

let reference source =
  let input = Iarray.init (String.length source) (String.get source) in
  if Iarray.length input <= 4194304 then begin
    let hints = H.from_source input in
    if Iarray.length hints = Iarray.length input then begin
      let plan = MS.from_source input in
      assert (plan = P.from_hints input hints);
      assert (F.from_source input = plan);
      match Z.encode input plan with
      | None -> failwith "allocation failed"
      | Some buffer ->
        let snapshot = S.snapshot_prefix buffer in
        let wire = String.init (Iarray.length snapshot.values)
          (Iarray.get snapshot.values) in
        B.release snapshot.buffer;
        wire
    end else failwith "invalid hint length"
  end else failwith "input too large"

let check source =
  let fast = Vox_lz4_baseline.compress source in
  assert (fast = reference source);
  let verified = Vox_lz4_streaming_codec.compress_string source in
  assert (verified = fast);
  assert (Vox_lz4.compress source = fast);
  let capacity = Vox_string_view.length source in
  if capacity <= 4194304 then begin
    let decoded = Vox_lz4_string_decode.decode_string verified capacity in
    ghost_ (Vox_lz4_streaming_roundtrip.roundtrip source verified capacity decoded);
    assert (decoded = Ok source)
  end

let () =
  (match B.create 0 with
   | None -> failwith "allocation failed"
   | Some buffer ->
     let { B.block; permission; used } = buffer in
     let copied = Vox_lz4_string_copy.copy_prefix block used
       (borrow_ permission) in
     B.release { B.block; permission; used };
     assert (copied = ""));
  check "";
  check "#";
  check (String.make 1000 'a');
  let file = open_in_bin __FILE__ in
  let code = really_input_string file (in_channel_length file) in
  close_in file;
  check code;
  check_mutable_roundtrip code;
  assert (Vox_lz4_streaming_roundtrip.compress_decompress code = code);
  let random = Random.State.make [|71; 23; 2026|] in
  for _ = 1 to 1000 do
    let length = Random.State.int random 4097 in
    let period = 1 + Random.State.int random 64 in
    let pattern = String.init period (fun _ ->
      Char.chr (Random.State.int random 256)) in
    check (String.init length (fun i -> pattern.[i mod period]))
  done;
  let large = String.init 65536 (fun _ -> Char.chr (Random.State.int random 256)) in
  let wire = Vox_lz4_streaming_codec.compress_string large in
  assert (wire = Vox_lz4.compress large);
  let decoded = Vox_lz4_string_decode.decode_string wire 65536 in
  Gc.full_major ();
  assert (decoded = Ok large)
