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
                lz4_codec.ml";
 { bytecode; }
 { native; }
*)

open Vox_lz4

let decode block =
  match decompress block with
  | Ok bytes -> bytes
  | Error _ -> failwith "valid LZ4 block rejected"

let () =
  assert (compress "" = "\x00");
  assert (decode "\x00" = "");
  assert (decode "\x50Hello" = "Hello");
  assert (decode "\x1aa\x01\x00\x50aaaaa" = String.make 20 'a');
  let longest_offset =
    "\xf3" ^ String.make 256 '\xff' ^ "\xf0"
    ^ String.make 65535 'x' ^ "\xff\xff\x50xxxxx" in
  assert (decode longest_offset = String.make 65547 'x');
  assert (decode (compress (String.make 20 'a')) = String.make 20 'a');
  assert (String.length (compress (String.make 1000 'a')) < 32)

let () =
  let bad block expected =
    match decompress block with
    | Error (Malformed (reason, _)) -> assert (reason = expected)
    | _ -> failwith "malformed block accepted or misclassified"
  in
  bad "" Empty_block;
  bad "\xf0" Truncated_length;
  bad "\x20a" Truncated_literals;
  bad "\x10a\x00" Truncated_offset;
  bad "\x10a\x00\x00\x50aaaaa" Zero_offset;
  bad "\x00\x01\x00\x50aaaaa" Offset_beyond_output;
  bad "\x10a\x01\x00\x00" Invalid_terminal_sequence;
  assert (decompress ~capacity:2 "\x30abc" = Error Output_limit);
  assert (decompress ~capacity:19 "\x1aa\x01\x00\x50aaaaa" =
          Error Output_limit);
  assert (decompress ~capacity:(-1) "\x00" = Error Invalid_capacity);
  assert (decompress ~capacity:(max_block_size + 1) "\x00" =
          Error Invalid_capacity)

let () =
  let state = Random.State.make [| 17; 42; 2026 |] in
  for length = 0 to 4096 do
    let input = String.init length (fun _ ->
      Char.chr (Random.State.int state 256)) in
    assert (decode (compress input) = input)
  done;
  for length = 12 to 4096 do
    let period = 1 + Random.State.int state 32 in
    let pattern = String.init period (fun _ ->
      Char.chr (Random.State.int state 256)) in
    let input = String.init length (fun i -> pattern.[i mod period]) in
    assert (decode (compress input) = input)
  done;
  for length = 0 to 128 do
    for _ = 1 to 100 do
      let block = String.init length (fun _ ->
        Char.chr (Random.State.int state 256)) in
      let capacity = Random.State.int state 256 in
      ignore (decompress ~capacity block)
    done
  done;
  let dense_matches = String.init 4_000_005 (fun i ->
    let k = i mod 8 in
    if k < 4 then "ABCD".[k]
    else Char.chr (((i / 8) lsr ((k - 4) * 8)) land 255)) in
  assert (decompress (compress dense_matches) = Ok dense_matches);
  let maximum = String.init max_block_size (fun _ ->
    Char.chr (Random.State.int state 256)) in
  assert (decompress (compress maximum) = Ok maximum)
