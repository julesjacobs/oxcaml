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
                lz4_packed.ml";
 { bytecode; }
 { native; }
*)

module B = Vox_lz4_buffer
module P = Vox_lz4_packed

let bytes values = Iarray.map Char.chr values

let run () =
  match B.create 20 with
  | None -> ()
  | Some buffer ->
    let buffer = B.append buffer 97 in
    let buffer = P.copy_match 1 14 buffer in
    let buffer = P.copy_match 1 5 buffer in
    let last, buffer = B.get buffer 19 in
    assert (last = 97);
    B.release buffer

let () = run ()

let () =
  match P.decode (bytes [: 0 :]) 0 with
  | None -> failwith "allocation failed"
  | Some (status, buffer) ->
    assert (status = P.Done);
    assert (B.used (borrow_ buffer) = 0);
    B.release buffer

let () =
  match P.decode (bytes [: 0x1a; 97; 1; 0; 0x50; 97; 97; 97; 97; 97 :]) 20 with
  | None -> failwith "allocation failed"
  | Some (status, buffer) ->
    assert (status = P.Done);
    assert (B.used (borrow_ buffer) = 20);
    if B.used (borrow_ buffer) > 19 then begin
      let first, buffer = B.get buffer 0 in
      let last, buffer = B.get buffer 19 in
      assert (first = 97 && last = 97);
      B.release buffer
    end else B.release buffer

let () =
  match P.decode (bytes [: 0x30; 97; 98; 99 :]) 2 with
  | None -> failwith "allocation failed"
  | Some (status, buffer) ->
    assert (status = P.Output_limit);
    assert (B.used (borrow_ buffer) = 0);
    B.release buffer

let () =
  match P.decode (bytes [: 0x10; 97; 0; 0; 0x50; 97; 97; 97; 97; 97 :]) 20 with
  | None -> failwith "allocation failed"
  | Some (status, buffer) ->
    assert (status = P.Malformed);
    B.release buffer

let expect case expected size source capacity =
  match P.decode (bytes source) capacity with
  | None -> failwith "allocation failed"
  | Some (status, buffer) ->
    if status <> expected then failwith ("status " ^ string_of_int case);
    if B.used (borrow_ buffer) <> size then
      failwith ("size " ^ string_of_int case);
    B.release buffer

let () =
  expect 1 P.Done 5 [: 0x50; 72; 101; 108; 108; 111 :] 5;
  expect 2 P.Malformed 0 [: :] 20;
  expect 3 P.Malformed 0 [: 0xf0 :] 20;
  expect 4 P.Malformed 0 [: 0x20; 97 :] 20;
  expect 5 P.Malformed 0 [: 0x10; 97; 0 :] 20;
  expect 6 P.Malformed 0 [: 0x00; 1; 0; 0x50; 97; 97; 97; 97; 97 :] 20;
  expect 7 P.Malformed 5 [: 0x10; 97; 1; 0; 0 :] 20;
  expect 8 P.Output_limit 15
    [: 0x1a; 97; 1; 0; 0x50; 97; 97; 97; 97; 97 :] 19

let to_iarray source =
  Iarray.init (String.length source) (String.get source)

let rec check_bytes source index buffer =
  if index < 0 || index >= B.used (borrow_ buffer) then B.release buffer
  else
    let value, buffer = B.get buffer index in
    assert (index < String.length source && value = Char.code source.[index]);
    check_bytes source (index + 1) buffer

let check_roundtrip input =
  let encoded = Vox_lz4.compress input in
  match P.decode (to_iarray encoded) (String.length input) with
  | None -> failwith "allocation failed"
  | Some (status, buffer) ->
    assert (status = P.Done &&
            B.used (borrow_ buffer) = String.length input);
    check_bytes input 0 buffer

let () =
  let random = Random.State.make [| 17; 42; 2026 |] in
  for length = 0 to 512 do
    check_roundtrip (String.init length (fun _ ->
      Char.chr (Random.State.int random 256)));
    let period = 1 + Random.State.int random 32 in
    let pattern = String.init period (fun _ ->
      Char.chr (Random.State.int random 256)) in
    check_roundtrip (String.init length (fun i -> pattern.[i mod period]))
  done;
  let longest_offset =
    "\xf3" ^ String.make 256 '\xff' ^ "\xf0"
    ^ String.make 65535 'x' ^ "\xff\xff\x50xxxxx" in
  match P.decode (to_iarray longest_offset) 65547 with
  | None -> failwith "allocation failed"
  | Some (status, buffer) ->
    assert (status = P.Done && B.used (borrow_ buffer) = 65547);
    check_bytes (String.make 65547 'x') 0 buffer
