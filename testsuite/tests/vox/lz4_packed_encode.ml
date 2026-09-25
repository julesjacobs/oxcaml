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
                lz4_packed_encode.ml";
 { bytecode; }
 { native; }
*)

module B = Vox_lz4_encode_buffer
module E = Vox_lz4_packed_encode

let () =
  match B.create 5 with
  | None -> ()
  | Some buffer ->
    let buffer = E.emit_extensions 510 buffer in
    assert (B.used (borrow_ buffer) = 3);
    if B.used (borrow_ buffer) >= 3 then begin
      let a, buffer = B.get buffer 0 in
      let b, buffer = B.get buffer 1 in
      let c, buffer = B.get buffer 2 in
      assert (a = 255 && b = 255 && c = 0);
      B.release buffer
    end else B.release buffer

let to_iarray source = Iarray.init (String.length source) (String.get source)

let to_string buffer =
  let output = Bytes.create (B.used (borrow_ buffer)) in
  let rec copy index buffer =
    if index < 0 || index >= B.used (borrow_ buffer)
       || index >= Bytes.length output then B.release buffer
    else
      let byte, buffer = B.get buffer index in
      Bytes.set output index (Char.chr byte);
      copy (index + 1) buffer
  in
  copy 0 buffer;
  Bytes.to_string output

let check_roundtrip source =
  match E.encode (to_iarray source) with
  | None -> failwith "encoding failed"
  | Some buffer ->
    let encoded = to_string buffer in
    match Vox_lz4.decompress ~capacity:(String.length source) encoded with
    | Ok decoded -> assert (decoded = source)
    | Error _ -> failwith "encoded block rejected"

let () =
  let random = Random.State.make [| 91; 17; 42 |] in
  check_roundtrip (String.make 12 'x');
  check_roundtrip (String.make 5 'x' ^ String.make 8 'z');
  for length = 0 to 1024 do
    check_roundtrip (String.init length (fun _ ->
      Char.chr (Random.State.int random 256)));
    let prefix = String.make (max 0 (length - 5)) 'x' in
    let suffix = String.init (min 5 length) (fun i -> Char.chr (65 + i)) in
    check_roundtrip (prefix ^ suffix)
  done;
  match E.encode (to_iarray (String.make 1000 'a')) with
  | None -> failwith "encoding failed"
  | Some buffer ->
    assert (B.used (borrow_ buffer) < 32);
    B.release buffer
