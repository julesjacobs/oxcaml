(* TEST
 has-z3;
 flags = "-extension refinement_types -principal";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml \
                vox_int_sequence.mli vox_int_sequence.ml \
                vox_iarray.mli vox_iarray.ml \
                vox_string_view.mli vox_string_view.ml \
                borrow_iarray.mli borrow_iarray.ml pref.mli pref.ml \
                ghost_pref.mli ghost_pref.ml raw_memory.mli raw_memory.ml \
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
                lz4_snapshot.ml";
 { bytecode; }
 { native; }
*)

module B = Vox_lz4_encode_buffer
module D = Vox_lz4_buffer
module E = Vox_lz4_packed_encode
module P = Vox_lz4_packed
module S = Vox_lz4_snapshot
module R = Vox_lz4_roundtrip

let rec check_decoded input index buffer =
  if index < 0 || index >= D.used (borrow_ buffer) then D.release buffer
  else
    let byte, buffer = D.get buffer index in
    assert (index < String.length input && byte = Char.code input.[index]);
    check_decoded input (index + 1) buffer

let check bytes =
  let source = Iarray.init (String.length bytes) (String.get bytes) in
  match E.encode source with
  | None -> failwith "encoding failed"
  | Some buffer ->
    let result = S.snapshot_prefix buffer in
    let compressed = String.init (Iarray.length result.values)
                       (Iarray.get result.values) in
    (match Vox_lz4.decompress ~capacity:(String.length bytes) compressed with
     | Ok output -> assert (output = bytes)
     | Error _ -> failwith "encoded block rejected");
    B.release result.buffer;
    (match P.decode result.values (String.length bytes) with
     | None -> failwith "decoder allocation failed"
     | Some (status, decoded) ->
       assert (status = P.Done
               && D.used (borrow_ decoded) = String.length bytes);
       check_decoded bytes 0 decoded);
    (match R.roundtrip source with
     | None -> failwith "verified round trip allocation failed"
     | Some (status, decoded) ->
       assert (status = P.Done
               && D.used (borrow_ decoded) = String.length bytes);
       check_decoded bytes 0 decoded)

let () =
  check "";
  check "a";
  check "a\000b\255c";
  check (String.make 1000 'a');
  check (String.make 400 'a' ^ String.make 300 'b');
  check (String.init 8192 (fun i -> Char.chr (i mod 256)))
