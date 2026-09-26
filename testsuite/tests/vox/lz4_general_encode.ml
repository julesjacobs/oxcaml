(* TEST
 has-z3;
 flags = "-extension refinement_types -principal";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml \
                vox_int_sequence.mli vox_int_sequence.ml \
                vox_iarray.mli vox_iarray.ml \
                borrow_iarray.mli borrow_iarray.ml pref.mli pref.ml \
                ghost_pref.mli ghost_pref.ml raw_memory.mli raw_memory.ml vox_lz4_spec_storage.ml \
                vox_string_view.mli vox_string_view.ml \
                vox_lz4_spec_parse.ml vox_lz4_spec_decode.ml \
                vox_lz4_spec_decode_bytes.ml vox_lz4_spec_bytes.ml \
                vox_lz4_heap_bytes.ml \
                vox_lz4_spec_match.ml vox_lz4_spec_plan.ml \
                vox_lz4_spec_token.ml vox_lz4_spec_wire.ml \
                vox_lz4_spec_hashes.ml vox_lz4_spec_scan.ml \
                vox_lz4_spec.ml \
                vox_lz4_buffer.ml vox_lz4_packed.ml \
                vox_lz4_encode_buffer.ml vox_lz4_packed_encode.ml \
                vox_lz4_snapshot.ml vox_lz4_roundtrip.ml \
                vox_lz4_general_match.ml vox_lz4_general_plan.ml \
                vox_lz4_general_encode.ml lz4_general_encode.ml";
 { native; }
*)

module M = Vox_lz4_general_match
module P = Vox_lz4_general_plan
module E = Vox_lz4_general_encode
module EB = Vox_lz4_encode_buffer
module S = Vox_lz4_snapshot
module D = Vox_lz4_packed
module DB = Vox_lz4_buffer

let rec check_decoded source index buffer =
  if index < 0 || index >= DB.used (borrow_ buffer) then DB.release buffer
  else
    let byte, buffer = DB.get buffer index in
    assert (index < String.length source
            && byte = Char.code source.[index]);
    check_decoded source (index + 1) buffer

let check source should_compress =
  let input = Iarray.init (String.length source) (String.get source) in
  let table = Array.make 65536 (-1) in
  let hints = Iarray.init (Iarray.length input) (fun position ->
    if position < 0 || position > Iarray.length input - 4 then -1
    else
      let hash = Vox_lz4_spec_match.hash4 input position in
      let previous = table.(hash) in
      table.(hash) <- position;
      previous) in
  if Iarray.length input <= 4194304
     && Iarray.length hints = Iarray.length input then begin
    let plan = P.from_hints input hints in
    match E.encode input plan with
    | None -> failwith "general encoding failed"
    | Some buffer ->
      let snapshot = S.snapshot_prefix buffer in
      let wire = snapshot.values in
      if should_compress then assert (Iarray.length wire < String.length source);
      EB.release snapshot.buffer;
      (match D.decode wire (String.length source) with
       | None -> failwith "general decoding allocation failed"
       | Some (status, decoded) ->
         assert (status = D.Done
                 && DB.used (borrow_ decoded) = String.length source);
         check_decoded source 0 decoded)
  end

let () =
  check "" false;
  check "a" false;
  check "abcabcabcXYZ" false;
  check (String.make 1000 'a') true;
  let code = String.concat "\n"
    (List.init 64 (fun i ->
       Printf.sprintf "let f%d x = x + %d" (i mod 8) (i mod 8))) in
  check code true;
  let random = Random.State.make [| 71; 23 |] in
  check (String.init 2048 (fun _ ->
    Char.chr (Random.State.int random 256))) false
