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
                vox_lz4_spec_decode.ml vox_lz4_spec_bytes.ml \
                vox_lz4_spec_match.ml vox_lz4_spec_plan.ml \
                vox_lz4_spec_token.ml vox_lz4_spec_wire.ml \
                vox_lz4_spec_hashes.ml vox_lz4_spec_scan.ml \
                vox_lz4_spec.ml \
                vox_lz4_buffer.ml vox_lz4_packed.ml \
                vox_lz4_encode_buffer.ml vox_lz4_packed_encode.ml \
                vox_lz4_snapshot.ml vox_lz4_roundtrip.ml \
                vox_lz4_general_match.ml vox_lz4_general_plan.ml \
                vox_lz4_general_encode.ml vox_lz4_general_wire.ml \
                vox_lz4_general_cost.ml vox_lz4_general_bridge.ml \
                vox_lz4_general_sized.ml vox_lz4_general_roundtrip.ml \
                lz4_general_roundtrip.ml";
 { bytecode; }
 { native; }
*)

module M = Vox_lz4_general_match
module T = Vox_lz4_general_roundtrip
module D = Vox_lz4_packed
module B = Vox_lz4_buffer

let rec check_decoded source (index : {i : int | 0 <= i}) buffer =
  if index >= B.used (borrow_ buffer) then B.release buffer
  else
    let byte, buffer = B.get buffer index in
    assert (byte = Char.code source.[index]);
    check_decoded source (index + 1) buffer

let check source =
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
     && Iarray.length hints = Iarray.length input then
    match T.roundtrip_hints input hints with
    | None -> failwith "allocation failed"
    | Some (status, decoded) ->
      assert (status = D.Done);
      assert (B.used (borrow_ decoded) = String.length source);
      check_decoded source 0 decoded
  else failwith "input too large"

let () =
  check "";
  check "a";
  check "abcabcabcXYZ";
  check (String.make 1000 'a');
  let code = String.concat "\n"
    (List.init 64 (fun i ->
       Printf.sprintf "let f%d x = x + %d" (i mod 8) (i mod 8))) in
  check code;
  let random = Random.State.make [| 71; 23 |] in
  check (String.init 2048 (fun _ ->
    Char.chr (Random.State.int random 256)));
  check (String.init 800005 (fun i ->
    let k = i mod 8 in
    if k < 4 then "ABCD".[k]
    else Char.chr (((i / 8) lsr ((k - 4) * 8)) land 255)))
