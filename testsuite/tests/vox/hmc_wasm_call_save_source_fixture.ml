module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module H = Hmc_heap_objects
module W = Hmc_heap_wire
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module L = Wasm_locals
module F = Hmc_wasm_call_save
module R = Hmc_wasm_relayout
module C = Wasm_cross_copy
module I = Wasm_instruction
module Header = Hmc_wasm_header_update
type fixture = {memory : B.bytes; expected : B.bytes; code : Wasm_code.t; prefix : I.t list; cursor : B.u32}
let rec cells = function [] -> H.Empty | value :: rest -> H.Cell (value, cells rest)
let rec fill n = if n = 0 then B.End else B.Byte (173, fill (n - 1))
let rec prefix n tail = if n = 0 then tail else B.Byte (173, prefix (n - 1) tail)
let rec context = function [] -> D.Empty_context | _ :: rest -> D.Binding (D.Forall (D.Z, D.Word64), context rest)
module Pad = Hmc_wasm_frame_padding
module Padded = Hmc_wasm_call_save_padded
let fixture (base : B.u32) padding_length env saved rest =
  match Hmc_u32_index.encode 8 padding_length with
  | None -> failwith "padding count"
  | Some padding_count ->
  if base > 7 then failwith "save base" else
  let temporary = if rest = [] then G.Empty_temporaries else G.Environment (context rest, G.Empty_temporaries) in
  let signature = {G.locals = context env; temporaries = G.Value (context saved, D.Function (D.Word64, D.Word64), temporary); accumulator = Some D.Word64} in
  let capacity = 3 + List.length env + List.length saved + List.length rest in
  if capacity < 3 || capacity > 32 then failwith "save capacity" else (
  if F.build signature (D.S D.Z) (capacity - 1) 10 <> None then failwith "save capacity accepted";
  if F.build signature (D.S D.Z) capacity 0 <> None then failwith "save PC accepted";
  match F.build signature (D.S D.Z) capacity 10 with
  | None -> failwith "save plan rejected"
  | Some fragment ->
    let current = V.Closure_pointer 1024 in
    let accumulator = V.Word {Hmc_word64.lo = 42; hi = 37} in
    let source = cells ([V.Word (Header.number 0); current; accumulator] @ env @ [V.Closure_pointer 2048] @ saved @ rest) in
    let source_size = 16 * (4 + List.length env + List.length saved + List.length rest) in
    let destination : B.u32 = 512 + base in
    let expected_cells = cells ([V.Word (Header.number 1); current; accumulator] @ saved @ rest) in
    let expected_size = 16 * (3 + List.length saved + List.length rest) in
    let memory = prefix base (W.encode_cells source (fill (1024 - base - source_size))) in
    let expected = prefix base (W.encode_cells source (prefix (512 - source_size)
      (W.encode_cells (Hmc_frame_segments.append expected_cells (Pad.cells padding_length)) (fill (1024 - destination - expected_size - 16 * padding_count))))) in
    if expected_size < 48 || expected_size > 256 then failwith "padding live extent" else (
    let padding_position : Hmc_wasm_relayout.count = expected_size / 16 in
    let padding = Pad.build padding_position padding_count padding_length () in
    let locals_tail = S.Push (S.I32 destination, S.Empty) in
    let locals = S.Push (S.I32 base, locals_tail) in
    let state = {X.memory; machine = {E.locals; stack = S.Empty}} in
    let () =
      let remaining = cells (saved @ rest) in
      match signature.G.temporaries with
      | G.Value (saved_context, ty, schema) ->
        (match Hmc_u32_index.encode 32 (Hmc_pointer_frame_codec.locals_size signature.G.locals), Hmc_u32_index.encode 32 (H.length remaining) with
        | Some env_count, Some count ->
          if env_count > 32 || count > 12 then failwith "save size encoding" else (
          if not (Hm_elaboration_check.index_equal (H.length remaining)
            (D.add (Hmc_pointer_frame_codec.locals_size saved_context) (Hmc_pointer_frame_codec.temporaries_size schema))) then failwith "save remaining layout" else
          match Hmc_linear_bytes.drop memory 1024, Hmc_linear_bytes.drop memory base, source with
          | Some _, Some source_bytes, H.Cell (V.Word old_word, source_payload) ->
            if not (Hmc_word64.equal old_word (Header.number 0)) then failwith "source PC" else
            (match Wasm_word_sequence.decode (Hmc_wire_word_sequence.words source) source_bytes,
              Hmc_pointer_frame_codec.decode signature D.Z source_payload with
            | Some source_suffix, Some (activation, H.Empty) ->
              ghost_ (Hmc_linear_bounds.covers_def memory 1024;
                Hmc_wire_word_sequence.recover source source_bytes source_suffix ();
                H.length_def source; Hmc_word64.equal_def old_word (Header.number 0);
                Hmc_pointer_frame_codec.decode_def signature D.Z source_payload;
                Hmc_u32_index.represents_def D.Z 0;
                L.get_def locals 0; L.get_def locals 1; L.get_def locals_tail 0);
              if padding_position <> 3 + count then failwith "padding position" else
              let result = Hmc_wasm_call_save_source.correct signature activation source_payload H.Empty saved_context ty schema (D.S D.Z)
                env_count count 0 fragment capacity padding padding_count padding_length state base destination 1024 source_bytes source_suffix 0 1 () in
              if result.Hmc_wasm_call_save_source.written.Hmc_wasm_frame_pad_finish.memory <> expected then failwith "source saved memory"
            | _ -> failwith "source frame decode")
          | _ -> failwith "source frame bytes")
        | _ -> failwith "save size encoding")
      | _ -> failwith "save operand shape"
    in
    if X.run (Padded.emit fragment padding 0 1) state <> X.Done {X.memory = expected; machine = state.X.machine} then failwith "padded save execution";
    {memory; expected; code = Padded.emit fragment padding 0 1;
      prefix = [I.I32_const base; I.Local_set 0; I.I32_const destination; I.Local_set 1]; cursor = destination}))
let fixtures () =
  let word n = V.Word (Header.number n) in
  List.concat_map (fun padding_length ->
    List.concat_map (fun base -> [fixture base padding_length [] [] []; fixture base padding_length [word 11; word 12] [word 21] [word 31; word 32]]) [0; 7])
    [D.Z; D.S D.Z; D.S (D.S (D.S D.Z))]
