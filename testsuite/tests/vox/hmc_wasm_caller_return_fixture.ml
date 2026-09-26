module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module Codec = Hmc_pointer_frame_codec
module Wire = Hmc_heap_wire
module Words = Hmc_wire_word_sequence
module Q = Wasm_word_sequence
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module L = Wasm_locals
module I = Wasm_instruction
module Header = Hmc_wasm_header_update
module Return = Hmc_wasm_return_result
module Full = Hmc_wasm_return_frame
module Pad = Hmc_wasm_frame_padding
module Restore = Hmc_wasm_frame_restore
module Cells = Hmc_wasm_call_save_memory
module Check = Hmc_wasm_read_check
module T = Wasm_control
module C = Wasm_code
module Caller = Hmc_wasm_caller_return
let rec fuel n = if n = 0 then C.Zero else C.Succ (fuel (n - 1))
let rec fill n tail = if n = 0 then tail else B.Byte (173, fill (n - 1) tail)
type fixture = {memory : B.bytes; expected : B.bytes; code : Wasm_code.t; prefix : I.t list; cursor : B.u32}
let fixture (base : B.u32) padding_length value =
  match Hmc_u32_index.encode 8 padding_length with
  | None -> failwith "return padding"
  | Some padding_count ->
  if base > 7 then failwith "return base" else
  let caller : B.u32 = 512 + base in
  let local_context = D.Binding (D.Forall (D.Z, D.Word64), D.Empty_context) in
  let temporary_context = D.Binding (D.Forall (D.Z, D.Boolean), D.Empty_context) in
  let schema = G.Environment (temporary_context, G.Empty_temporaries) in
  let signature = {G.locals = local_context; temporaries = schema; accumulator = None} in
  let local_value = V.Word (Header.number 11) in
  let temporary_value = V.Boolean true in
  let env = H.Cell (local_value, H.Empty) in
  let temporary_env = H.Cell (temporary_value, H.Empty) in
  let padding = Pad.cells padding_length in
  let rest = H.Cell (local_value, H.Cell (temporary_value, padding)) in
  let current = V.Closure_pointer 1024 in
  let old = V.Word (Header.number 42) in
  let saved = {F.pc = D.S D.Z; env; temporaries = F.Environment (temporary_env, F.Empty); current; accumulator = old} in
  let callee = Cells.cells 9 (V.Closure_pointer 2048) value H.Empty in
  let old_cells = Cells.cells 1 current old rest in
  let new_cells = Cells.cells 1 current value rest in
  let count : Hmc_wasm_relayout.count = 4 + padding_count in
  let width : B.u32 = 16 + 16 * count in
  let top : B.u32 = caller + width in
  let suffix = fill (1024 - caller - width) B.End in
  let memory = fill base (Wire.encode_cells callee (fill (512 - 48) (Wire.encode_cells old_cells suffix))) in
  let expected = fill base (Wire.encode_cells new_cells (fill (512 - width) (Wire.encode_cells new_cells suffix))) in
  let tail_locals = S.Push (S.I32 top, S.Empty) in
  let locals = S.Push (S.I32 base, tail_locals) in
  let state = {X.memory; machine = {E.locals; stack = S.Empty}} in
  if not (Check.word memory base 32 (V.tag value) && Check.word memory base 40 (V.payload value)) then failwith "return source reads" else
  match Hmc_linear_bytes.drop memory caller, Hmc_linear_bytes.drop memory 1024 with
  | Some before, Some _ ->
    (match Q.decode (Words.words old_cells) before with
    | None -> failwith "return caller bytes"
    | Some after ->
      ghost_ (Words.recover old_cells before after ();
        Hmc_linear_bounds.covers_def memory 1024;
        Codec.decode_def signature (D.S D.Z) (H.Cell (current, H.Cell (old, rest)));
        Codec.decode_environment_def local_context rest; Codec.decode_environment_def D.Empty_context (H.Cell (temporary_value, padding));
        Codec.decode_temporaries_def schema (H.Cell (temporary_value, padding));
        Codec.decode_environment_def temporary_context (H.Cell (temporary_value, padding)); Codec.decode_environment_def D.Empty_context padding;
        Codec.decode_temporaries_def G.Empty_temporaries padding;
        Return.tag_offset_def (); Return.payload_offset_def ();
        L.get_def locals 0; L.get_def locals 1; L.get_def tail_locals 0);
      let payload = H.Cell (current, H.Cell (old, rest)) in
      ghost_ (Pad.length padding_length;
        H.length_def payload; H.length_def (H.Cell (old, rest)); H.length_def rest; H.length_def (H.Cell (temporary_value, padding));
        Hmc_u32_index.represents_def (H.length payload) count;
        Hmc_u32_index.represents_def (H.length (H.Cell (old, rest))) (3 + padding_count);
        Hmc_u32_index.represents_def (H.length rest) (2 + padding_count);
        Hmc_u32_index.represents_def (H.length (H.Cell (temporary_value, padding))) (1 + padding_count));
      let plan = Restore.build (H.length payload) count () in
      let result = Full.correct signature (D.S D.Z) 1 saved rest padding value plan count state base caller top 1024 before after 0 1 () in
      if result.Full.state.X.memory <> expected then failwith "return independent caller encoding";
      if X.run (Full.emit plan width 0 1) state <> X.Done result.Full.state then failwith "return execution";
      let root = T.Instruction (I.Plain I.Unreachable, T.Empty) in
      let control = Caller.emit plan count 0 1 caller root T.Empty in
      if T.run (fuel 20000) {T.code = control; labels = T.No_labels; state} <> T.Finished result.Full.state then failwith "caller selection and continuation";
      {memory; expected; code = T.flatten control C.Empty; prefix = [I.I32_const base; I.Local_set 0; I.I32_const top; I.Local_set 1]; cursor = caller})
  | _ -> failwith "return coverage"
let fixtures () =
  let values = [V.Word {Hmc_word64.lo = 7; hi = 37}; V.Boolean false; V.Nil; V.Cons_pointer 2048; V.Closure_pointer 4096] in
  List.concat_map (fun padding -> List.concat_map (fun base -> List.map (fixture base padding) values) [0; 7])
    [D.Z; D.S D.Z; D.S (D.S (D.S D.Z))]
