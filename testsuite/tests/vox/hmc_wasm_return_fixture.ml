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
module Cells = Hmc_wasm_call_save_memory
module Check = Hmc_wasm_read_check
let rec fill n tail = if n = 0 then tail else B.Byte (173, fill (n - 1) tail)
type fixture = {memory : B.bytes; expected : B.bytes; code : Wasm_code.t; prefix : I.t list; cursor : B.u32}
let fixture (base : B.u32) value =
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
  let rest = H.Cell (local_value, temporary_env) in
  let current = V.Closure_pointer 1024 in
  let old = V.Word (Header.number 42) in
  let saved = {F.pc = D.S D.Z; env; temporaries = F.Environment (temporary_env, F.Empty); current; accumulator = old} in
  let callee = Cells.cells 9 (V.Closure_pointer 2048) value H.Empty in
  let old_cells = Cells.cells 1 current old rest in
  let new_cells = Cells.cells 1 current value rest in
  let suffix = fill (1024 - caller - 80) B.End in
  let memory = fill base (Wire.encode_cells callee (fill (512 - 48) (Wire.encode_cells old_cells suffix))) in
  let expected = fill base (Wire.encode_cells callee (fill (512 - 48) (Wire.encode_cells new_cells suffix))) in
  let tail_locals = S.Push (S.I32 caller, S.Empty) in
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
        Codec.decode_environment_def local_context rest; Codec.decode_environment_def D.Empty_context temporary_env;
        Codec.decode_temporaries_def schema temporary_env;
        Codec.decode_environment_def temporary_context temporary_env; Codec.decode_environment_def D.Empty_context H.Empty;
        Codec.decode_temporaries_def G.Empty_temporaries H.Empty;
        Return.tag_offset_def (); Return.payload_offset_def ();
        L.get_def locals 0; L.get_def locals 1; L.get_def tail_locals 0);
      let result = Return.correct signature (D.S D.Z) 1 saved rest value state base caller 1024 before after 0 1 () in
      if result.Return.memory <> expected then failwith "return independent caller encoding";
      if X.run (Return.emit 0 1) state <> X.Done {X.memory = expected; machine = state.X.machine} then failwith "return execution";
      {memory; expected; code = Return.emit 0 1; prefix = [I.I32_const base; I.Local_set 0; I.I32_const caller; I.Local_set 1]; cursor = caller})
  | _ -> failwith "return coverage"
let fixtures () =
  let values = [V.Word {Hmc_word64.lo = 7; hi = 37}; V.Boolean false; V.Nil; V.Cons_pointer 2048; V.Closure_pointer 4096] in
  List.concat_map (fun base -> List.map (fixture base) values) [0; 7]
