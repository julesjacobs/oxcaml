module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module H = Hmc_heap_objects
module V = Hmc_tagged_cell
module Codec = Hmc_pointer_frame_codec
module W = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module L = Wasm_locals
module T = Wasm_control
module I = Wasm_instruction
module C = Wasm_code
module F = Hmc_wasm_call_save_source_fixture
module Save = Hmc_wasm_call_save
module Pad = Hmc_wasm_frame_padding
module Guard = Hmc_wasm_call_save_guard
module Index = Hmc_u32_index
module Stack = Hmc_memory_stack
module Capacity = Hmc_memory_stack_capacity
module Runtime = Hmc_runtime_closures
module Table = Hmc_runtime_descriptor_table
type fixture = {memory : B.bytes; expected : B.bytes; code : C.t; prefix : I.t list; cursor : B.u32}
let rec fuel n = if n = 0 then C.Zero else C.Succ (fuel (n - 1))
let fixture (base : B.u32) available =
  if base > 7 then failwith "guard source base" else
  let top : B.u32 = 512 + base in
  let stack_limit : B.u32 = if available then top + 48 else top in
  let stack_capacity = if available then D.S D.Z else D.Z in
  let target_signature = Hmc_frame_call_save.signature D.Empty_context G.Empty_temporaries in
  let block = {G.signature = target_signature; instruction = G.Jump (D.S D.Z)} in
  let blocks = G.Add (block, G.Add (block, G.Empty)) in
  let ty = D.Function (D.Word64, D.Word64) in
  let signature = {G.locals = D.Empty_context; temporaries = G.Value (D.Empty_context, ty, G.Empty_temporaries); accumulator = Some D.Word64} in
  let current = V.Closure_pointer 1024 in
  let value = V.Word {Hmc_word64.lo = 42; hi = 37} in
  let cells = H.Cell (current, H.Cell (value, H.Cell (V.Closure_pointer 2048, H.Empty))) in
  let full = H.Cell (V.Word (Header.number 0), cells) in
  let memory = F.prefix base (W.encode_cells full (F.fill (1024 - base - 64))) in
  let expected = if not available then memory else
    F.prefix base (W.encode_cells full (F.prefix 448
      (W.encode_cells (H.Cell (V.Word (Header.number 1), H.Cell (current, H.Cell (value, H.Empty)))) (F.fill (1024 - top - 48))))) in
  match Save.build signature (D.S D.Z) 3 10, G.lookup blocks (D.S D.Z),
    Index.encode 10 (Hmc_frame_capacity.capacity blocks), Codec.decode signature D.Z cells,
    Hmc_linear_bytes.drop memory base, Hmc_linear_bytes.drop memory 1024 with
  | Some fragment, Some target, Some capacity, Some (activation, H.Empty), Some bytes, Some _ ->
    if capacity <> 2 then failwith "guard frame capacity" else
    if not (Hm_elaboration_check.index_equal (Codec.locals_size signature.G.locals) D.Z
      && Hm_elaboration_check.index_equal (D.add (Codec.locals_size D.Empty_context) (Codec.temporaries_size G.Empty_temporaries)) D.Z)
    then failwith "guard source counts" else
    (match Wasm_word_sequence.decode (Hmc_wire_word_sequence.words full) bytes with
    | None -> failwith "guard source wire"
    | Some suffix ->
      let heap = H.Empty_heap 0 in
      let locals_tail = S.Push (S.I32 top, S.Push (S.I32 stack_limit, S.Empty)) in
      let locals = S.Push (S.I32 base, locals_tail) in
      let state = {X.memory; machine = {E.locals; stack = S.Empty}} in
      ghost_ (Index.represents_def D.Z 0; Hmc_frame_call_save.signature_def D.Empty_context G.Empty_temporaries);
      let padding = Pad.build 3 0 D.Z () in
      let outer = T.Label ({T.restart = None; continuation = T.Empty; saved = S.Empty}, T.No_labels) in
      ghost_ (G.lookup_def blocks (D.S D.Z); G.size_def (G.Add (block, G.Empty)); G.size_def G.Empty;
        let _ = Hm_elaboration_check.index_equal (D.S D.Z) (G.size (G.Add (block, G.Empty))) in
        Hmc_wire_word_sequence.recover full bytes suffix (); H.length_def full;
        H.length_def cells; H.length_def (H.Cell (value, H.Cell (V.Closure_pointer 2048, H.Empty)));
        H.length_def (H.Cell (V.Closure_pointer 2048, H.Empty)); H.length_def H.Empty;
        Index.represents_def (D.S (D.S (D.S D.Z))) 3; Index.represents_def (D.S (D.S D.Z)) 2;
        Index.represents_def (D.S D.Z) 1; Index.represents_def D.Z 0;
        Codec.decode_def signature D.Z cells;
        H.valid_def Hmc_closure_ir.Empty heap; H.used_def heap; Hmc_heap_image.related_def memory heap;
        Table.related_def Runtime.Empty memory 0 0;
        Hmc_linear_bounds.covers_def memory 1024;
        L.get_def locals 0; L.get_def locals 1; L.get_def locals 2; L.get_def locals_tail 0; L.get_def locals_tail 1;
        L.get_def (S.Push (S.I32 stack_limit, S.Empty)) 0;
        Capacity.region_def 48 stack_capacity top stack_limit; Stack.previous_def 48 stack_limit;
        Capacity.region_def 48 D.Z top top; Stack.related_def blocks 48 memory top top Hmc_heap_state.Halt;
        Hmc_memory_saved_frame.slots_def blocks; Index.represents_def (D.S (Hmc_frame_capacity.capacity blocks)) 3;
        Stack.zero_def ());
      match Hmc_wasm_reservation.reserve (Hmc_memory_saved_frame.slots blocks) 3 0 48 () with
      | None -> failwith "guard span"
      | Some _ ->
        let result = Guard.correct Hmc_heap_state.Halt top 48 stack_capacity stack_limit 2 1 T.Empty outer blocks target capacity
          Hmc_closure_ir.Empty heap Runtime.Empty 0 0 3 (base + 64) signature activation cells H.Empty D.Empty_context ty G.Empty_temporaries
          (D.S D.Z) 0 0 0 fragment 3 padding 0 D.Z state base top 1024 bytes suffix 0 1 () in
        (match result with None -> if available then failwith "guard rejected room" | Some _ -> if not available then failwith "guard accepted full");
        let control = T.Block (Guard.emit fragment padding 0 1 48 2 1 T.Empty, T.Empty) in
        let cursor : B.u32 = if available then top + 48 else top in
        let final = {X.memory = expected; machine = {E.locals = S.Push (S.I32 base, S.Push (S.I32 cursor, S.Push (S.I32 stack_limit, S.Empty))); stack = S.Empty}} in
        if T.run (fuel 2000) {T.code = control; labels = T.No_labels; state} <> T.Finished final then failwith "guarded source save execution";
        {memory; expected; cursor; code = T.flatten control C.Empty;
          prefix = [I.I32_const base; I.Local_set 0; I.I32_const top; I.Local_set 1; I.I32_const stack_limit; I.Local_set 2]})
  | _ -> failwith "guard fixture setup"
let fixtures () = List.concat_map (fun base -> [fixture base true; fixture base false]) [0; 7]
