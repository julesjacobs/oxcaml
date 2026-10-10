module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Frame = Hmc_heap_frame
module Codec = Hmc_pointer_frame_codec
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module T = Wasm_control
module I = Wasm_instruction
module C = Wasm_code
module Capture = Hmc_wasm_cons_capture
module Conditional = Hmc_wasm_cons_conditional
module Lift = Wasm_control_lift
module Fuel = Wasm_control_compose
type fixture = {memory : B.bytes; expected : B.bytes; prefix : I.t list; code : C.t; cursor : B.u32}
let rec filler n = if n = 0 then B.End else B.Byte (173, filler (n - 1))
let fixture (base : B.u32) =
  if base > 7 then failwith "operand fixture base" else
  let context1 = D.Binding (D.Forall (D.Z, D.Word64), D.Empty_context) in
  let context2 = D.Binding (D.Forall (D.Z, D.Word64), context1) in
  let signature = {G.locals = context2; temporaries = G.Value (context1, D.Word64, G.Empty_temporaries); accumulator = Some (D.List_type D.Word64)} in
  let head = V.Word (Header.number 42) in
  let activation = {Frame.pc = Hmc_wasm_control_fixture.index 7; current = V.Closure_pointer 64; accumulator = V.Nil;
    env = Heap.Cell (V.Word (Header.number 9), Heap.Cell (V.Word (Header.number 10), Heap.Empty));
    temporaries = Frame.Value (head, Heap.Cell (V.Word (Header.number 11), Heap.Empty), Frame.Empty)} in
  if not (Codec.shape signature activation) then failwith "operand frame shape" else
  let padding = Heap.Cell (V.Nil, Heap.Cell (V.Nil, Heap.Empty)) in
  let cells = Codec.encode signature activation padding () in
  let suffix = filler 128 in
  let full = Heap.Cell (V.Word (Header.number 7), cells) in
  let bytes = Wire.encode_cells full suffix in
  let memory = Hmc_wasm_frame_fixture.prefix base bytes in
  let heap_base : B.u32 = base + 192 in
  let limit : B.u32 = heap_base + 32 in
  let slots = {Capture.head_tag = 3; head_payload = 4; tail_tag = 5; tail_payload = 6} in
  let scratch = S.Push (S.I64 (Header.number 999), S.Push (S.I64 (Header.number 999), S.Push (S.I64 (Header.number 999), S.Push (S.I64 (Header.number 999), S.Empty)))) in
  let locals = S.Push (S.I32 base, S.Push (S.I32 heap_base, S.Push (S.I32 limit, scratch))) in
  if not (Capture.distinct slots && Capture.separate slots 0 && Capture.separate slots 1 && Capture.separate slots 2 && Capture.writable slots locals) then failwith "operand scratch layout" else
  let state = {X.memory; machine = {E.locals; stack = S.Empty}} in
  ghost_ (Heap.length_def full; Wasm_locals.get_def locals 0;
    Wasm_locals.get_def locals 1; Wasm_locals.get_def (S.Push (S.I32 heap_base, S.Push (S.I32 limit, scratch))) 0;
    Wasm_locals.get_def locals 2; Wasm_locals.get_def (S.Push (S.I32 heap_base, S.Push (S.I32 limit, scratch))) 1; Wasm_locals.get_def (S.Push (S.I32 limit, scratch)) 0;
    Codec.locals_size_def context2; Codec.locals_size_def context1; Codec.locals_size_def D.Empty_context;
    Hmc_u32_index.represents_def (D.S (D.S D.Z)) 2; Hmc_u32_index.represents_def (D.S D.Z) 1; Hmc_u32_index.represents_def D.Z 0);
  let captured_locals = Hmc_wasm_cons_operands.correct signature activation context1 D.Word64 G.Empty_temporaries head cells padding 7 2 state base 0 1 2 slots bytes suffix () in
  let captured = {state with X.machine = {state.X.machine with E.locals = captured_locals}} in
  let capture_code = Capture.emit (Hmc_wasm_simple_lower.slot_tag 2) (Hmc_wasm_simple_lower.slot_payload 2) slots 0 in
  match Hmc_linear_bytes.drop memory limit with
  | None -> failwith "operand heap coverage"
  | Some _ ->
    let heap = Heap.Empty_heap heap_base in
    ghost_ (Hmc_linear_bounds.covers_def memory limit; Heap.valid_def Hmc_closure_ir.Empty heap; Heap.used_def heap;
      Hmc_heap_image.related_def memory heap; Hmc_wasm_cons_fixture.word_object heap (Header.number 42));
    match Hmc_u32_index.encode 8 (Heap.length cells) with
    | None -> failwith "Cons frame capacity"
    | Some capacity -> if capacity <> 8 then failwith "Cons frame size" else
    let next = Hmc_wasm_control_fixture.index 9 in
    let next_signature = {G.locals = context1; temporaries = G.Empty_temporaries; accumulator = Some (D.List_type D.Word64)} in
    let next_activation = {activation with Frame.pc = next; accumulator = V.Cons_pointer heap_base;
      env = Heap.Cell (V.Word (Header.number 11), Heap.Empty); temporaries = Frame.Empty} in
    let frame_stop : B.u32 = base + 144 in
    match Hmc_wasm_value_pop.build signature next 8 100 with
    | None -> failwith "Cons finish layout"
    | Some fragment ->
      ghost_ (Hmc_frame_value_pop.successor_def signature (D.List_type D.Word64);
        Hmc_frame_value_pop.transition_def activation next (V.Cons_pointer heap_base);
        Hmc_heap_image_suffix.above_def heap frame_stop);
      let result = Hmc_wasm_cons_success.correct signature next_signature activation next_activation Hmc_heap_state.Halt
        (D.List_type D.Word64) D.Word64 next context1 G.Empty_temporaries fragment 8 100 7 head V.Nil
        heap_base limit 1 cells padding Hmc_closure_ir.Empty heap limit 3 4 5 6 frame_stop captured 0 base bytes suffix () in
      let finish_code = Hmc_wasm_cons_success.emit fragment 0 1 3 4 5 6 in
      let code = E.append capture_code finish_code in
      ghost_ (X.append_correct capture_code finish_code state);
      (match X.run code state with
      | X.Done actual -> if actual <> result.Hmc_wasm_cons_success.state then failwith "Cons finish execution"
      | _ -> failwith "Cons finish incomplete");
      let prefix = [I.I32_const base; I.Local_set 0; I.I32_const heap_base; I.Local_set 1; I.I32_const limit; I.Local_set 2;
        I.I64_const (Header.number 999); I.Local_set 3; I.I64_const (Header.number 999); I.Local_set 4;
        I.I64_const (Header.number 999); I.Local_set 5; I.I64_const (Header.number 999); I.Local_set 6] in
      {memory; expected = result.Hmc_wasm_cons_success.state.X.memory; prefix; code; cursor = limit}
let fixtures () = [fixture 0; fixture 7]
