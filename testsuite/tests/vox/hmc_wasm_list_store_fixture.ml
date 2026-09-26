module B = Wasm_u32
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Store = Hmc_wasm_list_store
module Capture = Hmc_wasm_list_capture
module Finish = Hmc_wasm_list_finish
module Write = Wasm_frame_write
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module I = Wasm_instruction
module C = Wasm_code
type fixture = {memory : B.bytes; expected : B.bytes; code : C.t}
let fixture (base : B.u32) closure =
  if base > 7 then failwith "list store base" else
  let pc = V.Word (Header.number 11) in let current = V.Closure_pointer 128 in let accumulator = V.Cons_pointer 256 in
  let head = if closure then V.Closure_pointer 512 else V.Word {Hmc_word64.lo = 42; hi = 37} in
  let tail = if closure then V.Nil else V.Cons_pointer 768 in
  let old_head = V.Word (Header.number 99) in let old_tail = V.Boolean true in
  let rest = Heap.Cell (V.Word (Header.number 17), Heap.Cell (V.Closure_pointer 1024, Heap.Empty)) in
  let suffix = B.Byte (173, B.Byte (29, B.End)) in
  let before_frame = Wire.encode_cells (Store.cells pc current accumulator old_head old_tail rest) suffix in
  let after_frame = Wire.encode_cells (Store.cells pc current accumulator head tail rest) suffix in
  let memory = Hmc_wasm_frame_fixture.prefix base before_frame in
  let expected = Wasm_memory_splice.replace memory base before_frame after_frame () in
  let slots = {Capture.head_tag = 1; head_payload = 2; tail_tag = 3; tail_payload = 4} in
  let locals = S.Push (S.I32 base, S.Push (S.I64 (V.tag head), S.Push (S.I64 (V.payload head),
    S.Push (S.I64 (V.tag tail), S.Push (S.I64 (V.payload tail), S.Empty))))) in
  ghost_ (Wasm_locals.get_def locals 0; Wasm_locals.get_def locals 1; Wasm_locals.get_def locals 2;
    Wasm_locals.get_def locals 3; Wasm_locals.get_def locals 4);
  ghost_ (Wasm_locals.get_def (S.Push (S.I64 (V.tag head), S.Push (S.I64 (V.payload head), S.Push (S.I64 (V.tag tail), S.Push (S.I64 (V.payload tail), S.Empty))))) 0;
    Wasm_locals.get_def (S.Push (S.I64 (V.tag head), S.Push (S.I64 (V.payload head), S.Push (S.I64 (V.tag tail), S.Push (S.I64 (V.payload tail), S.Empty))))) 1;
    Wasm_locals.get_def (S.Push (S.I64 (V.tag head), S.Push (S.I64 (V.payload head), S.Push (S.I64 (V.tag tail), S.Push (S.I64 (V.payload tail), S.Empty))))) 2;
    Wasm_locals.get_def (S.Push (S.I64 (V.tag head), S.Push (S.I64 (V.payload head), S.Push (S.I64 (V.tag tail), S.Push (S.I64 (V.payload tail), S.Empty))))) 3;
    Wasm_locals.get_def (S.Push (S.I64 (V.payload head), S.Push (S.I64 (V.tag tail), S.Push (S.I64 (V.payload tail), S.Empty)))) 0;
    Wasm_locals.get_def (S.Push (S.I64 (V.payload head), S.Push (S.I64 (V.tag tail), S.Push (S.I64 (V.payload tail), S.Empty)))) 1;
    Wasm_locals.get_def (S.Push (S.I64 (V.payload head), S.Push (S.I64 (V.tag tail), S.Push (S.I64 (V.payload tail), S.Empty)))) 2;
    Wasm_locals.get_def (S.Push (S.I64 (V.tag tail), S.Push (S.I64 (V.payload tail), S.Empty))) 0;
    Wasm_locals.get_def (S.Push (S.I64 (V.tag tail), S.Push (S.I64 (V.payload tail), S.Empty))) 1;
    Wasm_locals.get_def (S.Push (S.I64 (V.payload tail), S.Empty)) 0);
  let state = {X.memory; machine = {E.locals; stack = S.Empty}} in
  ghost_ (Store.correct pc current accumulator old_head old_tail head tail rest locals slots memory expected base before_frame after_frame suffix ();
    Write.correct (Finish.writes slots) 0 state base expected ());
  let stores = Write.emit (Finish.writes slots) 0 in
  (match X.run stores state with
  | X.Done out -> if out.X.memory <> expected || out.X.machine <> state.X.machine then failwith "list store representation"
  | _ -> failwith "list store execution");
  let prefix = C.Next (I.I32_const base, C.Next (I.Local_set 0,
    C.Next (I.I64_const (V.tag head), C.Next (I.Local_set 1,
    C.Next (I.I64_const (V.payload head), C.Next (I.Local_set 2,
    C.Next (I.I64_const (V.tag tail), C.Next (I.Local_set 3,
    C.Next (I.I64_const (V.payload tail), C.Next (I.Local_set 4, C.Empty)))))))))) in
  {memory; expected; code = E.append prefix (E.append stores (C.Next (I.I32_const 0, C.Empty)))}
let fixtures () = [fixture 0 false; fixture 7 false; fixture 0 true; fixture 7 true]
