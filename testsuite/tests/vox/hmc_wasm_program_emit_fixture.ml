module B = Wasm_u32
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module T = Wasm_control
module I = Wasm_instruction
module Emit = Hmc_wasm_program_emit
let rec fuel n = if n = 0 then Wasm_code.Zero else Wasm_code.Succ (fuel (n - 1))
let run code state = match T.run (fuel 200) {T.code; labels = T.No_labels; state} with
  | T.Finished state -> state
  | _ -> failwith "program block did not finish"
let protection () =
  let state = {X.memory = B.Byte (77, B.End); machine = {E.locals = S.Push (S.I32 99, S.Empty); stack = S.Empty}} in
  let check failure condition expected =
    let body = T.Instruction (I.I32_const condition, T.If (T.Empty, T.Instruction (I.Br 2, T.Empty), T.Empty)) in
    let after = run (Emit.protected 0 failure body) state in
    if Wasm_locals.get after.X.machine.E.locals 0 <> Some (S.I32 expected) || after.X.memory <> state.X.memory then
      failwith "resource status branch" in
  check 2 1 0; check 2 0 2; check 3 1 0; check 3 0 3
let root () =
  let module Structured = Hmc_wasm_structured_block in
  let module Capture = Hmc_wasm_cons_capture in
  let module Slots = Hmc_wasm_descriptor_load in
  let module Lower = Hmc_wasm_program_lower in
  let module Heap = Hmc_heap_objects in
  let module V = Hmc_tagged_cell in
  let word = {Hmc_word64.lo = 4294967295; hi = 4294967295} in
  let memory = Hmc_heap_wire.encode_cells (Heap.Cell (V.Nil, Heap.Cell (V.Nil, Heap.Cell (V.Word word, Heap.Empty)))) B.End in
  let rec locals n = if n = 17 then S.Empty else
    let value = if n = 15 || n = 16 then S.I64 {Hmc_word64.lo = 0; hi = 0} else S.I32 (if n = 11 then 4096 else 0) in
    S.Push (value, locals (n + 1)) in
  let state = {X.memory; machine = {E.locals = locals 0; stack = S.Empty}} in
  let config = {Emit.structured = {Structured.frame = 1; heap = 7; limit = 8; object_ = 0;
      scratch = {Capture.head_tag = 9; head_payload = 10; tail_tag = 2; tail_payload = 3}};
    top = 11; stack_limit = 12; code = 5; address = 4;
    descriptor = {Slots.start = 6; captures = 7; recursive = 8}; status = 14; result_tag = 15; result_payload = 16} in
  let program = {Lower.blocks = Hmc_wasm_program_table.Empty; calls = Hmc_wasm_call_plan_table.Empty;
    restore = Wasm_parallel_copy.End; capacity = 0; width = 16} in
  let after = run (Emit.emit program Hmc_wasm_program_block.Return config 0 4096) state in
  if Wasm_locals.get after.X.machine.E.locals 14 <> Some (S.I32 1)
    || Wasm_locals.get after.X.machine.E.locals 15 <> Some (S.I64 (V.tag (V.Word word)))
    || Wasm_locals.get after.X.machine.E.locals 16 <> Some (S.I64 word)
    || after.X.memory <> memory then failwith "program root return"
let fixtures () = protection (); root ()
