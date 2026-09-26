module B = Wasm_u32
module W = Hmc_word64
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module L = Wasm_locals
module E = Wasm_execution
module M = Wasm_memory
module X = Wasm_memory_execution

let[@def] (emit @ total) (offset : B.u32) (base_local : B.u32) (word : W.t @ immutable) : C.t @ immutable =
  C.Next (I.Local_get base_local, C.Next (I.I64_const word, C.Next (I.I64_store (3, offset), C.Empty)))
let (correct @ total) : (offset : B.u32) -> (base_local : B.u32) -> (word : W.t) @ immutable ->
    (state : X.state) @ immutable -> (base : B.u32) -> (memory : B.bytes) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && M.store state.X.memory base offset (S.I64 word) === Some memory} ->
    {u : unit | X.run (emit offset base_local word) state === X.Done {X.memory; machine = state.X.machine}
      && Hmc_tagged_cell.length memory === Hmc_tagged_cell.length state.X.memory} @ ghost =
  fun offset base_local word state base memory premise -> ghost_ (
    let addressed = {X.memory = state.X.memory; machine = {E.locals = state.X.machine.E.locals;
      stack = S.Push (S.I32 base, state.X.machine.E.stack)}} in
    let operands = {X.memory = state.X.memory; machine = {E.locals = state.X.machine.E.locals;
      stack = S.Push (S.I64 word, addressed.X.machine.E.stack)}} in
    let write = C.Next (I.I64_store (3, offset), C.Empty) in
    let value = C.Next (I.I64_const word, write) in
    emit_def offset base_local word;
    X.run_def (emit offset base_local word) state; X.step_def (I.Local_get base_local) state;
    E.step_def (I.Local_get base_local) state.X.machine;
    X.run_def value addressed; X.step_def (I.I64_const word) addressed;
    E.step_def (I.I64_const word) addressed.X.machine; S.step_def (I.I64_const word) addressed.X.machine.E.stack;
    X.run_def write operands; X.step_def (I.I64_store (3, offset)) operands;
    M.width_def (S.I64 word); X.write_def M.W64 offset operands; X.compatible_def (S.I64 word) M.W64;
    X.run_def C.Empty {X.memory; machine = state.X.machine})
