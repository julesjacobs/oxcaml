module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module L = Wasm_locals
module E = Wasm_execution
module M = Wasm_memory
module X = Wasm_memory_execution
module Header = Hmc_wasm_header_update
let[@def] (emit @ total) (offset : B.u32) (base_local : B.u32) (pointer_local : B.u32) =
  C.Next (I.Local_get base_local, C.Next (I.Local_get pointer_local,
    C.Next (I.Plain I.I64_extend_i32_u, C.Next (I.I64_store (3, offset), C.Empty))))
let (correct @ total) : (offset : B.u32) -> (base_local : B.u32) -> (pointer_local : B.u32) ->
    (state : X.state) @ immutable -> (base : B.u32) -> (pointer : B.u32) -> (memory : B.bytes) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && L.get state.X.machine.E.locals pointer_local === Some (S.I32 pointer)
      && M.store state.X.memory base offset (S.I64 (Header.number pointer)) === Some memory} ->
    {u : unit | X.run (emit offset base_local pointer_local) state === X.Done {X.memory; machine = state.X.machine}
      && Hmc_tagged_cell.length memory === Hmc_tagged_cell.length state.X.memory} @ ghost =
  fun offset base_local pointer_local state base pointer memory premise -> ghost_ (
    let a = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 base, state.X.machine.E.stack)}} in
    let b = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 pointer, a.X.machine.E.stack)}} in
    let c = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I64 (Header.number pointer), a.X.machine.E.stack)}} in
    let store = C.Next (I.I64_store (3, offset), C.Empty) in
    let extend = C.Next (I.Plain I.I64_extend_i32_u, store) in
    let load = C.Next (I.Local_get pointer_local, extend) in
    emit_def offset base_local pointer_local;
    X.run_def (emit offset base_local pointer_local) state; X.step_def (I.Local_get base_local) state; E.step_def (I.Local_get base_local) state.X.machine;
    X.run_def load a; X.step_def (I.Local_get pointer_local) a; E.step_def (I.Local_get pointer_local) a.X.machine;
    X.run_def extend b; X.step_def (I.Plain I.I64_extend_i32_u) b; E.step_def (I.Plain I.I64_extend_i32_u) b.X.machine;
    S.step_def (I.Plain I.I64_extend_i32_u) b.X.machine.E.stack; Header.number_def pointer;
    X.run_def store c; X.step_def (I.I64_store (3, offset)) c; X.write_def M.W64 offset c;
    X.compatible_def (S.I64 (Header.number pointer)) M.W64; M.width_def (S.I64 (Header.number pointer));
    X.run_def C.Empty {X.memory; machine = state.X.machine})
