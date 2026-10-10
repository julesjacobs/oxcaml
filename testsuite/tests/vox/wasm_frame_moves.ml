module B = Wasm_u32
module C = Wasm_code
module S = Wasm_scalar
module L = Wasm_locals
module E = Wasm_execution
module X = Wasm_memory_execution
module Read = Wasm_frame_snapshot
module Write = Wasm_frame_write
let[@def] (emit @ total) (reads : Read.reads @ immutable) (writes : Write.writes @ immutable)
    (base_local : B.u32) : C.t @ immutable = E.append (Read.emit reads base_local) (Write.emit writes base_local)
let (correct @ total) : (reads : Read.reads) @ immutable -> (writes : Write.writes) @ immutable -> (base_local : B.u32) ->
    (state : X.state) @ immutable -> (base : B.u32) -> (snapshot : S.stack) @ immutable -> (memory : B.bytes) @ immutable ->
    {u : unit | Read.separate reads base_local
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && Read.project reads state.X.memory base state.X.machine.E.locals === Some snapshot
      && Write.apply writes state.X.memory base snapshot === Some memory} ->
    {u : unit | X.run (emit reads writes base_local) state === X.Done
      {X.memory; machine = {E.locals = snapshot; stack = state.X.machine.E.stack}}
      && Hmc_tagged_cell.length memory === Hmc_tagged_cell.length state.X.memory} @ ghost =
  fun reads writes base_local state base snapshot memory premise -> ghost_ (
    Read.correct reads base_local state base snapshot ();
    let captured = {X.memory = state.X.memory; machine = {E.locals = snapshot; stack = state.X.machine.E.stack}} in
    Write.correct writes base_local captured base memory ();
    emit_def reads writes base_local; X.append_correct (Read.emit reads base_local) (Write.emit writes base_local) state)
