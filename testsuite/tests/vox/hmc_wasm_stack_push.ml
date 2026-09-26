module B = Wasm_u32
module G = Hmc_cfg_ir
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module Stack = Hmc_memory_stack
module Saved = Hmc_memory_saved_frame
module Extent = Hmc_heap_extent
module C = Wasm_code
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module P = Hmc_linear_preservation
module Advance = Hmc_wasm_allocation_advance
let[@def] (emit @ total) (body : C.t @ immutable) (width : B.u32) (top_local : B.u32) = E.append body (Advance.emit width top_local)
let (correct @ total) : (blocks : G.table) @ immutable -> (width : B.u32) -> (base : B.u32) -> (top : B.u32) -> (stop : B.u32) -> (limit : B.u32) ->
    (frames : Q.frames) @ immutable -> (saved : F.activation) @ immutable -> (body : C.t) @ immutable ->
    (state : X.state) @ immutable -> (memory : B.bytes) @ immutable -> (top_local : B.u32) ->
    {u : unit | width > 0 && base <= top && stop <= limit && top + width = stop
      && Extent.span (Saved.slots blocks) top stop && Stack.related blocks width state.X.memory base top frames
      && X.run body state === X.Done {X.memory = memory; machine = state.X.machine}
      && Saved.load blocks memory top === Some saved && P.equal_prefix top state.X.memory memory
      && L.get state.X.machine.E.locals top_local === Some (S.I32 top)} ->
    {locals : S.stack | X.run (emit body width top_local) state === X.Done {X.memory = memory; machine = {E.locals; stack = state.X.machine.E.stack}}
      && Stack.related blocks width memory base stop (Q.Frame (saved, frames))
      && L.replaced state.X.machine.E.locals top_local (S.I32 stop) locals && L.get locals top_local === Some (S.I32 stop)
      && L.same_types state.X.machine.E.locals locals} @ immutable =
  fun blocks width base top stop limit frames saved body state memory top_local premise ->
    let locals = Advance.correct width top_local top limit {X.memory = memory; machine = state.X.machine} () in
    ghost_ (S.add32_def top width;
      Stack.preserve blocks width state.X.memory memory base top frames top ();
      Stack.previous_def width stop; Stack.related_def blocks width memory base stop (Q.Frame (saved, frames));
      emit_def body width top_local; X.append_correct body (Advance.emit width top_local) state);
    locals
