module B = Wasm_u32
module E = Wasm_execution
module Copy = Wasm_parallel_copy
module Relayout = Hmc_wasm_relayout
module Pop = Hmc_wasm_value_pop
module Write = Hmc_wasm_cons_result
type fragment = Pop.fragment
let[@def] (moves @ total) (fragment : fragment @ immutable) = Pop.moves fragment
let[@def] (emit @ total) (fragment : fragment @ immutable) (base_local : B.u32) (heap_local : B.u32) =
  E.append (Write.emit base_local heap_local) (Relayout.emit (moves fragment) base_local)
let (correct @ total) : (fragment : fragment) @ immutable -> (base_local : B.u32) -> (heap_local : B.u32) ->
    (state : Wasm_memory_execution.state) @ immutable -> (base : B.u32) ->
    (computed : B.bytes) @ immutable -> (copied : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    {u : unit | Wasm_locals.get state.Wasm_memory_execution.machine.E.locals base_local === Some (Wasm_scalar.I32 base)
      && Wasm_memory_execution.run (Write.emit base_local heap_local) state ===
        Wasm_memory_execution.Done {Wasm_memory_execution.memory = computed; machine = state.Wasm_memory_execution.machine}
      && Copy.apply fragment.copies computed base === Some copied && Relayout.finish copied base fragment.Pop.pc === Some after} ->
    {u : unit | Wasm_memory_execution.run (emit fragment base_local heap_local) state ===
      Wasm_memory_execution.Done {Wasm_memory_execution.memory = after; machine = state.Wasm_memory_execution.machine}} @ ghost =
  fun fragment base_local heap_local state base computed copied after premise -> ghost_ (
    moves_def fragment; Pop.moves_def fragment; emit_def fragment base_local heap_local;
    Relayout.correct (moves fragment) base_local {Wasm_memory_execution.memory = computed; machine = state.Wasm_memory_execution.machine} base copied after ();
    Wasm_memory_execution.append_correct (Write.emit base_local heap_local) (Relayout.emit (moves fragment) base_local) state)
