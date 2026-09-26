module B = Wasm_u32
module W = Hmc_word64
module V = Hmc_tagged_cell
module G = Hmc_cfg_ir
module H = Hmc_heap_objects
module F = Wasm_frame_literals
let[@def] (literal @ total) (atom : G.atom @ immutable) = match atom with
  | G.Truth -> Some (V.Boolean true) | G.False -> Some (V.Boolean false)
  | G.Word word -> Some (V.Word word) | G.Nil -> Some V.Nil
  | G.Local _ | G.Global _ | G.Closure _ -> None
let (source @ total) : (atom : G.atom) @ immutable -> (env : H.cells) @ immutable ->
    (value : V.value) @ immutable -> {u : unit | literal atom === Some value} ->
    {u : unit | Hmc_heap_simple.load env atom === Some value} @ ghost = fun atom env value premise -> ghost_ (
  literal_def atom; Hmc_heap_simple.load_def env atom)
let[@def] (writes @ total) (pc : W.limb) (value : V.value @ immutable) : F.writes @ immutable =
  F.Write (32, V.tag value, F.Write (40, V.payload value, F.Write (8, {W.lo = pc; hi = 0}, F.End)))
let[@def] (emit @ total) (pc : W.limb) (value : V.value @ immutable) (base_local : B.u32) =
  F.emit (writes pc value) base_local
let (correct @ total) : (pc : W.limb) -> (value : V.value) @ immutable -> (base_local : B.u32) ->
    (state : Wasm_memory_execution.state) @ immutable -> (base : B.u32) -> (memory : B.bytes) @ immutable ->
    {u : unit | Wasm_locals.get state.Wasm_memory_execution.machine.Wasm_execution.locals base_local
        === Some (Wasm_scalar.I32 base)
      && F.apply (writes pc value) state.Wasm_memory_execution.memory base === Some memory} ->
    {u : unit | Wasm_memory_execution.run (emit pc value base_local) state
      === Wasm_memory_execution.Done {Wasm_memory_execution.memory; machine = state.Wasm_memory_execution.machine}
      && V.length memory === V.length state.Wasm_memory_execution.memory} @ ghost =
  fun pc value base_local state base memory premise -> ghost_ (
    emit_def pc value base_local; F.correct (writes pc value) base_local state base memory ())
