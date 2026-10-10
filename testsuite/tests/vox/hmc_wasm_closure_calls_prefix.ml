module B = Wasm_u32
module C = Wasm_code
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module K = Hmc_closure_ir
module Image = Hmc_heap_image
module Index = Hmc_u32_index
module Relayout = Hmc_wasm_relayout
module Range = Hmc_wasm_range_copy
module Plan = Wasm_parallel_copy
module Bounds = Hmc_linear_bounds
module Bytes = Hmc_linear_bytes
module Preserve = Hmc_linear_preservation
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Lower = Hmc_wasm_closure_write
module Memory = Hmc_wasm_closure_memory
module Write_prefix = Hmc_wasm_closure_write_prefix
module Closure = Hmc_wasm_closure_prefix
module Prefix = Wasm_instruction_prefix
module T = Wasm_control
module Lift = Wasm_control_lift
module P = Wasm_instance_control
module M = Wasm_calls
module F = Wasm_functions
module Calls = Wasm_calls_instruction_prefix

let (correct @ total) : (table : K.table) @ immutable -> (heap : Heap.heap) @ immutable ->
    (captures : Heap.cells) @ immutable -> (count : Relayout.count) -> (fragment : Lower.fragment) @ immutable ->
    (state : X.state) @ immutable -> (frame_base : B.u32) -> (heap_base : B.u32) -> (limit : B.u32) ->
    (frame_local : B.u32) -> (heap_local : B.u32) -> (fuel : C.count) @ immutable ->
    (module_ : F.module_) @ immutable -> (configuration : M.configuration) @ immutable -> (tail : T.code) @ immutable ->
    {u : unit | Heap.valid table heap && Heap.used heap = heap_base && Image.related state.X.memory heap
      && Index.represents (Heap.length captures) count && 2 + count <= 268435452
      && fragment.Lower.bytes = 16 + 16 * count && heap_base + fragment.Lower.bytes <= limit
      && Relayout.range_is fragment.Lower.copies 2 0 (Heap.length captures) Plan.End
      && Range.reads state.X.memory frame_base (Memory.environment_position ()) captures && Bounds.covers state.X.memory limit
      && L.get state.X.machine.E.locals frame_local === Some (S.I32 frame_base)
      && L.get state.X.machine.E.locals heap_local === Some (S.I32 heap_base)
      && configuration.M.current.P.body.T.state === state
      && configuration.M.current.P.body.T.code === Lift.embed (Lower.emit fragment frame_local heap_local) tail} ->
    {u : unit | match M.run (C.length (Prefix.take fuel (Lower.emit fragment frame_local heap_local))) module_ configuration with
      | M.Running current -> current.M.callers === configuration.M.callers && current.M.capacity === configuration.M.capacity
        && current.M.result === configuration.M.result && current.M.current.P.globals === configuration.M.current.P.globals
        && current.M.current.P.body.T.labels === configuration.M.current.P.body.T.labels
        && current.M.current.P.body.T.code === Lift.embed (Calls.drop fuel (Lower.emit fragment frame_local heap_local)) tail
        && (let execution = current.M.current.P.body.T.state in
          execution.X.machine.E.locals === state.X.machine.E.locals
          && Write_prefix.partial fragment state.X.memory frame_base heap_base execution.X.memory
          && Image.related execution.X.memory heap && Bounds.covers execution.X.memory limit
          && Preserve.equal_prefix heap_base state.X.memory execution.X.memory
          && Bytes.drop state.X.memory (S.add32 heap_base fragment.Lower.bytes)
            === Bytes.drop execution.X.memory (S.add32 heap_base fragment.Lower.bytes)
          && V.length execution.X.memory === V.length state.X.memory)
      | _ -> false} @ ghost =
  fun table heap captures count fragment state frame_base heap_base limit frame_local heap_local fuel module_ configuration tail premise -> ghost_ (
    Closure.correct table heap captures count fragment state frame_base heap_base limit frame_local heap_local fuel ();
    match X.run (Prefix.take fuel (Lower.emit fragment frame_local heap_local)) state with
    | X.Done current -> Calls.correct fuel (Lower.emit fragment frame_local heap_local) tail module_ configuration current ()
    | _ -> ())
