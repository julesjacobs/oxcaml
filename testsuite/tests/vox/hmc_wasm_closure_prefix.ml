module B = Wasm_u32
module C = Wasm_code
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module K = Hmc_closure_ir
module Image = Hmc_heap_image
module Index = Hmc_u32_index
module Relayout = Hmc_wasm_relayout
module Range = Hmc_wasm_range_copy
module Range_prefix = Hmc_wasm_range_prefix
module Plan = Wasm_parallel_copy
module Copy = Wasm_cross_copy_prefix
module Bounds = Hmc_linear_bounds
module Bytes = Hmc_linear_bytes
module P = Hmc_linear_preservation
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Lower = Hmc_wasm_closure_write
module Memory = Hmc_wasm_closure_memory
module Write_prefix = Hmc_wasm_closure_write_prefix
module Prefix = Wasm_instruction_prefix

let (correct @ total) : (table : K.table) @ immutable -> (heap : Heap.heap) @ immutable ->
    (captures : Heap.cells) @ immutable -> (count : Relayout.count) -> (fragment : Lower.fragment) @ immutable ->
    (state : X.state) @ immutable -> (frame_base : B.u32) -> (heap_base : B.u32) -> (limit : B.u32) ->
    (frame_local : B.u32) -> (heap_local : B.u32) -> (fuel : C.count) @ immutable ->
    {u : unit | Heap.valid table heap && Heap.used heap = heap_base && Image.related state.X.memory heap
      && Index.represents (Heap.length captures) count && 2 + count <= 268435452
      && fragment.Lower.bytes = 16 + 16 * count && heap_base + fragment.Lower.bytes <= limit
      && Relayout.range_is fragment.Lower.copies 2 0 (Heap.length captures) Plan.End
      && Range.reads state.X.memory frame_base (Memory.environment_position ()) captures && Bounds.covers state.X.memory limit
      && L.get state.X.machine.E.locals frame_local === Some (S.I32 frame_base)
      && L.get state.X.machine.E.locals heap_local === Some (S.I32 heap_base)} ->
    {u : unit | match X.run (Prefix.take fuel (Lower.emit fragment frame_local heap_local)) state with
      | X.Done current -> current.X.machine.E.locals === state.X.machine.E.locals
        && Write_prefix.partial fragment state.X.memory frame_base heap_base current.X.memory
        && Image.related current.X.memory heap && Bounds.covers current.X.memory limit
        && P.equal_prefix heap_base state.X.memory current.X.memory
        && Bytes.drop state.X.memory (S.add32 heap_base fragment.Lower.bytes)
          === Bytes.drop current.X.memory (S.add32 heap_base fragment.Lower.bytes)
        && V.length current.X.memory === V.length state.X.memory
      | _ -> false} @ ghost =
  fun table heap captures count fragment state frame_base heap_base limit frame_local heap_local fuel premise -> ghost_ (
    let written = Memory.correct captures count fragment state frame_base heap_base limit frame_local heap_local () in
    let after = {X.memory = written.Memory.memory; machine = state.X.machine} in
    let stop : B.u32 = heap_base + fragment.Lower.bytes in
    Copy.bounded_def Plan.End heap_base heap_base stop;
    Range_prefix.bounded fragment.Lower.copies Plan.End 2 0 (Heap.length captures) count heap_base heap_base stop ();
    let _ = Bounds.suffix state.X.memory limit heap_base () in
    Bounds.covers_def state.X.memory heap_base;
    Write_prefix.correct fragment frame_local heap_local state frame_base heap_base after heap_base stop fuel ();
    S.add32_def heap_base fragment.Lower.bytes;
    match X.run (Prefix.take fuel (Lower.emit fragment frame_local heap_local)) state with
    | X.Done current ->
      Image.preserve table state.X.memory current.X.memory heap heap_base ();
      Bounds.same_length state.X.memory current.X.memory limit ()
    | _ -> ())
