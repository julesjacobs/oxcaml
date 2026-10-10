module B = Wasm_u32
module C = Wasm_code
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module K = Hmc_closure_ir
module A = Hmc_heap_allocate
module Machine = Hmc_heap_machine
module Image = Hmc_heap_image
module Bounds = Hmc_linear_bounds
module Index = Hmc_u32_index
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Write = Hmc_wasm_closure_write
module Memory = Hmc_wasm_closure_memory
module Advance = Hmc_wasm_allocation_advance
module Advance_prefix = Hmc_wasm_allocation_advance_prefix
module Allocate = Hmc_wasm_closure_allocate
module Closure = Hmc_wasm_closure_prefix
module Write_prefix = Hmc_wasm_closure_write_prefix
module Prefix = Wasm_instruction_prefix
module Range = Hmc_wasm_range_copy
module Plan = Wasm_parallel_copy
module Bytes = Hmc_linear_bytes
module P = Hmc_linear_preservation

let (correct @ total) : (table : K.table) @ immutable -> (heap : Heap.heap) @ immutable -> (id : D.index) @ immutable ->
    (captures : Heap.cells) @ immutable -> (count : Hmc_wasm_relayout.count) -> (fragment : Write.fragment) @ immutable ->
    (state : X.state) @ immutable -> (frame_base : B.u32) -> (base : B.u32) -> (limit : B.u32) ->
    (frame_local : B.u32) -> (heap_local : B.u32) -> (fuel : C.count) @ immutable ->
    {u : unit | Heap.valid table heap && Heap.object_valid table (Heap.view heap) (Heap.Closure (id, captures))
      && Index.represents id fragment.Write.code && Index.represents (Heap.length captures) count && 2 + count <= 268435452
      && fragment.Write.bytes = 16 + 16 * count
      && Hmc_wasm_relayout.range_is fragment.Write.copies 2 0 (Heap.length captures) Plan.End
      && Range.reads state.X.memory frame_base (Memory.environment_position ()) captures
      && Heap.used heap = base && base + fragment.Write.bytes <= limit && Image.related state.X.memory heap && Bounds.covers state.X.memory limit
      && L.get state.X.machine.E.locals frame_local === Some (S.I32 frame_base)
      && L.get state.X.machine.E.locals heap_local === Some (S.I32 base)} ->
    {out : Allocate.result | Machine.allocate heap limit (Heap.Closure (id, captures)) === A.Allocated out.Allocate.allocation
      && A.correct table heap limit (Heap.Closure (id, captures)) (A.Allocated out.Allocate.allocation)
      && Heap.used out.Allocate.allocation.A.heap = base + fragment.Write.bytes
      && X.run (Allocate.emit fragment frame_local heap_local) state === X.Done out.Allocate.state
      && match X.run (Prefix.take fuel (Allocate.emit fragment frame_local heap_local)) state with
        | X.Done current -> Bounds.covers current.X.memory limit
          && P.equal_prefix base state.X.memory current.X.memory
          && Bytes.drop state.X.memory (S.add32 base fragment.Write.bytes)
            === Bytes.drop current.X.memory (S.add32 base fragment.Write.bytes)
          && V.length current.X.memory === V.length state.X.memory
          && ((current.X.machine.E.locals === state.X.machine.E.locals
              && Image.related current.X.memory heap
              && (Write_prefix.partial fragment state.X.memory frame_base base current.X.memory
                || current.X.memory === out.Allocate.state.X.memory))
            || (L.replaced state.X.machine.E.locals heap_local (S.I32 (S.add32 base fragment.Write.bytes)) current.X.machine.E.locals
              && L.get current.X.machine.E.locals heap_local === Some (S.I32 (S.add32 base fragment.Write.bytes))
              && current.X.memory === out.Allocate.state.X.memory
              && Image.related current.X.memory out.Allocate.allocation.A.heap))
        | _ -> false} @ ghost =
  fun table heap id captures count fragment state frame_base base limit frame_local heap_local fuel premise -> ghost_ (
    let out = Allocate.correct table heap id captures count fragment state frame_base base limit frame_local heap_local () in
    let written = Memory.correct captures count fragment state frame_base base limit frame_local heap_local () in
    let middle = {X.memory = written.Memory.memory; machine = state.X.machine} in
    let write = Write.emit fragment frame_local heap_local in
    let advance = Advance.emit fragment.Write.bytes heap_local in
    let _ = Advance.correct fragment.Write.bytes heap_local base limit middle () in
    Allocate.emit_def fragment frame_local heap_local;
    X.append_correct write advance state;
    Prefix.run_append fuel write advance state middle ();
    (match Prefix.remaining fuel write with
    | None -> Closure.correct table heap captures count fragment state frame_base base limit frame_local heap_local fuel ()
    | Some rest ->
      Image.preserve table state.X.memory middle.X.memory heap base ();
      Advance_prefix.correct fragment.Write.bytes heap_local base limit middle rest ());
    out)
