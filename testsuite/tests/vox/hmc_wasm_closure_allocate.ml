module B = Wasm_u32
module W = Hmc_word64
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
module Reserve = Hmc_wasm_reservation
module Range = Hmc_wasm_range_copy
module Plan = Wasm_parallel_copy
let[@def] (emit @ total) (fragment : Write.fragment @ immutable) (frame_local : B.u32) (heap_local : B.u32) =
  E.append (Write.emit fragment frame_local heap_local) (Advance.emit fragment.Write.bytes heap_local)
type result = {allocation : A.allocation; state : X.state}
let (correct @ total) : (table : K.table) @ immutable -> (heap : Heap.heap) @ immutable -> (id : D.index) @ immutable ->
    (captures : Heap.cells) @ immutable -> (count : Hmc_wasm_relayout.count) -> (fragment : Write.fragment) @ immutable ->
    (state : X.state) @ immutable -> (frame_base : B.u32) -> (base : B.u32) -> (limit : B.u32) -> (frame_local : B.u32) -> (heap_local : B.u32) ->
    {u : unit | Heap.valid table heap && Heap.object_valid table (Heap.view heap) (Heap.Closure (id, captures))
      && Index.represents id fragment.Write.code && Index.represents (Heap.length captures) count && 2 + count <= 268435452
      && fragment.Write.bytes = 16 + 16 * count
      && Hmc_wasm_relayout.range_is fragment.Write.copies 2 0 (Heap.length captures) Plan.End
      && Range.reads state.X.memory frame_base (Memory.environment_position ()) captures
      && Heap.used heap = base && base + fragment.Write.bytes <= limit && Image.related state.X.memory heap && Bounds.covers state.X.memory limit
      && L.get state.X.machine.E.locals frame_local === Some (S.I32 frame_base)
      && L.get state.X.machine.E.locals heap_local === Some (S.I32 base)} ->
    {out : result | Machine.allocate heap limit (Heap.Closure (id, captures)) === A.Allocated out.allocation
      && A.correct table heap limit (Heap.Closure (id, captures)) (A.Allocated out.allocation)
      && X.run (emit fragment frame_local heap_local) state === X.Done out.state
      && Image.related out.state.X.memory out.allocation.A.heap && Bounds.covers out.state.X.memory limit
      && V.length out.state.X.memory === V.length state.X.memory
      && Hmc_linear_preservation.equal_prefix base state.X.memory out.state.X.memory
      && Hmc_linear_bytes.drop out.state.X.memory (S.add32 base fragment.Write.bytes) === Hmc_linear_bytes.drop state.X.memory (S.add32 base fragment.Write.bytes)
      && Heap.used out.allocation.A.heap = base + fragment.Write.bytes
      && out.allocation.A.reference === V.Closure_pointer base
      && L.get out.state.X.machine.E.locals heap_local === Some (S.I32 (Heap.used out.allocation.A.heap))
      && L.replaced state.X.machine.E.locals heap_local (S.I32 (Heap.used out.allocation.A.heap)) out.state.X.machine.E.locals
      && out.state.X.machine.E.stack === state.X.machine.E.stack} @ immutable =
  fun table heap id captures count fragment state frame_base base limit frame_local heap_local premise ->
    let slots = D.S (Heap.length captures) in
    ghost_ (Heap.slots_def (Heap.Closure (id, captures)); Index.represents_def slots (count + 1);
      Reserve.fits slots (count + 1) base limit ());
    let allocation = A.sufficient table heap limit (Heap.Closure (id, captures)) () in
    ghost_ (A.correct_def table heap limit (Heap.Closure (id, captures)) (A.Allocated allocation);
      Reserve.span slots (count + 1) base (Heap.used allocation.A.heap) ();
      Heap.reference_def (Heap.Closure (id, captures)) base);
    let reserved = Reserve.reserve slots (count + 1) base limit () in
    (match reserved with
    | None -> unreachable_ ()
    | Some _ ->
      let written = Memory.correct captures count fragment state frame_base base limit frame_local heap_local () in
      let middle = {state with X.memory = written.Memory.memory} in
      let locals = Advance.correct fragment.Write.bytes heap_local base limit middle () in
      let final = {X.memory = written.Memory.memory; machine = {E.locals; stack = state.X.machine.E.stack}} in
      ghost_ (Machine.allocate_def heap limit (Heap.Closure (id, captures));
        Image.preserve table state.X.memory written.Memory.memory heap base ();
        Hmc_wasm_closure_stored.correct written.Memory.memory base written.Memory.bytes written.Memory.suffix fragment.Write.code captures count ();
        Image.schema_def (Heap.Closure (id, captures)); Image.stored_def written.Memory.memory base (Heap.Closure (id, captures));
        Hmc_heap_wire.corresponds_def (Heap.Closure (id, captures)) (Hmc_heap_wire.Closure (fragment.Write.code, captures));
        Image.related_def written.Memory.memory allocation.A.heap;
        emit_def fragment frame_local heap_local;
        X.append_correct (Write.emit fragment frame_local heap_local) (Advance.emit fragment.Write.bytes heap_local) state);
      {allocation; state = final})
let (exhausted @ total) : (heap : Heap.heap) @ immutable -> (id : D.index) @ immutable -> (captures : Heap.cells) @ immutable ->
    (count : Hmc_wasm_relayout.count) -> (base : B.u32) -> (limit : B.u32) ->
    {u : unit | Heap.used heap = base && Index.represents (Heap.length captures) count && base + 16 + 16 * count > limit} ->
    {u : unit | Machine.allocate heap limit (Heap.Closure (id, captures)) === A.Exhausted} @ ghost =
  fun heap id captures count base limit premise -> ghost_ (
    let slots = D.S (Heap.length captures) in
    Heap.slots_def (Heap.Closure (id, captures)); Index.represents_def slots (count + 1);
    Reserve.fits slots (count + 1) base limit ();
    let _reserved = Hmc_heap_extent.reserve slots base limit in
    Machine.allocate_def heap limit (Heap.Closure (id, captures)))
