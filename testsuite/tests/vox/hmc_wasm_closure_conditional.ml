module B = Wasm_u32
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module K = Hmc_closure_ir
module A = Hmc_heap_allocate
module Machine = Hmc_heap_machine
module Image = Hmc_heap_image
module Bounds = Hmc_linear_bounds
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module C = Wasm_code
module T = Wasm_control
module Lift = Wasm_control_lift
module Fuel = Wasm_control_compose
module Select = Hmc_wasm_allocation_select
module Allocate = Hmc_wasm_closure_allocate
module Finish = Wasm_control_branch_finish
module D = Hm_declarative
module Write = Hmc_wasm_closure_write
module Index = Hmc_u32_index
module Range = Hmc_wasm_range_copy
module Memory = Hmc_wasm_closure_memory
module Plan = Wasm_parallel_copy
let[@def] (emit @ total) (fragment : Write.fragment @ immutable) (frame_local : B.u32) (base_local : B.u32) (limit_local : B.u32) =
  Select.emit fragment.Write.bytes base_local limit_local (Lift.embed (Allocate.emit fragment frame_local base_local) T.Empty) T.Empty T.Empty
let[@def] (cost @ total) (base : B.u32) (limit : B.u32) (fragment : Write.fragment @ immutable) (frame_local : B.u32) (base_local : B.u32) (limit_local : B.u32) =
  Fuel.add (Select.cost fragment.Write.bytes base_local limit_local)
    (Finish.cost (if base + fragment.Write.bytes <= limit then Allocate.emit fragment frame_local base_local else C.Empty))
type result = {allocation : A.result; state : X.state}
let (correct @ total) : (table : K.table) @ immutable -> (heap : Heap.heap) @ immutable ->
    (id : D.index) @ immutable -> (captures : Heap.cells) @ immutable ->
    (count : Hmc_wasm_relayout.count) -> (fragment : Write.fragment) @ immutable -> (frame_base : B.u32) -> (frame_local : B.u32) ->
    (state : X.state) @ immutable -> (base : B.u32) -> (limit : B.u32) -> (base_local : B.u32) -> (limit_local : B.u32) ->
    {u : unit | Heap.valid table heap && Heap.object_valid table (Heap.view heap) (Heap.Closure (id, captures))
      && Heap.used heap = base && base <= limit && Image.related state.X.memory heap && Bounds.covers state.X.memory limit
      && state.X.machine.E.stack === S.Empty
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && L.get state.X.machine.E.locals limit_local === Some (S.I32 limit)
      && Index.represents id fragment.Write.code && Index.represents (Heap.length captures) count && 2 + count <= 268435452
      && fragment.Write.bytes = 16 + 16 * count
      && Hmc_wasm_relayout.range_is fragment.Write.copies 2 0 (Heap.length captures) Plan.End
      && Range.reads state.X.memory frame_base (Memory.environment_position ()) captures
      && L.get state.X.machine.E.locals frame_local === Some (S.I32 frame_base)} ->
    {out : result | Machine.allocate heap limit (Heap.Closure (id, captures)) === out.allocation
      && T.run (cost base limit fragment frame_local base_local limit_local)
        {T.code = emit fragment frame_local base_local limit_local; labels = T.No_labels; state} === T.Finished out.state
      && (match out.allocation with
        | A.Exhausted -> out.state === state && base + fragment.Write.bytes > limit
        | A.Allocated allocation -> A.correct table heap limit (Heap.Closure (id, captures)) out.allocation
          && Image.related out.state.X.memory allocation.A.heap && Bounds.covers out.state.X.memory limit
          && allocation.A.reference === V.Closure_pointer base && Heap.used allocation.A.heap = base + fragment.Write.bytes
          && L.get out.state.X.machine.E.locals base_local === Some (S.I32 (Heap.used allocation.A.heap))
          && V.length out.state.X.memory === V.length state.X.memory)} @ immutable =
  fun table heap id captures count fragment frame_base frame_local state base limit base_local limit_local premise ->
    let body = Allocate.emit fragment frame_local base_local in
    let success = Lift.embed body T.Empty in
    ghost_ (Select.correct fragment.Write.bytes base_local limit_local base limit state T.No_labels success T.Empty T.Empty ();
      emit_def fragment frame_local base_local limit_local;
      cost_def base limit fragment frame_local base_local limit_local;
      Finish.labels_def ());
    let start = {T.code = emit fragment frame_local base_local limit_local; labels = T.No_labels; state} in
    if base + fragment.Write.bytes <= limit then (
      let out = Allocate.correct table heap id captures count fragment state frame_base base limit frame_local base_local () in
      ghost_ (Finish.correct body state out.Allocate.state ();
        Fuel.correct (Select.cost fragment.Write.bytes base_local limit_local) (Finish.cost body) start);
      {allocation = A.Allocated out.Allocate.allocation; state = out.Allocate.state})
    else (
      ghost_ (Allocate.exhausted heap id captures count base limit ();
        X.run_def C.Empty state; Lift.embed_def C.Empty T.Empty;
        Finish.correct C.Empty state state ();
        Fuel.correct (Select.cost fragment.Write.bytes base_local limit_local) (Finish.cost C.Empty) start);
      {allocation = A.Exhausted; state})
