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
module Allocate = Hmc_wasm_cons_allocate
module Finish = Wasm_control_branch_finish
module Four = Wasm_four_words
let[@def] (emit @ total) (base_local : B.u32) (limit_local : B.u32) (head_tag : B.u32) (head_payload : B.u32) (tail_tag : B.u32) (tail_payload : B.u32) =
  Select.emit (Four.width ()) base_local limit_local (Lift.embed (Allocate.emit base_local head_tag head_payload tail_tag tail_payload) T.Empty) T.Empty T.Empty
let[@def] (cost @ total) (base : B.u32) (limit : B.u32) (base_local : B.u32) (limit_local : B.u32)
    (head_tag : B.u32) (head_payload : B.u32) (tail_tag : B.u32) (tail_payload : B.u32) =
  Fuel.add (Select.cost (Four.width ()) base_local limit_local)
    (Finish.cost (if base + 32 <= limit then Allocate.emit base_local head_tag head_payload tail_tag tail_payload else C.Empty))
type result = {allocation : A.result; state : X.state}
let (correct @ total) : (table : K.table) @ immutable -> (heap : Heap.heap) @ immutable ->
    (head : V.value) @ immutable -> (tail : V.value) @ immutable ->
    (state : X.state) @ immutable -> (base : B.u32) -> (limit : B.u32) -> (base_local : B.u32) -> (limit_local : B.u32) ->
    (head_tag : B.u32) -> (head_payload : B.u32) -> (tail_tag : B.u32) -> (tail_payload : B.u32) ->
    {u : unit | Heap.valid table heap && Heap.object_valid table (Heap.view heap) (Heap.Cons (head, tail))
      && Heap.used heap = base && base <= limit && Image.related state.X.memory heap && Bounds.covers state.X.memory limit
      && state.X.machine.E.stack === S.Empty
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && L.get state.X.machine.E.locals limit_local === Some (S.I32 limit)
      && L.get state.X.machine.E.locals head_tag === Some (S.I64 (V.tag head))
      && L.get state.X.machine.E.locals head_payload === Some (S.I64 (V.payload head))
      && L.get state.X.machine.E.locals tail_tag === Some (S.I64 (V.tag tail))
      && L.get state.X.machine.E.locals tail_payload === Some (S.I64 (V.payload tail))} ->
    {out : result | Machine.allocate heap limit (Heap.Cons (head, tail)) === out.allocation
      && T.run (cost base limit base_local limit_local head_tag head_payload tail_tag tail_payload)
        {T.code = emit base_local limit_local head_tag head_payload tail_tag tail_payload; labels = T.No_labels; state} === T.Finished out.state
      && (match out.allocation with
        | A.Exhausted -> out.state === state && base + 32 > limit
        | A.Allocated allocation -> A.correct table heap limit (Heap.Cons (head, tail)) out.allocation
          && Image.related out.state.X.memory allocation.A.heap && Bounds.covers out.state.X.memory limit
          && allocation.A.reference === V.Cons_pointer base && Heap.used allocation.A.heap = base + 32
          && L.get out.state.X.machine.E.locals base_local === Some (S.I32 (Heap.used allocation.A.heap))
          && V.length out.state.X.memory === V.length state.X.memory)} @ immutable =
  fun table heap head tail state base limit base_local limit_local head_tag head_payload tail_tag tail_payload premise ->
    ghost_ (Four.width_def ());
    let body = Allocate.emit base_local head_tag head_payload tail_tag tail_payload in
    let success = Lift.embed body T.Empty in
    ghost_ (Select.correct 32 base_local limit_local base limit state T.No_labels success T.Empty T.Empty ();
      emit_def base_local limit_local head_tag head_payload tail_tag tail_payload;
      cost_def base limit base_local limit_local head_tag head_payload tail_tag tail_payload;
      Finish.labels_def ());
    let start = {T.code = emit base_local limit_local head_tag head_payload tail_tag tail_payload; labels = T.No_labels; state} in
    if base + 32 <= limit then (
      let out = Allocate.correct table heap head tail state base limit base_local head_tag head_payload tail_tag tail_payload () in
      ghost_ (Finish.correct body state out.Allocate.state ();
        Fuel.correct (Select.cost 32 base_local limit_local) (Finish.cost body) start);
      {allocation = A.Allocated out.Allocate.allocation; state = out.Allocate.state})
    else (
      ghost_ (Allocate.exhausted heap head tail base limit ();
        X.run_def C.Empty state; Lift.embed_def C.Empty T.Empty;
        Finish.correct C.Empty state state ();
        Fuel.correct (Select.cost 32 base_local limit_local) (Finish.cost C.Empty) start);
      {allocation = A.Exhausted; state})
