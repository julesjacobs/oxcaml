module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module Q = Hmc_heap_state
module Stack = Hmc_memory_stack
module Saved = Hmc_memory_saved_frame
module Capacity = Hmc_memory_stack_capacity
module Extent = Hmc_heap_extent
module C = Wasm_code
module T = Wasm_control
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Exit = Hmc_wasm_allocation_exit
module Continue = Wasm_control_branch_continue
let (fits @ total) : (blocks : G.table) @ immutable -> (width : B.u32) -> (base : B.u32) -> (top : B.u32) -> (limit : B.u32) ->
    (capacity : D.index) @ immutable -> (frames : Q.frames) @ immutable -> (memory : B.bytes) @ immutable ->
    {u : unit | width > 0 && Extent.span (Saved.slots blocks) (Stack.zero ()) width
      && Capacity.region width capacity base limit && Stack.related blocks width memory base top frames} ->
    {u : unit | D.present capacity (Q.depth frames) = (top + width <= limit)} @ ghost =
  fun blocks width base top limit capacity frames memory premise -> ghost_ (
    Capacity.depth blocks width memory base top frames ();
    Capacity.available width capacity (Q.depth frames) base top limit ();
    Capacity.fits_def width top limit)
let (correct @ total) : (blocks : G.table) @ immutable -> (width : B.u32) -> (base : B.u32) -> (top : B.u32) -> (limit : B.u32) ->
    (capacity : D.index) @ immutable -> (frames : Q.frames) @ immutable -> (body : C.t) @ immutable ->
    (top_local : B.u32) -> (limit_local : B.u32) -> (depth : B.u32) -> (tail : T.code) @ immutable -> (outer : T.labels) @ immutable ->
    (before : X.state) @ immutable -> (after : X.state) @ immutable ->
    {u : unit | width > 0 && Extent.span (Saved.slots blocks) (Stack.zero ()) width
      && Capacity.region width capacity base limit && Stack.related blocks width before.X.memory base top frames
      && before.X.machine.E.stack === S.Empty
      && L.get before.X.machine.E.locals top_local === Some (S.I32 top)
      && L.get before.X.machine.E.locals limit_local === Some (S.I32 limit)
      && (if D.present capacity (Q.depth frames) then X.run body before === X.Done after && after.X.machine.E.stack === S.Empty else true)} ->
    {u : unit | T.run (Exit.cost width top_local limit_local body top limit)
        {T.code = Exit.emit width top_local limit_local body depth tail; labels = outer; state = before} ===
      (if D.present capacity (Q.depth frames) then T.Running {T.code = tail; labels = outer; state = after}
       else T.branch depth (Continue.labels tail outer) before)} @ ghost =
  fun blocks width base top limit capacity frames body top_local limit_local depth tail outer before after premise -> ghost_ (
    fits blocks width base top limit capacity frames before.X.memory ();
    Exit.correct width top_local limit_local body depth tail outer top limit before after ())
