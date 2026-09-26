module D = Hm_declarative
module W = Hmc_word64
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module Heap = Hmc_heap_objects
module A = Hmc_heap_allocate
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module Machine = Hmc_heap_machine
let (success @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (heap : Heap.heap) @ immutable -> (limit : W.limb) -> (stack_limit : D.index) @ immutable ->
    (activation : F.activation) @ immutable -> (frames : Q.frames) @ immutable ->
    (id : D.index) @ immutable -> (type_ : D.mono) @ immutable -> (typing : D.typing) @ immutable -> (next : D.index) @ immutable ->
    (allocation : A.allocation) @ immutable ->
    {u : unit | I.lookup program.I.code activation.F.pc === Some (I.Keep (G.Load (G.Closure id, type_, typing, next)))
      && Machine.allocate heap limit (Heap.Closure (id, activation.F.env)) === A.Allocated allocation} ->
    {u : unit | Machine.step program globals limit stack_limit {Machine.heap; state = Q.Running (activation, frames)} ===
      Machine.Advanced {Machine.heap = allocation.A.heap;
        state = Q.Running ({activation with F.pc = next; accumulator = allocation.A.reference}, frames)}} @ ghost =
  fun program globals heap limit stack_limit activation frames id type_ typing next allocation premise -> ghost_ (
    Machine.step_def program globals limit stack_limit {Machine.heap; state = Q.Running (activation, frames)};
    Machine.allocation_result_def activation frames next activation.F.env activation.F.temporaries (A.Allocated allocation))
let (exhausted @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (heap : Heap.heap) @ immutable -> (limit : W.limb) -> (stack_limit : D.index) @ immutable ->
    (activation : F.activation) @ immutable -> (frames : Q.frames) @ immutable ->
    (id : D.index) @ immutable -> (type_ : D.mono) @ immutable -> (typing : D.typing) @ immutable -> (next : D.index) @ immutable ->
    {u : unit | I.lookup program.I.code activation.F.pc === Some (I.Keep (G.Load (G.Closure id, type_, typing, next)))
      && Machine.allocate heap limit (Heap.Closure (id, activation.F.env)) === A.Exhausted} ->
    {u : unit | Machine.step program globals limit stack_limit {Machine.heap; state = Q.Running (activation, frames)} === Machine.Exhausted Machine.Heap} @ ghost =
  fun program globals heap limit stack_limit activation frames id type_ typing next premise -> ghost_ (
    Machine.step_def program globals limit stack_limit {Machine.heap; state = Q.Running (activation, frames)};
    Machine.allocation_result_def activation frames next activation.F.env activation.F.temporaries A.Exhausted)
