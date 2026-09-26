module D = Hm_declarative
module W = Hmc_word64
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module A = Hmc_heap_allocate
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module Machine = Hmc_heap_machine
module Pop = Hmc_frame_value_pop
let (success @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (heap : Heap.heap) @ immutable -> (limit : W.limb) -> (stack_limit : D.index) @ immutable ->
    (activation : F.activation) @ immutable -> (frames : Q.frames) @ immutable ->
    (next : D.index) @ immutable -> (head : V.value) @ immutable -> (allocation : A.allocation) @ immutable ->
    (after : F.activation) @ immutable ->
    {u : unit | I.lookup program.I.code activation.F.pc === Some (I.Keep (G.Cons next))
      && (match activation.F.temporaries with F.Value (value, _, _) -> value === head | _ -> false)
      && Machine.allocate heap limit (Heap.Cons (head, activation.F.accumulator)) === A.Allocated allocation
      && Pop.transition activation next allocation.A.reference === Some after} ->
    {u : unit | Machine.step program globals limit stack_limit {Machine.heap; state = Q.Running (activation, frames)} ===
      Machine.Advanced {Machine.heap = allocation.A.heap; state = Q.Running (after, frames)}} @ ghost =
  fun program globals heap limit stack_limit activation frames next head allocation after premise -> ghost_ (
    Pop.transition_def activation next allocation.A.reference;
    Machine.step_def program globals limit stack_limit {Machine.heap; state = Q.Running (activation, frames)};
    match activation.F.temporaries with
    | F.Value (_, env, rest) -> Machine.allocation_result_def activation frames next env rest (A.Allocated allocation)
    | _ -> ())
let (exhausted @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (heap : Heap.heap) @ immutable -> (limit : W.limb) -> (stack_limit : D.index) @ immutable ->
    (activation : F.activation) @ immutable -> (frames : Q.frames) @ immutable ->
    (next : D.index) @ immutable -> (head : V.value) @ immutable ->
    {u : unit | I.lookup program.I.code activation.F.pc === Some (I.Keep (G.Cons next))
      && (match activation.F.temporaries with F.Value (value, _, _) -> value === head | _ -> false)
      && Machine.allocate heap limit (Heap.Cons (head, activation.F.accumulator)) === A.Exhausted} ->
    {u : unit | Machine.step program globals limit stack_limit {Machine.heap; state = Q.Running (activation, frames)} ===
      Machine.Exhausted Machine.Heap} @ ghost =
  fun program globals heap limit stack_limit activation frames next head premise -> ghost_ (
    Machine.step_def program globals limit stack_limit {Machine.heap; state = Q.Running (activation, frames)};
    match activation.F.temporaries with
    | F.Value (_, env, rest) -> Machine.allocation_result_def activation frames next env rest A.Exhausted
    | _ -> ())
