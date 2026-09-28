module D = Hm_declarative
module W = Hmc_word64
module G = Hmc_cfg_ir
module Program = Hmc_tail_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Frame = Hmc_heap_frame
module State = Hmc_heap_state
module Machine = Hmc_heap_machine
let (empty @ total) : (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (heap_limit : W.limb) -> (stack_limit : D.index) @ immutable -> (heap : Heap.heap) @ immutable ->
    (activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (empty : D.index) @ immutable -> (full : D.index) @ immutable ->
    {u : unit | Program.lookup program.Program.code activation.Frame.pc === Some (Program.Keep (G.List_branch (empty, full)))
      && activation.Frame.accumulator === V.Nil} ->
    {u : unit | Machine.step program globals heap_limit stack_limit {Machine.heap; state = State.Running (activation, frames)} ===
      Machine.Advanced {Machine.heap; state = State.Running ({activation with Frame.pc = empty}, frames)}} @ ghost =
  fun program globals heap_limit stack_limit heap activation frames empty full premise -> ghost_ (
    Machine.step_def program globals heap_limit stack_limit {Machine.heap; state = State.Running (activation, frames)})
let (full @ total) : (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (heap_limit : W.limb) -> (stack_limit : D.index) @ immutable -> (heap : Heap.heap) @ immutable ->
    (activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (empty : D.index) @ immutable -> (full : D.index) @ immutable -> (address : W.limb) ->
    (head : V.value) @ immutable -> (tail : V.value) @ immutable ->
    {u : unit | Program.lookup program.Program.code activation.Frame.pc === Some (Program.Keep (G.List_branch (empty, full)))
      && activation.Frame.accumulator === V.Cons_pointer address
      && Hmc_heap_preservation.lookup_object heap address === Some (Heap.Cons (head, tail))} ->
    {u : unit | Machine.step program globals heap_limit stack_limit {Machine.heap; state = State.Running (activation, frames)} ===
      Machine.Advanced {Machine.heap; state = State.Running ({activation with Frame.pc = full;
        env = Heap.Cell (head, Heap.Cell (tail, activation.Frame.env));
        temporaries = Frame.Environment (activation.Frame.env, activation.Frame.temporaries)}, frames)}} @ ghost =
  fun program globals heap_limit stack_limit heap activation frames empty full address head tail premise -> ghost_ (
    Machine.step_def program globals heap_limit stack_limit {Machine.heap; state = State.Running (activation, frames)})
