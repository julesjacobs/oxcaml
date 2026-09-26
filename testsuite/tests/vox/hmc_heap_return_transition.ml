module D = Hm_declarative
module W = Hmc_word64
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module Machine = Hmc_heap_machine
let (caller @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable -> (heap : H.heap) @ immutable ->
    (heap_limit : W.limb) -> (stack_limit : D.index) @ immutable -> (activation : F.activation) @ immutable ->
    (saved : F.activation) @ immutable -> (frames : Q.frames) @ immutable ->
    {u : unit | I.lookup program.I.code activation.F.pc === Some (I.Keep G.Return) && activation.F.temporaries === F.Empty} ->
    {u : unit | Machine.step program globals heap_limit stack_limit {Machine.heap; state = Q.Running (activation, Q.Frame (saved, frames))} ===
      Machine.Advanced {Machine.heap; state = Q.Running ({saved with F.accumulator = activation.F.accumulator}, frames)}} @ ghost =
  fun program globals heap heap_limit stack_limit activation saved frames premise -> ghost_ (
    Machine.step_def program globals heap_limit stack_limit {Machine.heap; state = Q.Running (activation, Q.Frame (saved, frames))};
    Hmc_heap_simple.step_def G.Return (Q.Running (activation, Q.Frame (saved, frames))))
let (root @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable -> (heap : H.heap) @ immutable ->
    (heap_limit : W.limb) -> (stack_limit : D.index) @ immutable -> (activation : F.activation) @ immutable ->
    {u : unit | I.lookup program.I.code activation.F.pc === Some (I.Keep G.Return) && activation.F.temporaries === F.Empty} ->
    {u : unit | Machine.step program globals heap_limit stack_limit {Machine.heap; state = Q.Running (activation, Q.Halt)} ===
      Machine.Advanced {Machine.heap; state = Q.Done activation.F.accumulator}} @ ghost =
  fun program globals heap heap_limit stack_limit activation premise -> ghost_ (
    Machine.step_def program globals heap_limit stack_limit {Machine.heap; state = Q.Running (activation, Q.Halt)};
    Hmc_heap_simple.step_def G.Return (Q.Running (activation, Q.Halt)))
