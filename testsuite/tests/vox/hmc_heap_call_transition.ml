module D = Hm_declarative
module W = Hmc_word64
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module K = Hmc_closure_ir
module C = Hmc_cfg_program
module Machine = Hmc_heap_machine
let (invoke @ total) : (program : I.program) @ immutable -> (heap : Heap.heap) @ immutable -> (address : W.limb) ->
    (id : D.index) @ immutable -> (captures : Heap.cells) @ immutable -> (entry : K.entry) @ immutable -> (code : C.function_entry) @ immutable -> (argument : V.value) @ immutable ->
    {u : unit | Hmc_heap_preservation.lookup_object heap address === Some (Heap.Closure (id, captures))
      && K.lookup program.I.origin.C.origin.Hmc_closure_program.table id === Some entry
      && C.lookup program.I.origin.C.functions id === Some code} ->
    {u : unit | Machine.invoke program heap (V.Closure_pointer address) argument ===
      Some {F.pc = code.C.start; env = Heap.Cell (argument, (if entry.K.recursive then Heap.Cell (V.Closure_pointer address, captures) else captures));
        accumulator = V.Nil; temporaries = F.Empty; current = V.Closure_pointer address}} @ ghost =
  fun program heap address id captures entry code argument premise -> ghost_ (Machine.invoke_def program heap (V.Closure_pointer address) argument)
let (success @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable -> (heap : Heap.heap) @ immutable ->
    (heap_limit : W.limb) -> (stack_limit : D.index) @ immutable -> (activation : F.activation) @ immutable -> (frames : Q.frames) @ immutable ->
    (next : D.index) @ immutable -> (closure : V.value) @ immutable -> (env : Heap.cells) @ immutable -> (rest : F.temporaries) @ immutable -> (entered : F.activation) @ immutable ->
    {u : unit | I.lookup program.I.code activation.F.pc === Some (I.Keep (G.Call next))
      && activation.F.temporaries === F.Value (closure, env, rest)
      && Machine.invoke program heap closure activation.F.accumulator === Some entered
      && D.present stack_limit (Q.depth frames)} ->
    {u : unit | Machine.step program globals heap_limit stack_limit {Machine.heap; state = Q.Running (activation, frames)} ===
      Machine.Advanced {Machine.heap; state = Q.Running (entered, Q.Frame ({activation with F.pc = next; env; temporaries = rest}, frames))}} @ ghost =
  fun program globals heap heap_limit stack_limit activation frames next closure env rest entered premise ->
    ghost_ (Machine.step_def program globals heap_limit stack_limit {Machine.heap; state = Q.Running (activation, frames)})
let (exhausted @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable -> (heap : Heap.heap) @ immutable ->
    (heap_limit : W.limb) -> (stack_limit : D.index) @ immutable -> (activation : F.activation) @ immutable -> (frames : Q.frames) @ immutable ->
    (next : D.index) @ immutable -> (closure : V.value) @ immutable -> (env : Heap.cells) @ immutable -> (rest : F.temporaries) @ immutable -> (entered : F.activation) @ immutable ->
    {u : unit | I.lookup program.I.code activation.F.pc === Some (I.Keep (G.Call next))
      && activation.F.temporaries === F.Value (closure, env, rest)
      && Machine.invoke program heap closure activation.F.accumulator === Some entered
      && not (D.present stack_limit (Q.depth frames))} ->
    {u : unit | Machine.step program globals heap_limit stack_limit {Machine.heap; state = Q.Running (activation, frames)} === Machine.Exhausted Machine.Stack} @ ghost =
  fun program globals heap heap_limit stack_limit activation frames next closure env rest entered premise ->
    ghost_ (Machine.step_def program globals heap_limit stack_limit {Machine.heap; state = Q.Running (activation, frames)})
let (tail @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable -> (heap : Heap.heap) @ immutable ->
    (heap_limit : W.limb) -> (stack_limit : D.index) @ immutable -> (activation : F.activation) @ immutable -> (frames : Q.frames) @ immutable ->
    (closure : V.value) @ immutable -> (env : Heap.cells) @ immutable -> (rest : F.temporaries) @ immutable -> (entered : F.activation) @ immutable ->
    {u : unit | I.lookup program.I.code activation.F.pc === Some I.Tail_call
      && activation.F.temporaries === F.Value (closure, env, rest)
      && Machine.invoke program heap closure activation.F.accumulator === Some entered} ->
    {u : unit | Machine.step program globals heap_limit stack_limit {Machine.heap; state = Q.Running (activation, frames)} ===
      Machine.Advanced {Machine.heap; state = Q.Running (entered, frames)}} @ ghost =
  fun program globals heap heap_limit stack_limit activation frames closure env rest entered premise ->
    ghost_ (Machine.step_def program globals heap_limit stack_limit {Machine.heap; state = Q.Running (activation, frames)})
