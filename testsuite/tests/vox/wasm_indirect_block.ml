module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module GE = Wasm_global_execution
module T = Wasm_control
module P = Wasm_instance_control
module F = Wasm_functions
module M = Wasm_calls
module Body = Wasm_calls_body
module Fuel = Wasm_control_compose
let[@def] (entry @ total) (function_ : F.function_ @ immutable) (before : P.configuration @ immutable) =
  {P.globals = before.P.globals; body = {T.code = function_.F.code; labels = T.No_labels;
    state = {X.memory = before.P.body.T.state.X.memory; machine = {E.locals = F.zero_locals function_.F.locals; stack = S.Empty}}}}
let[@def] (cost @ total) (body_fuel : C.count @ immutable) = C.Succ (Fuel.add body_fuel (C.Succ C.Zero))
let (correct @ total) : (module_ : F.module_) @ immutable -> (signature : B.u32) -> (slot : B.u32) -> (index : B.u32) ->
    (function_ : F.function_) @ immutable -> (tail : T.code) @ immutable -> (before : M.configuration) @ immutable ->
    (stack : S.stack) @ immutable -> (capacity : C.count) @ immutable -> (after : GE.state) @ immutable -> (body_fuel : C.count) @ immutable ->
    {u : unit | before.M.current.P.body.T.code === T.Instruction (I.Call_indirect signature, tail)
      && before.M.current.P.body.T.state.X.machine.E.stack === S.Push (S.I32 slot, stack)
      && before.M.capacity === C.Succ capacity && F.signature module_.F.signatures signature === Some F.Void
      && F.element module_.F.table slot === Some index && F.lookup module_.F.functions index === Some function_
      && function_.F.result === F.Void && after.GE.execution.X.machine.E.stack === S.Empty
      && P.run body_fuel (entry function_ before.M.current) ===
        P.Running {P.body = {T.code = T.Empty; labels = T.No_labels; state = after.GE.execution}; globals = after.GE.globals}} ->
    {u : unit | M.run (cost body_fuel) module_ before === M.Running
      {before with M.current = {P.globals = after.GE.globals;
        body = {T.code = tail; labels = before.M.current.P.body.T.labels;
          state = {X.memory = after.GE.execution.X.memory;
            machine = {E.locals = before.M.current.P.body.T.state.X.machine.E.locals; stack}}}}}} @ ghost =
  fun module_ signature slot index function_ tail before stack capacity after body_fuel premise -> ghost_ (
    entry_def function_ before.M.current;
    let ready = {before with M.current = M.with_stack before.M.current stack} in
    M.with_stack_def before.M.current stack; T.stack_def before.M.current.P.body.T.state stack;
    let caller = {M.code = tail; labels = before.M.current.P.body.T.labels;
      locals = before.M.current.P.body.T.state.X.machine.E.locals; stack; result = before.M.result} in
    let entered = {M.current = entry function_ before.M.current; result = F.Void;
      callers = M.Caller (caller, before.M.callers); capacity} in
    let finished = {P.body = {T.code = T.Empty; labels = T.No_labels; state = after.GE.execution}; globals = after.GE.globals} in
    let leaving = {entered with M.current = finished} in
    M.step_def module_ before; F.same_result_def F.Void F.Void; M.enter_def function_ tail ready;
    Body.run body_fuel module_ entered finished ();
    M.run_def (C.Succ C.Zero) module_ leaving; M.step_def module_ leaving;
    M.advance_def leaving; P.step_def finished; T.step_def finished.P.body;
    F.complete_def F.Void after.GE.execution.X.machine.E.stack;
    M.leave_def leaving; F.take_result_def F.Void after.GE.execution.X.machine.E.stack;
    F.deliver_def None stack;
    M.run_def C.Zero module_ {before with M.current = {P.globals = after.GE.globals;
      body = {T.code = tail; labels = before.M.current.P.body.T.labels;
        state = {X.memory = after.GE.execution.X.memory; machine = {E.locals = before.M.current.P.body.T.state.X.machine.E.locals; stack}}}}};
    Body.compose body_fuel (C.Succ C.Zero) module_ entered;
    cost_def body_fuel; M.run_def (cost body_fuel) module_ before)
