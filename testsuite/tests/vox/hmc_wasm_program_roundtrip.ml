module B = Wasm_u32
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module T = Wasm_control
module P = Wasm_instance_control
module GE = Wasm_global_execution
module F = Wasm_functions
module M = Wasm_calls
module R = Wasm_global_registers
module W = Wasm_register_block
module Continue = Wasm_control_branch_continue
module Indirect = Wasm_indirect_block
module Assembly = Hmc_wasm_program_functions
module Emit = Hmc_wasm_program_emit
module Lower = Hmc_wasm_program_lower
module Block = Hmc_wasm_program_block
let[@def] (labels @ total) (config : Assembly.config @ immutable) = Continue.labels (W.epilogue config.Assembly.stores T.Empty) T.No_labels
let[@def] (cost @ total) (config : Assembly.config @ immutable) (body : C.count @ immutable) =
  Indirect.cost (W.cost config.Assembly.loads body config.Assembly.stores)
let (store_preserves @ total) : (plan : R.plan) @ immutable -> (before : GE.state) @ immutable -> (after : GE.state) @ immutable ->
    {u : unit | GE.run (R.store_code plan) before === GE.Done after} ->
    {u : unit | after.GE.execution === before.GE.execution} @ ghost = fun plan before after premise -> ghost_ (
  match R.store plan before with None -> () | Some _ -> ())
let (correct @ total) : (program : Lower.program) @ immutable -> (fragment : Block.fragment) @ immutable -> (config : Assembly.config) @ immutable ->
    (module_ : F.module_) @ immutable -> (call : M.configuration) @ immutable -> (signature : B.u32) -> (slot : B.u32) -> (index : B.u32) ->
    (tail : T.code) @ immutable -> (stack : S.stack) @ immutable -> (capacity : C.count) @ immutable ->
    (imported : X.state) @ immutable -> (after : X.state) @ immutable -> (exported : GE.state) @ immutable -> (body_fuel : C.count) @ immutable ->
    {u : unit | call.M.current.P.body.T.code === T.Instruction (Wasm_instruction.Call_indirect signature, tail)
      && call.M.current.P.body.T.state.X.machine.E.stack === S.Push (S.I32 slot, stack) && call.M.capacity === C.Succ capacity
      && F.signature module_.F.signatures signature === Some F.Void && F.element module_.F.table slot === Some index
      && F.lookup module_.F.functions index === Some (Assembly.function_ program fragment config)
      && GE.run (R.load_code config.Assembly.loads)
        {GE.globals = call.M.current.P.globals; execution = {X.memory = call.M.current.P.body.T.state.X.memory;
          machine = {E.locals = F.zero_locals config.Assembly.local_types; stack = S.Empty}}}
        === GE.Done {GE.globals = call.M.current.P.globals; execution = imported}
      && imported.X.machine.E.stack === S.Empty && after.X.machine.E.stack === S.Empty
      && T.run body_fuel {T.code = Emit.emit program fragment config.Assembly.locals config.Assembly.table_base config.Assembly.stack_base;
        labels = labels config; state = imported} === T.Running {T.code = T.Empty; labels = labels config; state = after}
      && GE.run (R.store_code config.Assembly.stores) {GE.globals = call.M.current.P.globals; execution = after} === GE.Done exported} ->
    {u : unit | M.run (cost config body_fuel) module_ call === M.Running
      {call with M.current = {P.globals = exported.GE.globals;
        body = {T.code = tail; labels = call.M.current.P.body.T.labels;
          state = {X.memory = exported.GE.execution.X.memory;
            machine = {E.locals = call.M.current.P.body.T.state.X.machine.E.locals; stack}}}}}} @ ghost =
  fun program fragment config module_ call signature slot index tail stack capacity imported after exported body_fuel premise -> ghost_ (
    let function_ = Assembly.function_ program fragment config in
    let body = Emit.emit program fragment config.Assembly.locals config.Assembly.table_base config.Assembly.stack_base in
    let initial = {GE.globals = call.M.current.P.globals; execution = {X.memory = call.M.current.P.body.T.state.X.memory;
      machine = {E.locals = F.zero_locals config.Assembly.local_types; stack = S.Empty}}} in
    labels_def config; Assembly.function__def program fragment config;
    W.correct config.Assembly.loads body config.Assembly.stores T.Empty T.No_labels initial
      {GE.globals = call.M.current.P.globals; execution = imported} after exported body_fuel ();
    store_preserves config.Assembly.stores {GE.globals = call.M.current.P.globals; execution = after} exported ();
    Indirect.entry_def function_ call.M.current;
    Indirect.correct module_ signature slot index function_ tail call stack capacity exported
      (W.cost config.Assembly.loads body_fuel config.Assembly.stores) ();
    cost_def config body_fuel)
