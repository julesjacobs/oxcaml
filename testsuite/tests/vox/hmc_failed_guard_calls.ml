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
module Calls = Wasm_calls
module Body = Wasm_calls_body
module Fuel = Wasm_control_compose
module Local = Hmc_failed_guard_reach
module Model = Hmc_failed_guard_model
module Registers = Wasm_global_registers
module Register = Wasm_register_block
module Continue = Wasm_control_branch_continue
module Indirect = Wasm_indirect_block

type witness = {prefix : C.count; before : Calls.configuration}
type optional = Absent | Present of witness [@@inductive]
let[@def] (reaches @ total) (resource : Model.resource @ immutable) (module_ : F.module_ @ immutable)
    (start : Calls.configuration @ immutable) (witness : witness @ immutable) = ghost_ (
  Calls.run witness.prefix module_ start === Calls.Running witness.before
  && Model.failed resource witness.before.Calls.current.P.body)

let (prepend @ total) : (resource : Model.resource) @ immutable -> (module_ : F.module_) @ immutable ->
    (first : C.count) @ immutable -> (start : Calls.configuration) @ immutable ->
    (middle : Calls.configuration) @ immutable -> (witness : witness) @ immutable ->
    {u : unit | Calls.run first module_ start === Calls.Running middle && reaches resource module_ middle witness} ->
    {out : witness | reaches resource module_ start out} @ immutable ghost =
  fun resource module_ first start middle witness premise -> ghost_ (
    reaches_def resource module_ middle witness; Body.compose first witness.prefix module_ start;
    let out = {prefix = Fuel.add first witness.prefix; before = witness.before} in
    reaches_def resource module_ start out; out)

let (register_prefix @ total) : (resource : Model.resource) @ immutable ->
    (loads : Registers.plan) @ immutable -> (code : T.code) @ immutable -> (stores : Registers.plan) @ immutable ->
    (before : GE.state) @ immutable -> (imported : GE.state) @ immutable -> (guard : Local.witness) @ immutable ->
    {u : unit | GE.run (Registers.load_code loads) before === GE.Done imported
      && imported.GE.execution.X.machine.E.stack === S.Empty
      && Local.reaches resource {T.code = code;
        labels = Continue.labels (Register.epilogue stores T.Empty) T.No_labels; state = imported.GE.execution} guard} ->
    {u : unit | P.run (Fuel.add (C.length (Registers.load_code loads)) (C.Succ guard.Local.prefix))
        {P.body = {T.code = Register.emit loads code stores T.Empty; labels = T.No_labels; state = before.GE.execution}; globals = before.GE.globals}
      === P.Running {P.body = guard.Local.before; globals = imported.GE.globals}} @ ghost =
  fun resource loads code stores before imported guard premise -> ghost_ (
    let finish = Register.epilogue stores T.Empty in
    let scoped = Continue.labels finish T.No_labels in
    let start = {P.body = {T.code = Register.emit loads code stores T.Empty; labels = T.No_labels; state = before.GE.execution}; globals = before.GE.globals} in
    let block = {P.body = {T.code = T.Block (code, finish); labels = T.No_labels; state = imported.GE.execution}; globals = imported.GE.globals} in
    let body = {P.body = {T.code = code; labels = scoped; state = imported.GE.execution}; globals = imported.GE.globals} in
    Local.reaches_def resource body.P.body guard;
    Registers.load_straight loads; Register.emit_def loads code stores T.Empty;
    P.straight_line (Registers.load_code loads) (T.Block (code, finish)) T.No_labels before imported ();
    P.step_def block; T.step_def block.P.body; T.enter_def code finish None block.P.body;
    Continue.labels_def finish T.No_labels; T.stack_def imported.GE.execution S.Empty;
    Wasm_instance_body.run guard.Local.prefix body.P.body guard.Local.before imported.GE.globals ();
    P.run_def (C.Succ guard.Local.prefix) block;
    Wasm_instance_body.compose (C.length (Registers.load_code loads)) (C.Succ guard.Local.prefix) start)

let (indirect_prefix @ total) : (resource : Model.resource) @ immutable -> (module_ : F.module_) @ immutable ->
    (signature : B.u32) -> (slot : B.u32) -> (index : B.u32) -> (function_ : F.function_) @ immutable ->
    (tail : T.code) @ immutable -> (before : Calls.configuration) @ immutable -> (stack : S.stack) @ immutable ->
    (capacity : C.count) @ immutable -> (fuel : C.count) @ immutable -> (guard : P.configuration) @ immutable ->
    {u : unit | before.Calls.current.P.body.T.code === T.Instruction (I.Call_indirect signature, tail)
      && before.Calls.current.P.body.T.state.X.machine.E.stack === S.Push (S.I32 slot, stack)
      && before.Calls.capacity === C.Succ capacity && F.signature module_.F.signatures signature === Some F.Void
      && F.element module_.F.table slot === Some index && F.lookup module_.F.functions index === Some function_
      && function_.F.result === F.Void
      && P.run fuel (Indirect.entry function_ before.Calls.current) === P.Running guard
      && Model.failed resource guard.P.body} ->
    {out : witness | reaches resource module_ before out} @ immutable ghost =
  fun resource module_ signature slot index function_ tail before stack capacity fuel guard premise -> ghost_ (
    Indirect.entry_def function_ before.Calls.current;
    let ready = {before with Calls.current = Calls.with_stack before.Calls.current stack} in
    Calls.with_stack_def before.Calls.current stack; T.stack_def before.Calls.current.P.body.T.state stack;
    let caller = {Calls.code = tail; labels = before.Calls.current.P.body.T.labels;
      locals = before.Calls.current.P.body.T.state.X.machine.E.locals; stack; result = before.Calls.result} in
    let entered = {Calls.current = Indirect.entry function_ before.Calls.current; result = F.Void;
      callers = Calls.Caller (caller, before.Calls.callers); capacity} in
    Calls.step_def module_ before; F.same_result_def F.Void F.Void; Calls.enter_def function_ tail ready;
    Body.run fuel module_ entered guard ();
    Calls.run_def (C.Succ fuel) module_ before;
    let out = {prefix = C.Succ fuel; before = {entered with Calls.current = guard}} in
    reaches_def resource module_ before out; out)

module Lower = Hmc_wasm_program_lower
module Block = Hmc_wasm_program_block
module Emit = Hmc_wasm_program_emit
module Assembly = Hmc_wasm_program_functions
module Runtime = Hmc_wasm_program_runtime
module Values = Hmc_wasm_program_registers
module Round = Hmc_wasm_program_roundtrip
module Dispatch = Hmc_wasm_program_dispatch
module Step = Hmc_wasm_program_register_step
module Heap = Hmc_heap_objects
module D = Hm_declarative
module Cell = Hmc_tagged_cell
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Bytes = Hmc_linear_bytes

let (from_body @ total) : (resource : Model.resource) @ immutable -> (program : Lower.program) @ immutable ->
    (fragment : Block.fragment) @ immutable -> (module_ : F.module_) @ immutable ->
    (table_base : B.u32) -> (stack_base : B.u32) -> (before : Values.registers) @ immutable ->
    (memory : B.bytes) @ immutable -> (bytes : B.bytes) @ immutable -> (cells : Heap.cells) @ immutable ->
    (suffix : B.bytes) @ immutable -> (capacity : C.count) @ immutable -> (pc : B.u32) -> (index : B.u32) ->
    (guard : Local.witness) @ immutable ->
    {u : unit | before.Values.frame <= 4294967280 && Bytes.drop memory before.Values.frame === Some bytes
      && Wire.decode_cells (D.S (Heap.length cells)) bytes === Some (Heap.Cell (Cell.Word (Header.number pc), cells), suffix)
      && F.signature module_.F.signatures (Dispatch.void_signature ()) === Some F.Void
      && F.element module_.F.table pc === Some index
      && F.lookup module_.F.functions index === Some (Assembly.function_ program fragment (Runtime.config table_base stack_base))
      && Local.reaches resource
        {T.code = Emit.emit program fragment (Runtime.config table_base stack_base).Assembly.locals table_base stack_base;
          labels = Round.labels (Runtime.config table_base stack_base);
          state = {X.memory; machine = {E.locals = Values.locals before; stack = S.Empty}}} guard} ->
    {out : witness | reaches resource module_
      (Dispatch.loop (Values.globals before) memory (C.Succ capacity)) out} @ immutable ghost =
  fun resource program fragment module_ table_base stack_base before memory bytes cells suffix capacity pc index guard premise -> ghost_ (
    let config = Runtime.config table_base stack_base in
    let globals = Values.globals before in
    let function_ = Assembly.function_ program fragment config in
    let code = Emit.emit program fragment config.Assembly.locals table_base stack_base in
    let call = Dispatch.call globals memory pc (C.Succ capacity) in
    Hmc_wasm_program_header.correct memory before.Values.frame bytes pc cells suffix ();
    Step.controls before; Dispatch.pc_offset_def ();
    Dispatch.header module_ globals memory (C.Succ capacity) before.Values.frame pc ();
    Values.globals_def before; Runtime.config_def table_base stack_base;
    let imported = Values.import before globals memory table_base stack_base () in
    let initial = {GE.globals; execution = {X.memory;
      machine = {E.locals = F.zero_locals config.Assembly.local_types; stack = S.Empty}}} in
    Round.labels_def config;
    register_prefix resource config.Assembly.loads code config.Assembly.stores initial
      {GE.globals; execution = imported} guard ();
    Local.reaches_def resource {T.code = code; labels = Round.labels config; state = imported} guard;
    let fuel = Fuel.add (C.length (Registers.load_code config.Assembly.loads)) (C.Succ guard.Local.prefix) in
    let guarded = {P.body = guard.Local.before; globals} in
    Assembly.function__def program fragment config;
    Dispatch.call_def globals memory pc (C.Succ capacity); Dispatch.void_signature_def ();
    Dispatch.point_def (T.Instruction (I.Call_indirect (Dispatch.void_signature ()), Dispatch.tail ()))
      (Dispatch.labels ()) globals memory (S.Push (S.I32 pc, S.Empty)) (C.Succ capacity);
    Indirect.entry_def function_ call.Calls.current;
    let local = indirect_prefix resource module_ (Dispatch.void_signature ()) pc index function_
      (Dispatch.tail ()) call S.Empty capacity fuel guarded () in
    prepend resource module_ (Dispatch.two ())
      (Dispatch.loop globals memory (C.Succ capacity)) call local ())

let rec (suffix @ total) : (prefix : C.count) @ immutable -> (fuel : C.count) @ immutable ->
    (module_ : F.module_) @ immutable -> (start : Calls.configuration) @ immutable ->
    (guard : Calls.configuration) @ immutable -> (final : GE.state) @ immutable ->
    {u : unit | Calls.run prefix module_ start === Calls.Running guard
      && Calls.run fuel module_ start === Calls.Finished final} ->
    {rest : C.count | Calls.run rest module_ guard === Calls.Finished final} @ immutable ghost =
  fun prefix fuel module_ start guard final premise -> ghost_ (
    Calls.run_def prefix module_ start; Calls.run_def fuel module_ start;
    match prefix with
    | C.Zero -> fuel
    | C.Succ earlier -> match fuel with
      | C.Zero -> unreachable_ ()
      | C.Succ later -> match Calls.step module_ start with
        | Calls.Running next -> suffix earlier later module_ next guard final ()
        | _ -> unreachable_ ())
