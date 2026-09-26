module B = Wasm_u32
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module T = Wasm_control
module L = Wasm_locals
module Model = Hmc_failed_guard_model
module Local = Hmc_failed_guard_reach
module Lower = Hmc_wasm_program_lower
module Block = Hmc_wasm_program_block
module Structured = Hmc_wasm_structured_block
module Emit = Hmc_wasm_program_emit
module Runtime = Hmc_wasm_program_runtime
module Assembly = Hmc_wasm_program_functions
module Values = Hmc_wasm_program_registers
module Round = Hmc_wasm_program_roundtrip
module Lift = Wasm_control_lift
module Exit = Hmc_wasm_allocation_exit
module Closure = Hmc_wasm_closure_lower
module Write = Hmc_wasm_closure_write
module Success = Hmc_wasm_closure_success
module Call = Hmc_wasm_ordinary_call
module Saved = Hmc_wasm_call_save_guard
module Loaded = Hmc_wasm_loaded_call

let (locals @ total) : (resource : Model.resource) @ immutable -> (before : Values.registers) @ immutable ->
    {u : unit | L.can_set (Values.locals before) (Model.status_local ()) (S.I32 (Model.status resource))
      && L.get (Values.locals before) (Model.cursor_local resource)
        === Some (S.I32 (match resource with Model.Heap -> before.Values.heap | Model.Stack -> before.Values.top))
      && L.get (Values.locals before) (Model.limit_local resource)
        === Some (S.I32 (match resource with Model.Heap -> before.Values.heap_limit | Model.Stack -> before.Values.stack_limit))} @ ghost =
  fun resource before -> ghost_ (
    Values.local_values before; Values.matches_def (Values.locals before) before;
    Model.status_def resource; Model.status_local_def (); Model.cursor_local_def resource; Model.limit_local_def resource;
    L.can_set_def (Values.locals before) (Model.status_local ()) (S.I32 (Model.status resource));
    S.same_type_def (S.I32 before.Values.status) (S.I32 (Model.status resource)))

let (closure @ total) : (program : Lower.program) @ immutable -> (fragment : Closure.fragment) @ immutable ->
    (table_base : B.u32) -> (stack_base : B.u32) -> (before : Values.registers) @ immutable -> (memory : B.bytes) @ immutable ->
    {u : unit | before.Values.heap <= before.Values.heap_limit
      && before.Values.heap + fragment.Closure.object_.Write.bytes > before.Values.heap_limit} ->
    {out : Local.witness | Local.reaches Model.Heap
      {T.code = Emit.emit program (Block.Structured (Structured.Closure fragment))
          (Runtime.config table_base stack_base).Assembly.locals table_base stack_base;
        labels = Round.labels (Runtime.config table_base stack_base);
        state = {X.memory; machine = {E.locals = Values.locals before; stack = S.Empty}}} out} @ immutable ghost =
  fun program fragment table_base stack_base before memory premise -> ghost_ (
    let config = Runtime.config table_base stack_base in
    let code = Success.emit fragment.Closure.object_ fragment.Closure.pc (Model.frame_local ()) (Model.cursor_local Model.Heap) in
    locals Model.Heap before;
    Model.cursor_local_def Model.Heap; Model.limit_local_def Model.Heap; Model.status_def Model.Heap;
    Model.status_local_def (); Model.failure_depth_def (); Model.frame_local_def ();
    Runtime.config_def table_base stack_base;
    let out = Local.protected_guard Model.Heap fragment.Closure.object_.Write.bytes before.Values.heap before.Values.heap_limit
      {X.memory; machine = {E.locals = Values.locals before; stack = S.Empty}} (Round.labels config) (Lift.embed code T.Empty) T.Empty () in
    Emit.emit_def program (Block.Structured (Structured.Closure fragment)) config.Assembly.locals table_base stack_base;
    Structured.emit_def (Structured.Closure fragment) config.Assembly.locals.Emit.structured (Model.failure_depth ());
    Exit.emit_def fragment.Closure.object_.Write.bytes (Model.cursor_local Model.Heap) (Model.limit_local Model.Heap)
      code (Model.failure_depth ()) T.Empty;
    Exit.escape_def (Model.failure_depth ()); out)

let (call @ total) : (program : Lower.program) @ immutable -> (fragment : Block.call) @ immutable ->
    (table_base : B.u32) -> (stack_base : B.u32) -> (before : Values.registers) @ immutable -> (memory : B.bytes) @ immutable ->
    {u : unit | before.Values.top + program.Lower.width > before.Values.stack_limit} ->
    {out : Local.witness | Local.reaches Model.Stack
      {T.code = Emit.emit program (Block.Call fragment)
          (Runtime.config table_base stack_base).Assembly.locals table_base stack_base;
        labels = Round.labels (Runtime.config table_base stack_base);
        state = {X.memory; machine = {E.locals = Values.locals before; stack = S.Empty}}} out} @ immutable ghost =
  fun program fragment table_base stack_base before memory premise -> ghost_ (
    let config = Runtime.config table_base stack_base in
    let l = config.Assembly.locals in
    let b = l.Emit.structured in
    Runtime.config_def table_base stack_base;
    let code = Saved.body fragment.Block.save fragment.Block.padding b.Structured.frame l.Emit.top program.Lower.width in
    let tail = Loaded.emit fragment.Block.environment b.Structured.scratch program.Lower.calls table_base l.Emit.code
      l.Emit.address l.Emit.descriptor b.Structured.object_ b.Structured.frame in
    locals Model.Stack before;
    Model.cursor_local_def Model.Stack; Model.limit_local_def Model.Stack; Model.status_def Model.Stack;
    Model.status_local_def (); Model.failure_depth_def ();
    let out = Local.protected_guard Model.Stack program.Lower.width before.Values.top before.Values.stack_limit
      {X.memory; machine = {E.locals = Values.locals before; stack = S.Empty}} (Round.labels config) (Lift.embed code T.Empty) tail () in
    Emit.emit_def program (Block.Call fragment) l table_base stack_base;
    Call.emit_def fragment.Block.save fragment.Block.padding b.Structured.frame l.Emit.top program.Lower.width l.Emit.stack_limit
      (Model.failure_depth ()) fragment.Block.environment b.Structured.scratch program.Lower.calls table_base l.Emit.code l.Emit.address l.Emit.descriptor b.Structured.object_;
    Saved.emit_def fragment.Block.save fragment.Block.padding b.Structured.frame l.Emit.top program.Lower.width l.Emit.stack_limit
      (Model.failure_depth ()) tail;
    Exit.emit_def program.Lower.width l.Emit.top l.Emit.stack_limit code (Model.failure_depth ()) tail;
    Exit.escape_def (Model.failure_depth ()); out)

module C = Wasm_code
module Status = Hmc_wasm_program_status
module Capture = Hmc_wasm_cons_capture
module Pop = Hmc_wasm_value_pop
module Cons = Hmc_wasm_cons_success
module Recognize = Hmc_failed_guard_proof
module Fuel = Wasm_control_compose
module Select = Hmc_wasm_allocation_select

let (cons @ total) : (program : Lower.program) @ immutable -> (fragment : Pop.fragment) @ immutable ->
    (table_base : B.u32) -> (stack_base : B.u32) -> (before : Values.registers) @ immutable ->
    (memory : B.bytes) @ immutable -> (prepared : X.state) @ immutable -> (captured : X.state) @ immutable ->
    {u : unit | before.Values.heap <= before.Values.heap_limit && before.Values.heap + 32 > before.Values.heap_limit
      && T.run (Status.four ())
        {T.code = Emit.emit program (Block.Structured (Structured.Cons fragment))
          (Runtime.config table_base stack_base).Assembly.locals table_base stack_base;
          labels = Round.labels (Runtime.config table_base stack_base);
          state = {X.memory; machine = {E.locals = Values.locals before; stack = S.Empty}}}
        === T.Running {T.code = Structured.emit (Structured.Cons fragment)
          (Runtime.config table_base stack_base).Assembly.locals.Emit.structured (Status.exit_depth ());
          labels = Status.scope (Model.status_local ()) (Round.labels (Runtime.config table_base stack_base)); state = prepared}
      && X.run (Capture.emit fragment.Pop.head_tag fragment.Pop.head_payload
        (Runtime.config table_base stack_base).Assembly.locals.Emit.structured.Structured.scratch (Model.frame_local ())) prepared === X.Done captured
      && captured.X.machine.E.stack === S.Empty
      && L.get captured.X.machine.E.locals (Model.cursor_local Model.Heap) === Some (S.I32 before.Values.heap)
      && L.get captured.X.machine.E.locals (Model.limit_local Model.Heap) === Some (S.I32 before.Values.heap_limit)
      && L.get captured.X.machine.E.locals (Model.status_local ()) === Some (S.I32 (Model.status Model.Heap))} ->
    {out : Local.witness | Local.reaches Model.Heap
      {T.code = Emit.emit program (Block.Structured (Structured.Cons fragment))
          (Runtime.config table_base stack_base).Assembly.locals table_base stack_base;
        labels = Round.labels (Runtime.config table_base stack_base);
        state = {X.memory; machine = {E.locals = Values.locals before; stack = S.Empty}}} out} @ immutable ghost =
  fun program fragment table_base stack_base before memory prepared captured premise -> ghost_ (
    let config = Runtime.config table_base stack_base in
    let b = config.Assembly.locals.Emit.structured in
    let slots = b.Structured.scratch in
    let capture = Capture.emit fragment.Pop.head_tag fragment.Pop.head_payload slots b.Structured.frame in
    let success = Cons.emit fragment b.Structured.frame b.Structured.heap slots.Capture.head_tag slots.Capture.head_payload
      slots.Capture.tail_tag slots.Capture.tail_payload in
    let guard = Exit.emit (Wasm_four_words.width ()) b.Structured.heap b.Structured.limit success (Model.failure_depth ()) T.Empty in
    let labels = Status.scope (Model.status_local ()) (Round.labels config) in
    Model.frame_local_def (); Model.cursor_local_def Model.Heap; Model.limit_local_def Model.Heap;
    Model.status_local_def (); Model.failure_depth_def (); Model.status_def Model.Heap;
    Status.exit_depth_def (); Runtime.config_def table_base stack_base; Wasm_four_words.width_def ();
    Wasm_control_success.straight capture prepared captured ();
    Lift.correct capture guard labels prepared captured ();
    Exit.emit_def (Wasm_four_words.width ()) b.Structured.heap b.Structured.limit success (Model.failure_depth ()) T.Empty;
    Exit.escape_def (Model.failure_depth ());
    Recognize.recognize Model.Heap (Wasm_four_words.width ()) before.Values.heap before.Values.heap_limit captured labels
      (Lift.embed success T.Empty) T.Empty ();
    let local = {Local.prefix = C.length capture; before = {T.code = guard; labels; state = captured}} in
    Structured.emit_def (Structured.Cons fragment) b (Status.exit_depth ());
    let middle = {T.code = Structured.emit (Structured.Cons fragment) b (Status.exit_depth ()); labels; state = prepared} in
    Local.reaches_def Model.Heap middle local;
    Local.prepend Model.Heap (Status.four ())
      {T.code = Emit.emit program (Block.Structured (Structured.Cons fragment)) config.Assembly.locals table_base stack_base;
        labels = Round.labels config; state = {X.memory; machine = {E.locals = Values.locals before; stack = S.Empty}}}
      middle local ())
