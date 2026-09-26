module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module V = Hmc_tagged_cell
module Program = Hmc_tail_ir
module Machine = Hmc_heap_machine
module Codec = Hmc_pointer_frame_codec
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Cells = Hmc_wasm_call_save_memory
module Index = Hmc_u32_index
module Bytes = Hmc_linear_bytes
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module L = Wasm_locals
module I = Wasm_instruction
module C = Wasm_code
module T = Wasm_control
module Read = Wasm_frame_snapshot
module Values = Wasm_snapshot_values
module Lift = Wasm_control_lift
module Fuel = Wasm_control_compose
module Select = Hmc_wasm_return_select
module Continue = Wasm_control_branch_continue
module Stack = Hmc_memory_stack
module Root = Hmc_wasm_root_return
module Status = Hmc_wasm_program_status
module Emit = Hmc_wasm_program_emit
module Lower = Hmc_wasm_program_lower
module Caller = Hmc_wasm_caller_return
module Restore = Hmc_wasm_frame_restore
module LP = Wasm_local_preservation
let[@def] (finished @ total) (unit : unit) : B.u32 = 1
type result = {state : X.state; fuel : C.count}
let (correct @ total) : (lowered : Lower.program) @ immutable -> (locals : Emit.locals) @ immutable -> (table_base : B.u32) -> (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (heap : H.heap) @ immutable -> (heap_limit : B.u32) -> (stack_limit : D.index) @ immutable ->
    (activation : F.activation) @ immutable -> (pc : B.u32) -> (rest : H.cells) @ immutable ->
    (state : X.state) @ immutable -> (base : B.u32) -> (bytes : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    (frame_local : B.u32) -> (tag_local : B.u32) -> (payload_local : B.u32) -> (top_local : B.u32) ->
    (stack_base : B.u32) -> (width : B.u32) -> (labels : T.labels) @ immutable ->
    {u : unit | frame_local = locals.Emit.structured.Hmc_wasm_structured_block.frame && top_local = locals.Emit.top
      && tag_local = locals.Emit.result_tag && payload_local = locals.Emit.result_payload && width > 0
      && locals.Emit.status <> frame_local && locals.Emit.status <> top_local && locals.Emit.status <> tag_local && locals.Emit.status <> payload_local
      && L.can_set state.X.machine.E.locals locals.Emit.status (S.I32 (Status.zero ()))
      && LP.preserves (Read.emit (Root.reads tag_local payload_local) frame_local) locals.Emit.status
      && Program.lookup program.Program.code activation.F.pc === Some (Program.Keep G.Return)
      && activation.F.temporaries === F.Empty && Index.represents activation.F.pc pc
      && base <= 4294967248 && Bytes.drop state.X.memory base === Some bytes
      && Wire.decode_cells (H.length (Cells.cells pc activation.F.current activation.F.accumulator rest)) bytes ===
        Some (Cells.cells pc activation.F.current activation.F.accumulator rest, suffix)
      && state.X.machine.E.stack === S.Empty && L.get state.X.machine.E.locals frame_local === Some (S.I32 base)
      && L.get state.X.machine.E.locals top_local === Some (S.I32 stack_base)
      && Stack.related program.Program.origin.Hmc_cfg_program.blocks width state.X.memory stack_base stack_base Q.Halt
      && tag_local <> payload_local && tag_local <> frame_local && payload_local <> frame_local
      && tag_local <> top_local && payload_local <> top_local
      && L.can_set state.X.machine.E.locals tag_local (S.I64 (V.tag activation.F.accumulator))
      && L.can_set state.X.machine.E.locals payload_local (S.I64 (V.payload activation.F.accumulator))} ->
    {out : result | out.state.X.memory === state.X.memory && out.state.X.machine.E.stack === S.Empty
      && L.get out.state.X.machine.E.locals tag_local === Some (S.I64 (V.tag activation.F.accumulator))
      && L.get out.state.X.machine.E.locals payload_local === Some (S.I64 (V.payload activation.F.accumulator))
      && L.get out.state.X.machine.E.locals frame_local === Some (S.I32 base)
      && L.get out.state.X.machine.E.locals top_local === Some (S.I32 stack_base)
      && L.get out.state.X.machine.E.locals locals.Emit.status === Some (S.I32 (finished ()))
      && Machine.step program globals heap_limit stack_limit {Machine.heap; state = Q.Running (activation, Q.Halt)} ===
        Machine.Advanced {Machine.heap; state = Q.Done activation.F.accumulator}
      && T.run out.fuel {T.code = Emit.emit lowered Hmc_wasm_program_block.Return locals table_base stack_base; labels; state}
        === T.Running {T.code = T.Empty; labels; state = out.state}} @ immutable =
  fun lowered locals table_base program globals heap heap_limit stack_limit activation pc rest state base bytes suffix frame_local tag_local payload_local top_local stack_base width labels premise ->
    let local = locals.Emit.status in
    let read = Read.emit (Root.reads tag_local payload_local) frame_local in
    let done_code = Emit.status local (finished ()) T.Empty in
    let root = Lift.embed read done_code in
    let caller = Lift.embed (Hmc_wasm_return_frame.emit lowered.Lower.restore (Restore.width lowered.Lower.capacity) frame_local top_local) T.Empty in
    let select = Caller.emit lowered.Lower.restore lowered.Lower.capacity frame_local top_local stack_base root T.Empty in
    let prepared = Status.write local (Status.zero ()) select labels state () in
    ghost_ (L.other_local state.X.machine.E.locals local (S.I32 (Status.zero ())) prepared.X.machine.E.locals frame_local ();
      L.other_local state.X.machine.E.locals local (S.I32 (Status.zero ())) prepared.X.machine.E.locals top_local ();
      L.other_local state.X.machine.E.locals local (S.I32 (Status.zero ())) prepared.X.machine.E.locals tag_local ();
      L.other_local state.X.machine.E.locals local (S.I32 (Status.zero ())) prepared.X.machine.E.locals payload_local ();
      L.can_set_def state.X.machine.E.locals tag_local (S.I64 (V.tag activation.F.accumulator));
      L.can_set_def prepared.X.machine.E.locals tag_local (S.I64 (V.tag activation.F.accumulator));
      L.can_set_def state.X.machine.E.locals payload_local (S.I64 (V.payload activation.F.accumulator));
      L.can_set_def prepared.X.machine.E.locals payload_local (S.I64 (V.payload activation.F.accumulator)));
    let output = Root.correct program globals heap heap_limit stack_limit activation pc rest prepared base bytes suffix frame_local tag_local payload_local top_local stack_base width 0 labels () in
    let read_state = {X.memory = state.X.memory; machine = {E.locals = output; stack = S.Empty}} in
    let inner = Continue.labels T.Empty labels in
    ghost_ (LP.correct read prepared read_state local ();
      L.can_set_def output local (S.I32 (finished ()));
      S.same_type_def (S.I32 (Status.zero ())) (S.I32 (finished ())));
    let after = Status.write local (finished ()) T.Empty inner read_state () in
    let exit_fuel = Fuel.add (Status.two ()) (C.Succ C.Zero) in
    let root_fuel = Fuel.add (C.length read) exit_fuel in
    let selected_fuel = Fuel.add (Select.cost stack_base top_local) root_fuel in
    let fuel = Fuel.add (Status.two ()) selected_fuel in
    ghost_ (L.other_local output local (S.I32 (finished ())) after.X.machine.E.locals tag_local ();
      L.other_local output local (S.I32 (finished ())) after.X.machine.E.locals payload_local ();
      L.other_local output local (S.I32 (finished ())) after.X.machine.E.locals frame_local ();
      L.other_local output local (S.I32 (finished ())) after.X.machine.E.locals top_local ();
      Select.correct program.Program.origin.Hmc_cfg_program.blocks Q.Halt width stack_base top_local stack_base prepared labels root caller T.Empty ();
      Continue.labels_def T.Empty labels;
      Wasm_control_success.straight read prepared read_state ();
      Lift.correct read done_code inner prepared read_state ();
      T.run_def (C.Succ C.Zero) {T.code = T.Empty; labels = inner; state = after};
      T.step_def {T.code = T.Empty; labels = inner; state = after};
      T.stack_def after S.Empty;
      T.run_def C.Zero {T.code = T.Empty; labels; state = after};
      Fuel.correct (Status.two ()) (C.Succ C.Zero) {T.code = done_code; labels = inner; state = read_state};
      Fuel.correct (C.length read) exit_fuel {T.code = root; labels = inner; state = prepared};
      Caller.emit_def lowered.Lower.restore lowered.Lower.capacity frame_local top_local stack_base root T.Empty;
      Fuel.correct (Select.cost stack_base top_local) root_fuel {T.code = select; labels; state = prepared};
      Fuel.correct (Status.two ()) selected_fuel {T.code = Emit.status local (Status.zero ()) select; labels; state};
      finished_def (); Status.zero_def (); Emit.emit_def lowered Hmc_wasm_program_block.Return locals table_base stack_base);
    {state = after; fuel}
