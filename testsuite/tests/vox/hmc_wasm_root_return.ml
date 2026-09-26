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
let[@def] (reads @ total) (tag_local : B.u32) (payload_local : B.u32) =
  Read.Read (32, tag_local, Read.Read (40, payload_local, Read.End))
let[@def] (emit @ total) (frame_local : B.u32) (tag_local : B.u32) (payload_local : B.u32) (depth : B.u32) =
  Lift.embed (Read.emit (reads tag_local payload_local) frame_local) (T.Instruction (I.Br depth, T.Empty))
let[@def] (cost @ total) (frame_local : B.u32) (tag_local : B.u32) (payload_local : B.u32) =
  Fuel.add (C.length (Read.emit (reads tag_local payload_local) frame_local)) (C.Succ C.Zero)
let (correct @ total) : (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (heap : H.heap) @ immutable -> (heap_limit : B.u32) -> (stack_limit : D.index) @ immutable ->
    (activation : F.activation) @ immutable -> (pc : B.u32) -> (rest : H.cells) @ immutable ->
    (state : X.state) @ immutable -> (base : B.u32) -> (bytes : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    (frame_local : B.u32) -> (tag_local : B.u32) -> (payload_local : B.u32) -> (top_local : B.u32) ->
    (stack_base : B.u32) -> (width : B.u32) -> (depth : B.u32) -> (labels : T.labels) @ immutable ->
    {u : unit | Program.lookup program.Program.code activation.F.pc === Some (Program.Keep G.Return)
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
    {out : S.stack | X.run (Read.emit (reads tag_local payload_local) frame_local) state === X.Done {X.memory = state.X.memory; machine = {E.locals = out; stack = S.Empty}}
      && L.get out tag_local === Some (S.I64 (V.tag activation.F.accumulator))
      && L.get out payload_local === Some (S.I64 (V.payload activation.F.accumulator))
      && L.get out frame_local === Some (S.I32 base) && L.get out top_local === Some (S.I32 stack_base)
      && Machine.step program globals heap_limit stack_limit {Machine.heap; state = Q.Running (activation, Q.Halt)} ===
        Machine.Advanced {Machine.heap; state = Q.Done activation.F.accumulator}
      && T.run (cost frame_local tag_local payload_local)
        {T.code = emit frame_local tag_local payload_local depth; labels; state} ===
        T.branch depth labels {X.memory = state.X.memory; machine = {E.locals = out; stack = S.Empty}}} @ immutable =
  fun program globals heap heap_limit stack_limit activation pc rest state base bytes suffix frame_local tag_local payload_local top_local stack_base width depth labels premise ->
    let plan = reads tag_local payload_local in
    let cells = Cells.cells pc activation.F.current activation.F.accumulator rest in
    ghost_ (Cells.cells_def pc activation.F.current activation.F.accumulator rest;
      Hmc_heap_simple.lookup_def cells (D.S (D.S D.Z));
      Hmc_heap_simple.lookup_def (H.Cell (activation.F.current, H.Cell (activation.F.accumulator, rest))) (D.S D.Z);
      Hmc_heap_simple.lookup_def (H.Cell (activation.F.accumulator, rest)) D.Z;
      Index.represents_def (D.S (D.S D.Z)) 2; Index.represents_def (D.S D.Z) 1; Index.represents_def D.Z 0;
      Hmc_wasm_cells_read.correct state.X.memory base bytes (H.length cells) cells suffix (D.S (D.S D.Z)) 2 32 40 activation.F.accumulator ();
      reads_def tag_local payload_local;
      Values.ready_def plan state.X.memory base state.X.machine.E.locals;
      Values.ready_def (Read.Read (40, payload_local, Read.End)) state.X.memory base state.X.machine.E.locals;
      Values.ready_def Read.End state.X.memory base state.X.machine.E.locals);
    let out = Values.project plan state.X.memory base state.X.machine.E.locals () in
    let after = {X.memory = state.X.memory; machine = {E.locals = out; stack = S.Empty}} in
    ghost_ (Read.separate_def plan frame_local; Read.separate_def (Read.Read (40, payload_local, Read.End)) frame_local; Read.separate_def Read.End frame_local;
      Read.separate_def plan top_local; Read.separate_def (Read.Read (40, payload_local, Read.End)) top_local; Read.separate_def Read.End top_local;
      Read.correct plan frame_local state base out ();
      Values.selected_def plan tag_local; Values.selected_def (Read.Read (40, payload_local, Read.End)) tag_local; Values.selected_def Read.End tag_local;
      Values.selected_def plan payload_local; Values.selected_def (Read.Read (40, payload_local, Read.End)) payload_local; Values.selected_def Read.End payload_local;
      Values.get plan state.X.memory base state.X.machine.E.locals out tag_local ();
      Values.get plan state.X.memory base state.X.machine.E.locals out payload_local ();
      Values.unchanged plan state.X.memory base state.X.machine.E.locals out top_local ();
      Hmc_heap_return_transition.root program globals heap heap_limit stack_limit activation ();
      Wasm_control_success.straight (Read.emit plan frame_local) state after ();
      Lift.correct (Read.emit plan frame_local) (T.Instruction (I.Br depth, T.Empty)) labels state after ();
      emit_def frame_local tag_local payload_local depth; cost_def frame_local tag_local payload_local;
      Fuel.correct (C.length (Read.emit plan frame_local)) (C.Succ C.Zero) {T.code = emit frame_local tag_local payload_local depth; labels; state};
      T.run_def (C.Succ C.Zero) {T.code = T.Instruction (I.Br depth, T.Empty); labels; state = after};
      T.step_def {T.code = T.Instruction (I.Br depth, T.Empty); labels; state = after};
      (match T.branch depth labels after with T.Running next -> T.run_def C.Zero next | _ -> ()));
    out

let[@def] (selected_emit @ total) (frame_local : B.u32) (tag_local : B.u32) (payload_local : B.u32)
    (top_local : B.u32) (stack_base : B.u32) (depth : B.u32) (caller : T.code @ immutable) =
  Select.emit stack_base top_local (emit frame_local tag_local payload_local depth) caller T.Empty
let[@def] (selected_cost @ total) (frame_local : B.u32) (tag_local : B.u32) (payload_local : B.u32) (top_local : B.u32) (stack_base : B.u32) =
  Fuel.add (Select.cost stack_base top_local) (cost frame_local tag_local payload_local)
let (selected_correct @ total) : (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (heap : H.heap) @ immutable -> (heap_limit : B.u32) -> (stack_limit : D.index) @ immutable ->
    (activation : F.activation) @ immutable -> (pc : B.u32) -> (rest : H.cells) @ immutable ->
    (state : X.state) @ immutable -> (base : B.u32) -> (bytes : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    (frame_local : B.u32) -> (tag_local : B.u32) -> (payload_local : B.u32) -> (top_local : B.u32) ->
    (stack_base : B.u32) -> (width : B.u32) -> (depth : B.u32) -> (labels : T.labels) @ immutable -> (caller : T.code) @ immutable ->
    {u : unit | width > 0 && Program.lookup program.Program.code activation.F.pc === Some (Program.Keep G.Return)
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
    {out : S.stack | L.get out tag_local === Some (S.I64 (V.tag activation.F.accumulator))
      && L.get out payload_local === Some (S.I64 (V.payload activation.F.accumulator))
      && L.get out frame_local === Some (S.I32 base) && L.get out top_local === Some (S.I32 stack_base)
      && Machine.step program globals heap_limit stack_limit {Machine.heap; state = Q.Running (activation, Q.Halt)} ===
        Machine.Advanced {Machine.heap; state = Q.Done activation.F.accumulator}
      && T.run (selected_cost frame_local tag_local payload_local top_local stack_base)
        {T.code = selected_emit frame_local tag_local payload_local top_local stack_base depth caller; labels; state} ===
        T.branch depth (Continue.labels T.Empty labels) {X.memory = state.X.memory; machine = {E.locals = out; stack = S.Empty}}} @ immutable =
  fun program globals heap heap_limit stack_limit activation pc rest state base bytes suffix frame_local tag_local payload_local top_local stack_base width depth labels caller premise ->
    ghost_ (Select.correct program.Program.origin.Hmc_cfg_program.blocks Q.Halt width stack_base top_local stack_base state labels
      (emit frame_local tag_local payload_local depth) caller T.Empty ();
      Continue.labels_def T.Empty labels);
    let out = correct program globals heap heap_limit stack_limit activation pc rest state base bytes suffix frame_local tag_local payload_local top_local stack_base width depth (Continue.labels T.Empty labels) () in
    ghost_ (selected_emit_def frame_local tag_local payload_local top_local stack_base depth caller;
      selected_cost_def frame_local tag_local payload_local top_local stack_base;
      Fuel.correct (Select.cost stack_base top_local) (cost frame_local tag_local payload_local)
        {T.code = selected_emit frame_local tag_local payload_local top_local stack_base depth caller; labels; state});
    out
