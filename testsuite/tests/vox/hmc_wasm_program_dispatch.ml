module B = Wasm_u32
module C = Wasm_code
module I = Wasm_instruction
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module Memory = Wasm_memory
module GE = Wasm_global_execution
module G = Wasm_globals
module T = Wasm_control
module P = Wasm_instance_control
module F = Wasm_functions
module M = Wasm_calls
module Body = Wasm_calls_body
module Runtime = Hmc_wasm_program_runtime
let[@def] (void_signature @ total) (unit : unit) : B.u32 = 0
let[@def] (frame_global @ total) (unit : unit) : B.u32 = 0
let[@def] (status_global @ total) (unit : unit) : B.u32 = 5
let[@def] (pc_offset @ total) (unit : unit) : B.u32 = 8
let[@def] (two @ total) (unit : unit) = C.Succ (C.Succ C.Zero)
let[@def] (three @ total) (unit : unit) = C.Succ (two ())
let[@def] (six @ total) (unit : unit) = C.Succ (C.Succ (C.Succ (three ())))
let[@def] (tail @ total) (unit : unit) = T.Instruction (I.Global_get 5,
  T.Instruction (I.Plain I.I32_eqz, T.Instruction (I.Br_if 0, T.Empty)))
let[@def] (exit @ total) (unit : unit) = T.Instruction (I.Global_get 5, T.Empty)
let[@def] (labels @ total) (unit : unit) =
  T.Label ({T.restart = Some (Runtime.dispatch_body ()); continuation = exit (); saved = S.Empty}, T.No_labels)
let[@def] (point @ total) (code : T.code @ immutable) (labels : T.labels @ immutable) (globals : G.t @ immutable)
    (memory : B.bytes @ immutable) (stack : S.stack @ immutable) (capacity : C.count @ immutable) =
  {M.current = {P.globals; body = {T.code; labels; state = {X.memory; machine = {E.locals = S.Empty; stack}}}};
    result = F.I32; callers = M.Root; capacity}
let[@def] (loop @ total) (globals : G.t @ immutable) (memory : B.bytes @ immutable) (capacity : C.count @ immutable) =
  point (Runtime.dispatch_body ()) (labels ()) globals memory S.Empty capacity
let[@def] (call @ total) (globals : G.t @ immutable) (memory : B.bytes @ immutable) (pc : B.u32) (capacity : C.count @ immutable) =
  point (T.Instruction (I.Call_indirect 0, tail ())) (labels ()) globals memory (S.Push (S.I32 pc, S.Empty)) capacity
let[@def] (returned @ total) (globals : G.t @ immutable) (memory : B.bytes @ immutable) (capacity : C.count @ immutable) =
  point (tail ()) (labels ()) globals memory S.Empty capacity
let (header @ total) : (module_ : F.module_) @ immutable -> (globals : G.t) @ immutable -> (memory : B.bytes) @ immutable ->
    (capacity : C.count) @ immutable -> (base : B.u32) -> (pc : B.u32) ->
    {u : unit | G.get globals (frame_global ()) === Some (S.I32 base) && Memory.load memory base (pc_offset ()) Memory.W32 === Some (S.I32 pc)} ->
    {u : unit | M.run (two ()) module_ (loop globals memory capacity) === M.Running (call globals memory pc capacity)} @ ghost =
  fun module_ globals memory capacity base pc premise -> ghost_ (
    frame_global_def (); pc_offset_def (); Runtime.dispatch_body_def (); tail_def (); loop_def globals memory capacity; call_def globals memory pc capacity; two_def ();
    let p0 = point (Runtime.dispatch_body ()) (labels ()) globals memory (S.Empty) capacity in
    let p1 = point (T.Instruction (I.I32_load (0, 8), T.Instruction (I.Call_indirect 0, tail ()))) (labels ()) globals memory (S.Push (S.I32 base, S.Empty)) capacity in
    point_def (Runtime.dispatch_body ()) (labels ()) globals memory (S.Empty) capacity;
    point_def (T.Instruction (I.I32_load (0, 8), T.Instruction (I.Call_indirect 0, tail ()))) (labels ()) globals memory (S.Push (S.I32 base, S.Empty)) capacity;
    P.step_def p0.M.current; T.step_def p0.M.current.P.body;
    GE.step_def (I.Global_get 0) {GE.globals; execution = p0.M.current.P.body.T.state};
    X.step_def (I.Global_get 0) p0.M.current.P.body.T.state;
    E.step_def (I.Global_get 0) p0.M.current.P.body.T.state.X.machine;
    S.step_def (I.Global_get 0) (S.Empty);
    Body.step module_ p0 p1.M.current ();
    let p1 = point (T.Instruction (I.I32_load (0, 8), T.Instruction (I.Call_indirect 0, tail ()))) (labels ()) globals memory (S.Push (S.I32 base, S.Empty)) capacity in
    let p2 = point (T.Instruction (I.Call_indirect 0, tail ())) (labels ()) globals memory (S.Push (S.I32 pc, S.Empty)) capacity in
    point_def (T.Instruction (I.I32_load (0, 8), T.Instruction (I.Call_indirect 0, tail ()))) (labels ()) globals memory (S.Push (S.I32 base, S.Empty)) capacity;
    point_def (T.Instruction (I.Call_indirect 0, tail ())) (labels ()) globals memory (S.Push (S.I32 pc, S.Empty)) capacity;
    P.step_def p1.M.current; T.step_def p1.M.current.P.body;
    GE.step_def (I.I32_load (0, 8)) {GE.globals; execution = p1.M.current.P.body.T.state};
    X.step_def (I.I32_load (0, 8)) p1.M.current.P.body.T.state;
    E.step_def (I.I32_load (0, 8)) p1.M.current.P.body.T.state.X.machine;
    S.step_def (I.I32_load (0, 8)) (S.Push (S.I32 base, S.Empty));
    X.read_def Memory.W32 8 p1.M.current.P.body.T.state;
    Body.step module_ p1 p2.M.current ();
    M.run_def (two ()) module_ p0; M.run_def (C.Succ C.Zero) module_ p1; M.run_def C.Zero module_ p2)
let (resume @ total) : (module_ : F.module_) @ immutable -> (globals : G.t) @ immutable -> (memory : B.bytes) @ immutable ->
    (capacity : C.count) @ immutable -> (status : B.u32) ->
    {u : unit | G.get globals (status_global ()) === Some (S.I32 status)} ->
    {u : unit | M.run (if status = 0 then three () else six ()) module_ (returned globals memory capacity) ===
      (if status = 0 then M.Running (loop globals memory capacity) else M.Finished
        {GE.globals; execution = {X.memory; machine = {E.locals = S.Empty; stack = S.Push (S.I32 status, S.Empty)}}})} @ ghost =
  fun module_ globals memory capacity status premise -> ghost_ (
    status_global_def (); tail_def (); exit_def (); labels_def (); returned_def globals memory capacity; loop_def globals memory capacity;
    two_def (); three_def (); six_def ();
    let p0 = point (tail ()) (labels ()) globals memory (S.Empty) capacity in
    let p1 = point (T.Instruction (I.Plain I.I32_eqz, T.Instruction (I.Br_if 0, T.Empty))) (labels ()) globals memory (S.Push (S.I32 status, S.Empty)) capacity in
    point_def (tail ()) (labels ()) globals memory (S.Empty) capacity;
    point_def (T.Instruction (I.Plain I.I32_eqz, T.Instruction (I.Br_if 0, T.Empty))) (labels ()) globals memory (S.Push (S.I32 status, S.Empty)) capacity;
    P.step_def p0.M.current; T.step_def p0.M.current.P.body;
    GE.step_def (I.Global_get 5) {GE.globals; execution = p0.M.current.P.body.T.state};
    X.step_def (I.Global_get 5) p0.M.current.P.body.T.state;
    E.step_def (I.Global_get 5) p0.M.current.P.body.T.state.X.machine;
    S.step_def (I.Global_get 5) (S.Empty);
    Body.step module_ p0 p1.M.current ();
    let p1 = point (T.Instruction (I.Plain I.I32_eqz, T.Instruction (I.Br_if 0, T.Empty))) (labels ()) globals memory (S.Push (S.I32 status, S.Empty)) capacity in
    let p2 = point (T.Instruction (I.Br_if 0, T.Empty)) (labels ()) globals memory (S.Push (S.I32 (S.boolean (status = 0)), S.Empty)) capacity in
    point_def (T.Instruction (I.Plain I.I32_eqz, T.Instruction (I.Br_if 0, T.Empty))) (labels ()) globals memory (S.Push (S.I32 status, S.Empty)) capacity;
    point_def (T.Instruction (I.Br_if 0, T.Empty)) (labels ()) globals memory (S.Push (S.I32 (S.boolean (status = 0)), S.Empty)) capacity;
    P.step_def p1.M.current; T.step_def p1.M.current.P.body;
    GE.step_def (I.Plain I.I32_eqz) {GE.globals; execution = p1.M.current.P.body.T.state};
    X.step_def (I.Plain I.I32_eqz) p1.M.current.P.body.T.state;
    E.step_def (I.Plain I.I32_eqz) p1.M.current.P.body.T.state.X.machine;
    S.step_def (I.Plain I.I32_eqz) (S.Push (S.I32 status, S.Empty));
    S.boolean_def (status = 0);
    Body.step module_ p1 p2.M.current ();
    let p2 = point (T.Instruction (I.Br_if 0, T.Empty)) (labels ()) globals memory (S.Push (S.I32 (S.boolean (status = 0)), S.Empty)) capacity in
    let p3 = point (if status = 0 then Runtime.dispatch_body () else T.Empty) (labels ()) globals memory (S.Empty) capacity in
    point_def (T.Instruction (I.Br_if 0, T.Empty)) (labels ()) globals memory (S.Push (S.I32 (S.boolean (status = 0)), S.Empty)) capacity;
    point_def (if status = 0 then Runtime.dispatch_body () else T.Empty) (labels ()) globals memory (S.Empty) capacity;
    P.step_def p2.M.current; T.step_def p2.M.current.P.body;
    T.stack_def p2.M.current.P.body.T.state S.Empty;
    T.branch_def 0 (labels ()) (T.stack p2.M.current.P.body.T.state S.Empty);
    T.stack_def (T.stack p2.M.current.P.body.T.state S.Empty) S.Empty;
    Body.step module_ p2 p3.M.current ();
    if status = 0 then (
      M.run_def (three ()) module_ p0; M.run_def (two ()) module_ p1;
      M.run_def (C.Succ C.Zero) module_ p2; M.run_def C.Zero module_ p3
    ) else (
    let p3 = point (T.Empty) (labels ()) globals memory (S.Empty) capacity in
    let p4 = point (exit ()) (T.No_labels) globals memory (S.Empty) capacity in
    point_def (T.Empty) (labels ()) globals memory (S.Empty) capacity;
    point_def (exit ()) (T.No_labels) globals memory (S.Empty) capacity;
    P.step_def p3.M.current; T.step_def p3.M.current.P.body;
    T.stack_def p3.M.current.P.body.T.state S.Empty;
    Body.step module_ p3 p4.M.current ();
    let p4 = point (exit ()) (T.No_labels) globals memory (S.Empty) capacity in
    let p5 = point (T.Empty) (T.No_labels) globals memory (S.Push (S.I32 status, S.Empty)) capacity in
    point_def (exit ()) (T.No_labels) globals memory (S.Empty) capacity;
    point_def (T.Empty) (T.No_labels) globals memory (S.Push (S.I32 status, S.Empty)) capacity;
    P.step_def p4.M.current; T.step_def p4.M.current.P.body;
    GE.step_def (I.Global_get 5) {GE.globals; execution = p4.M.current.P.body.T.state};
    X.step_def (I.Global_get 5) p4.M.current.P.body.T.state;
    E.step_def (I.Global_get 5) p4.M.current.P.body.T.state.X.machine;
    S.step_def (I.Global_get 5) (S.Empty);
    Body.step module_ p4 p5.M.current ();
    M.step_def module_ p5; M.advance_def p5; P.step_def p5.M.current; T.step_def p5.M.current.P.body;
    F.complete_def F.I32 (S.Push (S.I32 status, S.Empty));
    M.leave_def p5; F.take_result_def F.I32 (S.Push (S.I32 status, S.Empty));
    F.deliver_def (Some (S.I32 status)) S.Empty;
    T.stack_def p5.M.current.P.body.T.state (S.Push (S.I32 status, S.Empty));
    M.run_def (six ()) module_ p0; M.run_def (C.Succ (C.Succ (three ()))) module_ p1;
    M.run_def (C.Succ (three ())) module_ p2; M.run_def (three ()) module_ p3;
    M.run_def (two ()) module_ p4; M.run_def (C.Succ C.Zero) module_ p5))
let (enter @ total) : (module_ : F.module_) @ immutable -> (globals : G.t) @ immutable -> (memory : B.bytes) @ immutable ->
    (capacity : C.count) @ immutable ->
    {u : unit | M.step module_ (point (Runtime.dispatcher ()).F.code T.No_labels globals memory S.Empty capacity)
      === M.Running (loop globals memory capacity)} @ ghost = fun module_ globals memory capacity -> ghost_ (
    Runtime.dispatcher_def (); exit_def (); labels_def (); loop_def globals memory capacity;
    point_def (Runtime.dispatcher ()).F.code T.No_labels globals memory S.Empty capacity;
    point_def (Runtime.dispatch_body ()) (labels ()) globals memory S.Empty capacity;
    let initial = point (Runtime.dispatcher ()).F.code T.No_labels globals memory S.Empty capacity in
    P.step_def initial.M.current; T.step_def initial.M.current.P.body;
    T.enter_def (Runtime.dispatch_body ()) (exit ()) (Some (Runtime.dispatch_body ())) initial.M.current.P.body;
    T.stack_def initial.M.current.P.body.T.state S.Empty;
    Body.step module_ initial (loop globals memory capacity).M.current ())
module Fuel = Wasm_control_compose
let[@def] (cost @ total) (body : C.count @ immutable) (status : B.u32) =
  Fuel.add (two ()) (Fuel.add body (if status = 0 then three () else six ()))
let (iteration @ total) : (module_ : F.module_) @ immutable ->
    (before : G.t) @ immutable -> (memory : B.bytes) @ immutable -> (capacity : C.count) @ immutable ->
    (base : B.u32) -> (pc : B.u32) -> (body_fuel : C.count) @ immutable ->
    (after : G.t) @ immutable -> (next_memory : B.bytes) @ immutable -> (status : B.u32) ->
    {u : unit | G.get before (frame_global ()) === Some (S.I32 base)
      && Memory.load memory base (pc_offset ()) Memory.W32 === Some (S.I32 pc)
      && M.run body_fuel module_ (call before memory pc capacity) === M.Running (returned after next_memory capacity)
      && G.get after (status_global ()) === Some (S.I32 status)} ->
    {u : unit | M.run (cost body_fuel status) module_ (loop before memory capacity) ===
      (if status = 0 then M.Running (loop after next_memory capacity) else M.Finished
        {GE.globals = after; execution = {X.memory = next_memory; machine = {E.locals = S.Empty; stack = S.Push (S.I32 status, S.Empty)}}})} @ ghost =
  fun module_ before memory capacity base pc body_fuel after next_memory status premise -> ghost_ (
    header module_ before memory capacity base pc ();
    resume module_ after next_memory capacity status ();
    Body.compose body_fuel (if status = 0 then three () else six ()) module_ (call before memory pc capacity);
    Body.compose (two ()) (Fuel.add body_fuel (if status = 0 then three () else six ())) module_ (loop before memory capacity);
    cost_def body_fuel status)
module Lower = Hmc_wasm_program_lower
module Block = Hmc_wasm_program_block
module Emit = Hmc_wasm_program_emit
module Assembly = Hmc_wasm_program_functions
module R = Wasm_global_registers
module Round = Hmc_wasm_program_roundtrip
let (generated_iteration @ total) : (program : Lower.program) @ immutable -> (fragment : Block.fragment) @ immutable ->
    (config : Assembly.config) @ immutable -> (module_ : F.module_) @ immutable ->
    (before : G.t) @ immutable -> (memory : B.bytes) @ immutable -> (capacity : C.count) @ immutable ->
    (base : B.u32) -> (pc : B.u32) -> (index : B.u32) ->
    (imported : X.state) @ immutable -> (after : X.state) @ immutable -> (exported : GE.state) @ immutable ->
    (body_fuel : C.count) @ immutable -> (status : B.u32) ->
    {u : unit | G.get before (frame_global ()) === Some (S.I32 base)
      && Memory.load memory base (pc_offset ()) Memory.W32 === Some (S.I32 pc)
      && F.signature module_.F.signatures (void_signature ()) === Some F.Void
      && F.element module_.F.table pc === Some index
      && F.lookup module_.F.functions index === Some (Assembly.function_ program fragment config)
      && GE.run (R.load_code config.Assembly.loads)
        {GE.globals = before; execution = {X.memory; machine = {E.locals = F.zero_locals config.Assembly.local_types; stack = S.Empty}}}
        === GE.Done {GE.globals = before; execution = imported}
      && imported.X.machine.E.stack === S.Empty && after.X.machine.E.stack === S.Empty
      && T.run body_fuel {T.code = Emit.emit program fragment config.Assembly.locals config.Assembly.table_base config.Assembly.stack_base;
        labels = Round.labels config; state = imported} === T.Running {T.code = T.Empty; labels = Round.labels config; state = after}
      && GE.run (R.store_code config.Assembly.stores) {GE.globals = before; execution = after} === GE.Done exported
      && G.get exported.GE.globals (status_global ()) === Some (S.I32 status)} ->
    {u : unit | M.run (cost (Round.cost config body_fuel) status) module_ (loop before memory (C.Succ capacity)) ===
      (if status = 0 then M.Running (loop exported.GE.globals exported.GE.execution.X.memory (C.Succ capacity)) else M.Finished
        {GE.globals = exported.GE.globals; execution = {X.memory = exported.GE.execution.X.memory;
          machine = {E.locals = S.Empty; stack = S.Push (S.I32 status, S.Empty)}}})} @ ghost =
  fun program fragment config module_ before memory capacity base pc index imported after exported body_fuel status premise -> ghost_ (
    void_signature_def (); frame_global_def (); call_def before memory pc (C.Succ capacity);
    point_def (T.Instruction (I.Call_indirect 0, tail ())) (labels ()) before memory (S.Push (S.I32 pc, S.Empty)) (C.Succ capacity);
    returned_def exported.GE.globals exported.GE.execution.X.memory (C.Succ capacity);
    point_def (tail ()) (labels ()) exported.GE.globals exported.GE.execution.X.memory S.Empty (C.Succ capacity);
    Round.correct program fragment config module_ (call before memory pc (C.Succ capacity)) 0 pc index (tail ()) S.Empty capacity imported after exported body_fuel ();
    iteration module_ before memory (C.Succ capacity) base pc (Round.cost config body_fuel) exported.GE.globals exported.GE.execution.X.memory status ())
