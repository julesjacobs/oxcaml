module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module L = Wasm_locals
module T = Wasm_control
module Emit = Hmc_wasm_program_emit
module Continue = Wasm_control_branch_continue
module Fuel = Wasm_control_compose
let[@def] (zero @ total) (unit : unit) : B.u32 = 0
let[@def] (two @ total) (unit : unit) = C.Succ (C.Succ C.Zero)
let[@def] (four @ total) (unit : unit) = C.Succ (C.Succ (two ()))
let (write @ total) : (local : B.u32) -> (value : B.u32) -> (tail : T.code) @ immutable -> (labels : T.labels) @ immutable ->
    (before : X.state) @ immutable -> {u : unit | L.can_set before.X.machine.E.locals local (S.I32 value)} ->
    {after : X.state | after.X.memory === before.X.memory && after.X.machine.E.stack === before.X.machine.E.stack
      && L.replaced before.X.machine.E.locals local (S.I32 value) after.X.machine.E.locals
      && L.get after.X.machine.E.locals local === Some (S.I32 value)
      && T.run (two ()) {T.code = Emit.status local value tail; labels; state = before} === T.Running {T.code = tail; labels; state = after}} @ immutable =
  fun local value tail labels before premise ->
    match L.set before.X.machine.E.locals local (S.I32 value) with
    | None -> unreachable_ ()
    | Some locals ->
      let after = {X.memory = before.X.memory; machine = {E.locals; stack = before.X.machine.E.stack}} in
      ghost_ (
        let pushed = {X.memory = before.X.memory; machine = {E.locals = before.X.machine.E.locals; stack = S.Push (S.I32 value, before.X.machine.E.stack)}} in
        let first = {T.code = Emit.status local value tail; labels; state = before} in
        let second = {T.code = T.Instruction (I.Local_set local, tail); labels; state = pushed} in
        Emit.status_def local value tail; two_def ();
        T.run_def (two ()) first; T.step_def first; X.step_def (I.I32_const value) before; E.step_def (I.I32_const value) before.X.machine; S.step_def (I.I32_const value) before.X.machine.E.stack;
        T.run_def (C.Succ C.Zero) second; T.step_def second; X.step_def (I.Local_set local) pushed; E.step_def (I.Local_set local) pushed.X.machine;
        T.run_def C.Zero {T.code = tail; labels; state = after});
      after
let[@def] (scope @ total) (local : B.u32) (outer : T.labels @ immutable) =
  Continue.labels (Emit.status local (zero ()) T.Empty) (Continue.labels T.Empty outer)
let (prepare @ total) : (local : B.u32) -> (failure : B.u32) -> (body : T.code) @ immutable -> (outer : T.labels) @ immutable ->
    (before : X.state) @ immutable -> {u : unit | before.X.machine.E.stack === S.Empty && L.can_set before.X.machine.E.locals local (S.I32 failure)} ->
    {after : X.state | after.X.memory === before.X.memory && after.X.machine.E.stack === S.Empty
      && L.replaced before.X.machine.E.locals local (S.I32 failure) after.X.machine.E.locals
      && L.get after.X.machine.E.locals local === Some (S.I32 failure)
      && T.run (four ()) {T.code = Emit.protected local failure body; labels = outer; state = before}
        === T.Running {T.code = body; labels = scope local outer; state = after}} @ immutable =
  fun local failure body outer before premise ->
    let finish = Emit.status local (zero ()) T.Empty in
    let outside = Continue.labels T.Empty outer in
    let inner = T.Block (body, finish) in
    let after = write local failure inner outside before () in
    ghost_ (
      zero_def (); scope_def local outer; four_def (); two_def ();
      Emit.protected_def local failure body;
      Continue.labels_def T.Empty outer; Continue.labels_def finish outside;
      let start = {T.code = Emit.protected local failure body; labels = outer; state = before} in
      let setting = {T.code = Emit.status local failure inner; labels = outside; state = before} in
      let entering = {T.code = inner; labels = outside; state = after} in
      T.run_def (four ()) start; T.step_def start;
      T.enter_def (Emit.status local failure inner) T.Empty None start;
      T.stack_def before S.Empty;
      Fuel.correct (two ()) (C.Succ C.Zero) setting;
      Fuel.add_def (two ()) (C.Succ C.Zero); Fuel.add_def (C.Succ C.Zero) (C.Succ C.Zero); Fuel.add_def C.Zero (C.Succ C.Zero);
      T.run_def (C.Succ C.Zero) entering; T.step_def entering; T.enter_def body finish None entering;
      T.stack_def after S.Empty;
      T.run_def C.Zero {T.code = body; labels = scope local outer; state = after});
    after
let (finish @ total) : (local : B.u32) -> (outer : T.labels) @ immutable -> (before : X.state) @ immutable ->
    {u : unit | before.X.machine.E.stack === S.Empty && L.can_set before.X.machine.E.locals local (S.I32 (zero ()))} ->
    {after : X.state | after.X.memory === before.X.memory && after.X.machine.E.stack === S.Empty
      && L.replaced before.X.machine.E.locals local (S.I32 (zero ())) after.X.machine.E.locals
      && L.get after.X.machine.E.locals local === Some (S.I32 (zero ()))
      && T.run (four ()) {T.code = T.Empty; labels = scope local outer; state = before}
        === T.Running {T.code = T.Empty; labels = outer; state = after}} @ immutable =
  fun local outer before premise ->
    let outside = Continue.labels T.Empty outer in
    let after = write local (zero ()) T.Empty outside before () in
    ghost_ (
      scope_def local outer; four_def (); two_def ();
      Continue.labels_def (Emit.status local (zero ()) T.Empty) outside;
      Continue.labels_def T.Empty outer;
      let start = {T.code = T.Empty; labels = scope local outer; state = before} in
      let setting = {T.code = Emit.status local (zero ()) T.Empty; labels = outside; state = before} in
      let exiting = {T.code = T.Empty; labels = outside; state = after} in
      T.run_def (four ()) start; T.step_def start; T.stack_def before S.Empty;
      Fuel.correct (two ()) (C.Succ C.Zero) setting;
      Fuel.add_def (two ()) (C.Succ C.Zero); Fuel.add_def (C.Succ C.Zero) (C.Succ C.Zero); Fuel.add_def C.Zero (C.Succ C.Zero);
      T.run_def (C.Succ C.Zero) exiting; T.step_def exiting; T.stack_def after S.Empty;
      T.run_def C.Zero {T.code = T.Empty; labels = outer; state = after});
    after
let[@def] (cost @ total) (body : C.count @ immutable) = Fuel.add (four ()) (Fuel.add body (four ()))
let (normal @ total) : (local : B.u32) -> (failure : B.u32) -> (body : T.code) @ immutable -> (outer : T.labels) @ immutable ->
    (before : X.state) @ immutable -> (prepared : X.state) @ immutable -> (after_body : X.state) @ immutable -> (body_fuel : C.count) @ immutable ->
    {u : unit | T.run (four ()) {T.code = Emit.protected local failure body; labels = outer; state = before}
        === T.Running {T.code = body; labels = scope local outer; state = prepared}
      && T.run body_fuel {T.code = body; labels = scope local outer; state = prepared}
        === T.Running {T.code = T.Empty; labels = scope local outer; state = after_body}
      && after_body.X.machine.E.stack === S.Empty && L.can_set after_body.X.machine.E.locals local (S.I32 (zero ()))} ->
    {after : X.state | after.X.memory === after_body.X.memory && after.X.machine.E.stack === S.Empty
      && L.replaced after_body.X.machine.E.locals local (S.I32 (zero ())) after.X.machine.E.locals
      && L.get after.X.machine.E.locals local === Some (S.I32 (zero ()))
      && T.run (cost body_fuel) {T.code = Emit.protected local failure body; labels = outer; state = before}
        === T.Running {T.code = T.Empty; labels = outer; state = after}} @ immutable =
  fun local failure body outer before prepared after_body body_fuel premise ->
    let after = finish local outer after_body () in
    ghost_ (
      cost_def body_fuel;
      Fuel.correct body_fuel (four ()) {T.code = body; labels = scope local outer; state = prepared};
      Fuel.correct (four ()) (Fuel.add body_fuel (four ())) {T.code = Emit.protected local failure body; labels = outer; state = before});
    after
let[@def] (exit_depth @ total) (unit : unit) : B.u32 = 2
let[@def] (escape_cost @ total) (body : C.count @ immutable) = Fuel.add (four ()) (Fuel.add body (C.Succ C.Zero))
let (exhausted @ total) : (local : B.u32) -> (failure : B.u32) -> (body : T.code) @ immutable -> (outer : T.labels) @ immutable ->
    (before : X.state) @ immutable -> (prepared : X.state) @ immutable -> (after_body : X.state) @ immutable ->
    (guard : T.label) @ immutable -> (body_fuel : C.count) @ immutable ->
    {u : unit | T.run (four ()) {T.code = Emit.protected local failure body; labels = outer; state = before}
        === T.Running {T.code = body; labels = scope local outer; state = prepared}
      && T.run body_fuel {T.code = body; labels = scope local outer; state = prepared}
        === T.Running {T.code = T.Instruction (I.Br (exit_depth ()), T.Empty); labels = T.Label (guard, scope local outer); state = after_body}
      && L.get after_body.X.machine.E.locals local === Some (S.I32 failure)} ->
    {u : unit | T.run (escape_cost body_fuel) {T.code = Emit.protected local failure body; labels = outer; state = before}
        === T.Running {T.code = T.Empty; labels = outer; state = T.stack after_body S.Empty}
      && L.get (T.stack after_body S.Empty).X.machine.E.locals local === Some (S.I32 failure)} @ ghost =
  fun local failure body outer before prepared after_body guard body_fuel premise -> ghost_ (
    scope_def local outer; exit_depth_def (); zero_def ();
    let outside = Continue.labels T.Empty outer in
    Continue.labels_def (Emit.status local (zero ()) T.Empty) outside;
    Continue.labels_def T.Empty outer;
    let escape = {T.code = T.Instruction (I.Br (exit_depth ()), T.Empty); labels = T.Label (guard, scope local outer); state = after_body} in
    T.run_def (C.Succ C.Zero) escape; T.step_def escape;
    T.branch_def 2 escape.T.labels after_body;
    T.branch_def 1 (scope local outer) after_body;
    T.branch_def 0 outside after_body;
    T.stack_def after_body S.Empty;
    T.run_def C.Zero {T.code = T.Empty; labels = outer; state = T.stack after_body S.Empty};
    Fuel.correct body_fuel (C.Succ C.Zero) {T.code = body; labels = scope local outer; state = prepared};
    Fuel.correct (four ()) (Fuel.add body_fuel (C.Succ C.Zero)) {T.code = Emit.protected local failure body; labels = outer; state = before};
    escape_cost_def body_fuel)
let (straight @ total) : (local : B.u32) -> (failure : B.u32) -> (body : C.t) @ immutable -> (outer : T.labels) @ immutable ->
    (before : X.state) @ immutable -> (prepared : X.state) @ immutable -> (after_body : X.state) @ immutable ->
    {u : unit | T.run (four ()) {T.code = Emit.protected local failure (Wasm_control_lift.embed body T.Empty); labels = outer; state = before}
        === T.Running {T.code = Wasm_control_lift.embed body T.Empty; labels = scope local outer; state = prepared}
      && X.run body prepared === X.Done after_body
      && after_body.X.machine.E.stack === S.Empty && L.can_set after_body.X.machine.E.locals local (S.I32 (zero ()))} ->
    {after : X.state | after.X.memory === after_body.X.memory && after.X.machine.E.stack === S.Empty
      && L.get after.X.machine.E.locals local === Some (S.I32 (zero ()))
      && L.replaced after_body.X.machine.E.locals local (S.I32 (zero ())) after.X.machine.E.locals
      && T.run (cost (C.length body)) {T.code = Emit.protected local failure (Wasm_control_lift.embed body T.Empty); labels = outer; state = before}
        === T.Running {T.code = T.Empty; labels = outer; state = after}} @ immutable =
  fun local failure body outer before prepared after_body premise ->
    ghost_ (Wasm_control_success.straight body prepared after_body ();
      Wasm_control_lift.correct body T.Empty (scope local outer) prepared after_body ());
    normal local failure (Wasm_control_lift.embed body T.Empty) outer before prepared after_body (C.length body) ()
