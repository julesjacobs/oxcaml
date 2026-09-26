module B = Wasm_u32
module W = Hmc_word64
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module M = Wasm_memory
module Header = Hmc_wasm_header_update
let[@def] (emit @ total) (label : W.limb) (code_local : B.u32) =
  C.Next (I.Local_get code_local, C.Next (I.I32_const label, C.Next (I.Plain I.I32_eq, C.Empty)))
let (correct @ total) : (label : W.limb) -> (pc : W.limb) -> (code_local : B.u32) ->
    (state : X.state) @ immutable ->
    {u : unit | Wasm_locals.get state.X.machine.E.locals code_local === Some (S.I32 pc)} ->
    {u : unit | X.run (emit label code_local) state === X.Done {X.memory = state.X.memory;
      machine = {E.locals = state.X.machine.E.locals; stack = S.Push (S.I32 (S.boolean (pc = label)), state.X.machine.E.stack)}}} @ ghost =
  fun label pc code_local state premise -> ghost_ (
    emit_def label code_local; Hmc_wasm_pc_update.offset_def (); Header.number_def pc; Header.number_def label; W.equal_def (Header.number pc) (Header.number label);
    let s2 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 pc, state.X.machine.E.stack)}} in
    let s3 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 label, s2.X.machine.E.stack)}} in
    let s4 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 (S.boolean (pc = label)), state.X.machine.E.stack)}} in
    let c3 = C.Next (I.Plain I.I32_eq, C.Empty) in
    let c2 = C.Next (I.I32_const label, c3) in
    X.run_def (emit label code_local) state; X.step_def (I.Local_get code_local) state; E.step_def (I.Local_get code_local) state.X.machine;
    X.run_def c2 s2; X.step_def (I.I32_const label) s2;
    E.step_def (I.I32_const label) s2.X.machine; S.step_def (I.I32_const label) s2.X.machine.E.stack;
    X.run_def c3 s3; X.step_def (I.Plain I.I32_eq) s3; E.step_def (I.Plain I.I32_eq) s3.X.machine;
    S.step_def (I.Plain I.I32_eq) s3.X.machine.E.stack;
    X.run_def C.Empty s4)
let (straight @ total) : (label : W.limb) -> (code_local : B.u32) ->
    {u : unit | Wasm_control_lift.straight (emit label code_local)} @ ghost = fun label code_local -> ghost_ (
    emit_def label code_local;
    let c3 = C.Next (I.Plain I.I32_eq, C.Empty) in
    let c2 = C.Next (I.I32_const label, c3) in
    Wasm_control_lift.straight_def (emit label code_local); Wasm_control_lift.ordinary_def (I.Local_get code_local);
    Wasm_control_lift.straight_def c2; Wasm_control_lift.ordinary_def (I.I32_const label);
    Wasm_control_lift.straight_def c3; Wasm_control_lift.ordinary_def (I.Plain I.I32_eq);
    Wasm_control_lift.straight_def C.Empty)
