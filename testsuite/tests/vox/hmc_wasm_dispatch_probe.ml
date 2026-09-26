module B = Wasm_u32
module W = Hmc_word64
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module M = Wasm_memory
module Header = Hmc_wasm_header_update
let[@def] (emit @ total) (label : W.limb) (base_local : B.u32) =
  C.Next (I.Local_get base_local, C.Next (I.I64_load (3, 8), C.Next (I.I64_const (Header.number label), C.Next (I.Plain I.I64_eq, C.Empty))))
let (correct @ total) : (label : W.limb) -> (pc : W.limb) -> (base_local : B.u32) ->
    (base : B.u32) -> (state : X.state) @ immutable ->
    {u : unit | Wasm_locals.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && M.load state.X.memory base (Hmc_wasm_pc_update.offset ()) M.W64 === Some (S.I64 (Header.number pc))} ->
    {u : unit | X.run (emit label base_local) state === X.Done {X.memory = state.X.memory;
      machine = {E.locals = state.X.machine.E.locals; stack = S.Push (S.I32 (S.boolean (pc = label)), state.X.machine.E.stack)}}} @ ghost =
  fun label pc base_local base state premise -> ghost_ (
    emit_def label base_local; Hmc_wasm_pc_update.offset_def (); Header.number_def pc; Header.number_def label; W.equal_def (Header.number pc) (Header.number label);
    let s1 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 base, state.X.machine.E.stack)}} in
    let s2 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I64 (Header.number pc), state.X.machine.E.stack)}} in
    let s3 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I64 (Header.number label), s2.X.machine.E.stack)}} in
    let s4 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 (S.boolean (pc = label)), state.X.machine.E.stack)}} in
    let c3 = C.Next (I.Plain I.I64_eq, C.Empty) in
    let c2 = C.Next (I.I64_const (Header.number label), c3) in
    let c1 = C.Next (I.I64_load (3, 8), c2) in
    X.run_def (emit label base_local) state; X.step_def (I.Local_get base_local) state; E.step_def (I.Local_get base_local) state.X.machine;
    X.run_def c1 s1; X.step_def (I.I64_load (3, 8)) s1; X.read_def M.W64 8 s1;
    X.run_def c2 s2; X.step_def (I.I64_const (Header.number label)) s2;
    E.step_def (I.I64_const (Header.number label)) s2.X.machine; S.step_def (I.I64_const (Header.number label)) s2.X.machine.E.stack;
    X.run_def c3 s3; X.step_def (I.Plain I.I64_eq) s3; E.step_def (I.Plain I.I64_eq) s3.X.machine;
    S.step_def (I.Plain I.I64_eq) s3.X.machine.E.stack;
    X.run_def C.Empty s4)
let (straight @ total) : (label : W.limb) -> (base_local : B.u32) ->
    {u : unit | Wasm_control_lift.straight (emit label base_local)} @ ghost = fun label base_local -> ghost_ (
    emit_def label base_local;
    let c3 = C.Next (I.Plain I.I64_eq, C.Empty) in
    let c2 = C.Next (I.I64_const (Header.number label), c3) in
    let c1 = C.Next (I.I64_load (3, 8), c2) in
    Wasm_control_lift.straight_def (emit label base_local); Wasm_control_lift.ordinary_def (I.Local_get base_local);
    Wasm_control_lift.straight_def c1; Wasm_control_lift.ordinary_def (I.I64_load (3, 8));
    Wasm_control_lift.straight_def c2; Wasm_control_lift.ordinary_def (I.I64_const (Header.number label));
    Wasm_control_lift.straight_def c3; Wasm_control_lift.ordinary_def (I.Plain I.I64_eq);
    Wasm_control_lift.straight_def C.Empty)
