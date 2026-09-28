module B = Wasm_u32
module W = Hmc_word64
module V = Hmc_tagged_cell
module H = Hmc_wasm_header_update
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module M = Wasm_memory
let[@def] (payload_offset @ total) (unit : unit) : B.u32 = 40
let[@def] (emit @ total) (yes : W.limb) (no : W.limb) (base_local : B.u32) : C.t @ immutable =
  C.Next (I.Local_get base_local, C.Next (I.I64_const (H.number yes), C.Next (I.I64_const (H.number no), C.Next (I.Local_get base_local, C.Next (I.I64_load (3, 40), C.Next (I.Plain I.I32_wrap_i64, C.Next (I.Plain I.Select, C.Next (I.I64_store (3, 8), C.Empty))))))))
let (correct @ total) : (yes : W.limb) -> (no : W.limb) -> (base_local : B.u32) ->
    (condition : bool) -> (state : X.state) @ immutable -> (base : B.u32) -> (after : B.bytes) @ immutable ->
    {u : unit | Wasm_locals.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && M.load state.X.memory base (payload_offset ()) M.W64 === Some (S.I64 (V.payload (V.Boolean condition)))
      && M.store state.X.memory base (Hmc_wasm_pc_update.offset ()) (S.I64 (H.number (if condition then yes else no))) === Some after} ->
    {u : unit | X.run (emit yes no base_local) state === X.Done {X.memory = after; machine = state.X.machine}} @ ghost =
  fun yes no base_local condition state base after premise -> ghost_ (
    emit_def yes no base_local; payload_offset_def (); Hmc_wasm_pc_update.offset_def ();
    V.payload_def (V.Boolean condition); S.boolean_def condition;
    let chosen = H.number (if condition then yes else no) in
    let s0 = state in
    let s1 = {X.memory = state.X.memory; machine = {E.locals = state.X.machine.E.locals; stack = S.Push (S.I32 base, state.X.machine.E.stack)}} in
    let s2 = {X.memory = state.X.memory; machine = {E.locals = state.X.machine.E.locals; stack = S.Push (S.I64 (H.number yes), s1.X.machine.E.stack)}} in
    let s3 = {X.memory = state.X.memory; machine = {E.locals = state.X.machine.E.locals; stack = S.Push (S.I64 (H.number no), s2.X.machine.E.stack)}} in
    let s4 = {X.memory = state.X.memory; machine = {E.locals = state.X.machine.E.locals; stack = S.Push (S.I32 base, s3.X.machine.E.stack)}} in
    let s5 = {X.memory = state.X.memory; machine = {E.locals = state.X.machine.E.locals; stack = S.Push (S.I64 (V.payload (V.Boolean condition)), s3.X.machine.E.stack)}} in
    let s6 = {X.memory = state.X.memory; machine = {E.locals = state.X.machine.E.locals; stack = S.Push (S.I32 (S.boolean condition), s3.X.machine.E.stack)}} in
    let s7 = {X.memory = state.X.memory; machine = {E.locals = state.X.machine.E.locals; stack = S.Push (S.I64 chosen, s1.X.machine.E.stack)}} in
    let c8 = C.Empty in
    let c7 = C.Next (I.I64_store (3, 8), c8) in
    let c6 = C.Next (I.Plain I.Select, c7) in
    let c5 = C.Next (I.Plain I.I32_wrap_i64, c6) in
    let c4 = C.Next (I.I64_load (3, 40), c5) in
    let c3 = C.Next (I.Local_get base_local, c4) in
    let c2 = C.Next (I.I64_const (H.number no), c3) in
    let c1 = C.Next (I.I64_const (H.number yes), c2) in
    let c0 = C.Next (I.Local_get base_local, c1) in
    X.run_def c0 s0; X.step_def (I.Local_get base_local) s0;
    E.step_def (I.Local_get base_local) s0.X.machine;
    X.run_def c1 s1; X.step_def (I.I64_const (H.number yes)) s1;
    E.step_def (I.I64_const (H.number yes)) s1.X.machine;
    S.step_def (I.I64_const (H.number yes)) s1.X.machine.E.stack;
    X.run_def c2 s2; X.step_def (I.I64_const (H.number no)) s2;
    E.step_def (I.I64_const (H.number no)) s2.X.machine;
    S.step_def (I.I64_const (H.number no)) s2.X.machine.E.stack;
    X.run_def c3 s3; X.step_def (I.Local_get base_local) s3;
    E.step_def (I.Local_get base_local) s3.X.machine;
    X.run_def c4 s4; X.step_def (I.I64_load (3, 40)) s4;
    X.read_def M.W64 40 s4;
    X.run_def c5 s5; X.step_def (I.Plain I.I32_wrap_i64) s5;
    E.step_def (I.Plain I.I32_wrap_i64) s5.X.machine;
    S.step_def (I.Plain I.I32_wrap_i64) s5.X.machine.E.stack;
    X.run_def c6 s6; X.step_def (I.Plain I.Select) s6;
    E.step_def (I.Plain I.Select) s6.X.machine;
    S.step_def (I.Plain I.Select) s6.X.machine.E.stack;
    S.same_type_def (S.I64 (H.number yes)) (S.I64 (H.number no));
    X.run_def c7 s7; X.step_def (I.I64_store (3, 8)) s7;
    M.width_def (S.I64 chosen); X.compatible_def (S.I64 chosen) M.W64; X.write_def M.W64 8 s7;
    X.run_def C.Empty {X.memory = after; machine = state.X.machine})
