module B = Wasm_u32
module W = Hmc_word64
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module Header = Hmc_wasm_header_update
let[@def] (emit @ total) (bytes : B.u32) (cursor_local : B.u32) (limit_local : B.u32) =
  C.Next (I.Local_get limit_local, C.Next (I.Plain I.I64_extend_i32_u,
  C.Next (I.Local_get cursor_local, C.Next (I.Plain I.I64_extend_i32_u,
  C.Next (I.I64_const (Header.number bytes), C.Next (I.Plain I.I64_add,
  C.Next (I.Plain I.I64_lt_u, C.Next (I.Plain I.I32_eqz, C.Empty))))))))
let (correct @ total) : (bytes : B.u32) -> (cursor_local : B.u32) -> (limit_local : B.u32) ->
    (cursor : B.u32) -> (limit : B.u32) -> (state : X.state) @ immutable ->
    {u : unit | Wasm_locals.get state.X.machine.E.locals cursor_local === Some (S.I32 cursor)
      && Wasm_locals.get state.X.machine.E.locals limit_local === Some (S.I32 limit)} ->
    {u : unit | X.run (emit bytes cursor_local limit_local) state === X.Done {X.memory = state.X.memory;
      machine = {E.locals = state.X.machine.E.locals; stack = S.Push (S.I32 (S.boolean (cursor + bytes <= limit)), state.X.machine.E.stack)}}} @ ghost =
  fun bytes cursor_local limit_local cursor limit state premise -> ghost_ (
    Header.number_def bytes; Header.number_def cursor; Header.number_def limit;
    let sum = W.add (Header.number cursor) (Header.number bytes) in
    W.add_def (Header.number cursor) (Header.number bytes);
    W.unsigned_less_def (Header.number limit) sum;
    S.boolean_def (cursor + bytes <= limit); S.boolean_def (W.unsigned_less (Header.number limit) sum);
    S.boolean_def (S.boolean (W.unsigned_less (Header.number limit) sum) = 0);
    emit_def bytes cursor_local limit_local;
    let s1 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 limit, state.X.machine.E.stack)}} in
    let s2 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I64 (Header.number limit), state.X.machine.E.stack)}} in
    let s3 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 cursor, s2.X.machine.E.stack)}} in
    let s4 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I64 (Header.number cursor), s2.X.machine.E.stack)}} in
    let s5 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I64 (Header.number bytes), s4.X.machine.E.stack)}} in
    let s6 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I64 sum, s2.X.machine.E.stack)}} in
    let s7 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 (S.boolean (W.unsigned_less (Header.number limit) sum)), state.X.machine.E.stack)}} in
    let s8 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 (S.boolean (cursor + bytes <= limit)), state.X.machine.E.stack)}} in
    let c7 = C.Next (I.Plain I.I32_eqz, C.Empty) in
    let c6 = C.Next (I.Plain I.I64_lt_u, c7) in
    let c5 = C.Next (I.Plain I.I64_add, c6) in
    let c4 = C.Next (I.I64_const (Header.number bytes), c5) in
    let c3 = C.Next (I.Plain I.I64_extend_i32_u, c4) in
    let c2 = C.Next (I.Local_get cursor_local, c3) in
    let c1 = C.Next (I.Plain I.I64_extend_i32_u, c2) in
    X.run_def (emit bytes cursor_local limit_local) state;
    X.step_def (I.Local_get limit_local) state; E.step_def (I.Local_get limit_local) state.X.machine;
    X.run_def c1 s1; X.step_def (I.Plain I.I64_extend_i32_u) s1; E.step_def (I.Plain I.I64_extend_i32_u) s1.X.machine; S.step_def (I.Plain I.I64_extend_i32_u) s1.X.machine.E.stack;
    X.run_def c2 s2; X.step_def (I.Local_get cursor_local) s2; E.step_def (I.Local_get cursor_local) s2.X.machine;
    X.run_def c3 s3; X.step_def (I.Plain I.I64_extend_i32_u) s3; E.step_def (I.Plain I.I64_extend_i32_u) s3.X.machine; S.step_def (I.Plain I.I64_extend_i32_u) s3.X.machine.E.stack;
    X.run_def c4 s4; X.step_def (I.I64_const (Header.number bytes)) s4; E.step_def (I.I64_const (Header.number bytes)) s4.X.machine; S.step_def (I.I64_const (Header.number bytes)) s4.X.machine.E.stack;
    X.run_def c5 s5; X.step_def (I.Plain I.I64_add) s5; E.step_def (I.Plain I.I64_add) s5.X.machine; S.step_def (I.Plain I.I64_add) s5.X.machine.E.stack;
    X.run_def c6 s6; X.step_def (I.Plain I.I64_lt_u) s6; E.step_def (I.Plain I.I64_lt_u) s6.X.machine; S.step_def (I.Plain I.I64_lt_u) s6.X.machine.E.stack;
    X.run_def c7 s7; X.step_def (I.Plain I.I32_eqz) s7; E.step_def (I.Plain I.I32_eqz) s7.X.machine; S.step_def (I.Plain I.I32_eqz) s7.X.machine.E.stack;
    X.run_def C.Empty s8)
let[@def] (width @ total) (number : Hmc_wasm_reservation.count) : B.u32 = 16 * number
let (source @ total) : (cells : Hm_declarative.index) @ immutable -> (number : Hmc_wasm_reservation.count) ->
    (cursor_local : B.u32) -> (limit_local : B.u32) -> (cursor : B.u32) -> (limit : B.u32) -> (state : X.state) @ immutable ->
    {u : unit | Hmc_u32_index.represents cells number
      && Wasm_locals.get state.X.machine.E.locals cursor_local === Some (S.I32 cursor)
      && Wasm_locals.get state.X.machine.E.locals limit_local === Some (S.I32 limit)} ->
    {u : unit | X.run (emit (width number) cursor_local limit_local) state === X.Done {X.memory = state.X.memory;
      machine = {E.locals = state.X.machine.E.locals; stack = S.Push (S.I32 (S.boolean (Hmc_heap_extent.fits cells cursor limit)), state.X.machine.E.stack)}}} @ ghost =
  fun cells number cursor_local limit_local cursor limit state premise -> ghost_ (
    width_def number; Hmc_wasm_reservation.fits cells number cursor limit (); correct (16 * number) cursor_local limit_local cursor limit state ())
