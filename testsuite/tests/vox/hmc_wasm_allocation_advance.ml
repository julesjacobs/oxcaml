module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module L = Wasm_locals
let[@def] (emit @ total) (bytes : B.u32) (cursor_local : B.u32) =
  C.Next (I.Local_get cursor_local, C.Next (I.I32_const bytes, C.Next (I.Plain I.I32_add, C.Next (I.Local_set cursor_local, C.Empty))))
let (correct @ total) : (bytes : B.u32) -> (cursor_local : B.u32) -> (cursor : B.u32) -> (limit : B.u32) -> (state : X.state) @ immutable ->
    {u : unit | cursor + bytes <= limit && L.get state.X.machine.E.locals cursor_local === Some (S.I32 cursor)} ->
    {out : S.stack | S.add32 cursor bytes = cursor + bytes && L.replaced state.X.machine.E.locals cursor_local (S.I32 (S.add32 cursor bytes)) out
      && L.get out cursor_local === Some (S.I32 (S.add32 cursor bytes)) && L.same_types state.X.machine.E.locals out
      && X.run (emit bytes cursor_local) state === X.Done {X.memory = state.X.memory; machine = {E.locals = out; stack = state.X.machine.E.stack}}} @ immutable =
  fun bytes cursor_local cursor limit state premise ->
    ghost_ (S.add32_def cursor bytes; L.can_set_def state.X.machine.E.locals cursor_local (S.I32 (S.add32 cursor bytes)); S.same_type_def (S.I32 cursor) (S.I32 (S.add32 cursor bytes)));
    match L.set state.X.machine.E.locals cursor_local (S.I32 (S.add32 cursor bytes)) with
    | None -> unreachable_ ()
    | Some out ->
      ghost_ (emit_def bytes cursor_local; S.add32_def cursor bytes;
        let s1 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 cursor, state.X.machine.E.stack)}} in
        let s2 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 bytes, s1.X.machine.E.stack)}} in
        let s3 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 (S.add32 cursor bytes), state.X.machine.E.stack)}} in
        let final = {X.memory = state.X.memory; machine = {E.locals = out; stack = state.X.machine.E.stack}} in
        let c3 = C.Next (I.Local_set cursor_local, C.Empty) in
        let c2 = C.Next (I.Plain I.I32_add, c3) in
        let c1 = C.Next (I.I32_const bytes, c2) in
        X.run_def (emit bytes cursor_local) state; X.step_def (I.Local_get cursor_local) state; E.step_def (I.Local_get cursor_local) state.X.machine;
        X.run_def c1 s1; X.step_def (I.I32_const bytes) s1; E.step_def (I.I32_const bytes) s1.X.machine; S.step_def (I.I32_const bytes) s1.X.machine.E.stack;
        X.run_def c2 s2; X.step_def (I.Plain I.I32_add) s2; E.step_def (I.Plain I.I32_add) s2.X.machine; S.step_def (I.Plain I.I32_add) s2.X.machine.E.stack;
        X.run_def c3 s3; X.step_def (I.Local_set cursor_local) s3; E.step_def (I.Local_set cursor_local) s3.X.machine;
        X.run_def C.Empty final);
      out
