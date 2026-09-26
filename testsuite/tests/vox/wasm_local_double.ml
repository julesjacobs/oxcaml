module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module L = Wasm_locals
let[@def] (emit @ total) (source : B.u32) (destination : B.u32) =
  C.Next (I.Local_get source, C.Next (I.Local_get source, C.Next (I.Plain I.I32_add, C.Next (I.Local_set destination, C.Empty))))
let (correct @ total) : (source : B.u32) -> (destination : B.u32) -> (cursor : B.u32) -> (state : X.state) @ immutable ->
    {u : unit | cursor <= 2147483647 && L.get state.X.machine.E.locals source === Some (S.I32 cursor)
      && L.can_set state.X.machine.E.locals destination (S.I32 (S.add32 cursor cursor))} ->
    {out : S.stack | S.add32 cursor cursor = cursor + cursor && L.replaced state.X.machine.E.locals destination (S.I32 (S.add32 cursor cursor)) out
      && L.get out destination === Some (S.I32 (S.add32 cursor cursor)) && L.same_types state.X.machine.E.locals out
      && X.run (emit source destination) state === X.Done {X.memory = state.X.memory; machine = {E.locals = out; stack = state.X.machine.E.stack}}} @ immutable =
  fun source destination cursor state premise ->
    ghost_ (S.add32_def cursor cursor; L.can_set_def state.X.machine.E.locals destination (S.I32 (S.add32 cursor cursor)); S.same_type_def (S.I32 cursor) (S.I32 (S.add32 cursor cursor)));
    match L.set state.X.machine.E.locals destination (S.I32 (S.add32 cursor cursor)) with
    | None -> unreachable_ ()
    | Some out ->
      ghost_ (emit_def source destination; S.add32_def cursor cursor;
        let s1 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 cursor, state.X.machine.E.stack)}} in
        let s2 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 cursor, s1.X.machine.E.stack)}} in
        let s3 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 (S.add32 cursor cursor), state.X.machine.E.stack)}} in
        let final = {X.memory = state.X.memory; machine = {E.locals = out; stack = state.X.machine.E.stack}} in
        let c3 = C.Next (I.Local_set destination, C.Empty) in
        let c2 = C.Next (I.Plain I.I32_add, c3) in
        let c1 = C.Next (I.Local_get source, c2) in
        X.run_def (emit source destination) state; X.step_def (I.Local_get source) state; E.step_def (I.Local_get source) state.X.machine;
        X.run_def c1 s1; X.step_def (I.Local_get source) s1; E.step_def (I.Local_get source) s1.X.machine;
        X.run_def c2 s2; X.step_def (I.Plain I.I32_add) s2; E.step_def (I.Plain I.I32_add) s2.X.machine; S.step_def (I.Plain I.I32_add) s2.X.machine.E.stack;
        X.run_def c3 s3; X.step_def (I.Local_set destination) s3; E.step_def (I.Local_set destination) s3.X.machine;
        X.run_def C.Empty final);
      out
