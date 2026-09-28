module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module L = Wasm_locals
module E = Wasm_execution
module X = Wasm_memory_execution
module Header = Hmc_wasm_header_update
let[@def] (emit @ total) (source : B.u32) (destination : B.u32) =
  C.Next (I.Local_get source, C.Next (I.Plain I.I32_wrap_i64, C.Next (I.Local_set destination, C.Empty)))
let (correct @ total) : (source : B.u32) -> (destination : B.u32) ->
    (state : X.state) @ immutable -> (address : B.u32) ->
    {u : unit | L.get state.X.machine.E.locals source === Some (S.I64 (Header.number address))
      && L.can_set state.X.machine.E.locals destination (S.I32 address)} ->
    {out : S.stack | X.run (emit source destination) state === X.Done {X.memory = state.X.memory; machine = {E.locals = out; stack = state.X.machine.E.stack}}
      && L.get out destination === Some (S.I32 address)
      && L.same_types state.X.machine.E.locals out && L.replaced state.X.machine.E.locals destination (S.I32 address) out} @ immutable =
  fun source destination state address premise ->
    match L.set state.X.machine.E.locals destination (S.I32 address) with
    | None -> unreachable_ ()
    | Some out ->
      ghost_ (
        let locals = state.X.machine.E.locals in let stack = state.X.machine.E.stack in
        let s2 = {X.memory = state.X.memory; machine = {E.locals; stack = S.Push (S.I64 (Header.number address), stack)}} in
        let s3 = {X.memory = state.X.memory; machine = {E.locals; stack = S.Push (S.I32 address, stack)}} in
        let c3 = C.Next (I.Local_set destination, C.Empty) in
        let c2 = C.Next (I.Plain I.I32_wrap_i64, c3) in
        emit_def source destination;
        X.run_def (emit source destination) state; X.step_def (I.Local_get source) state;
        E.step_def (I.Local_get source) state.X.machine;
        X.run_def c2 s2; X.step_def (I.Plain I.I32_wrap_i64) s2;
        E.step_def (I.Plain I.I32_wrap_i64) s2.X.machine; S.step_def (I.Plain I.I32_wrap_i64) s2.X.machine.E.stack;
        Header.number_def address;
        X.run_def c3 s3; X.step_def (I.Local_set destination) s3; E.step_def (I.Local_set destination) s3.X.machine;
        X.run_def C.Empty {X.memory = state.X.memory; machine = {E.locals = out; stack}});
      out
