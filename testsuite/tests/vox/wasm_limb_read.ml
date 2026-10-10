module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module L = Wasm_locals
module E = Wasm_execution
module X = Wasm_memory_execution
module M = Wasm_memory
module Header = Hmc_wasm_header_update
let[@def] (emit @ total) (offset : B.u32) (base_local : B.u32) (destination : B.u32) =
  C.Next (I.Local_get base_local, C.Next (I.I32_load (2, offset),
    C.Next (I.Local_set destination, C.Empty)))
let (correct @ total) : (offset : B.u32) -> (base_local : B.u32) -> (destination : B.u32) ->
    (state : X.state) @ immutable -> (base : B.u32) -> (address : B.u32) ->
    {u : unit | L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && M.load state.X.memory base offset M.W32 === Some (S.I32 address)
      && L.can_set state.X.machine.E.locals destination (S.I32 address)} ->
    {out : S.stack | X.run (emit offset base_local destination) state === X.Done {X.memory = state.X.memory; machine = {E.locals = out; stack = state.X.machine.E.stack}}
      && L.get out destination === Some (S.I32 address)
      && L.same_types state.X.machine.E.locals out && L.replaced state.X.machine.E.locals destination (S.I32 address) out} @ immutable =
  fun offset base_local destination state base address premise ->
    match L.set state.X.machine.E.locals destination (S.I32 address) with
    | None -> unreachable_ ()
    | Some out ->
      ghost_ (
        let locals = state.X.machine.E.locals in let stack = state.X.machine.E.stack in
        let s1 = {X.memory = state.X.memory; machine = {E.locals; stack = S.Push (S.I32 base, stack)}} in
        let s3 = {X.memory = state.X.memory; machine = {E.locals; stack = S.Push (S.I32 address, stack)}} in
        let c3 = C.Next (I.Local_set destination, C.Empty) in
        let c1 = C.Next (I.I32_load (2, offset), c3) in
        emit_def offset base_local destination;
        X.run_def (emit offset base_local destination) state; X.step_def (I.Local_get base_local) state;
        E.step_def (I.Local_get base_local) state.X.machine;
        X.run_def c1 s1; X.step_def (I.I32_load (2, offset)) s1; X.read_def M.W32 offset s1;
        X.run_def c3 s3; X.step_def (I.Local_set destination) s3; E.step_def (I.Local_set destination) s3.X.machine;
        X.run_def C.Empty {X.memory = state.X.memory; machine = {E.locals = out; stack}});
      out
