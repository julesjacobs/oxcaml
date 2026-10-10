module B = Wasm_u32
module W = Hmc_word64
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module L = Wasm_locals
module E = Wasm_execution
module O = Wasm_word_lowering

let (lower @ total) : (operation : O.operation) @ immutable ->
    (left : B.u32) -> (right : B.u32) -> (destination : B.u32) ->
    (state : E.state) @ ghost -> (a : W.t) @ ghost -> (b : W.t) @ ghost ->
    {u : unit | L.get state.E.locals left === Some (S.I64 a)
      && L.get state.E.locals right === Some (S.I64 b)
      && L.can_set state.E.locals destination (O.evaluate operation a b)} ->
    {code : C.t | match E.run code state with
      | E.Done after -> after.E.stack === state.E.stack
        && L.get after.E.locals destination === Some (O.evaluate operation a b)
        && L.same_types state.E.locals after.E.locals
        && L.replaced state.E.locals destination (O.evaluate operation a b) after.E.locals
      | _ -> false} @ immutable =
  fun operation left right destination state a b premise ->
    let op = I.Plain (O.opcode operation) in
    let store = C.Next (I.Local_set destination, C.Empty) in
    let compute = C.Next (op, store) in
    let second = C.Next (I.Local_get right, compute) in
    let code = C.Next (I.Local_get left, second) in
    ghost_ (
      let first_state = {E.locals = state.E.locals; stack = S.Push (S.I64 a, state.E.stack)} in
      let operands = {E.locals = state.E.locals; stack = S.Push (S.I64 b, first_state.E.stack)} in
      let value = O.evaluate operation a b in
      let computed = {E.locals = state.E.locals; stack = S.Push (value, state.E.stack)} in
      O.opcode_def operation; O.evaluate_def operation a b;
      E.run_def code state; E.step_def (I.Local_get left) state;
      E.run_def second first_state; E.step_def (I.Local_get right) first_state;
      E.run_def compute operands; E.step_def op operands; S.step_def op operands.E.stack;
      E.run_def store computed; E.step_def (I.Local_set destination) computed;
      match L.set state.E.locals destination value with
      | None -> ()
      | Some locals -> E.run_def C.Empty {E.locals; stack = state.E.stack});
    code
