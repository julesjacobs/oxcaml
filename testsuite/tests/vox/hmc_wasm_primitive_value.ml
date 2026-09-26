module D = Hm_declarative
module W = Hmc_word64
module V = Hmc_tagged_cell
module Simple = Hmc_heap_simple
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
let[@def] (opcode @ total) (operation : D.word_operation @ immutable) = match operation with
  | D.Add -> I.I64_add | D.Subtract -> I.I64_sub | D.Equal_word -> I.I64_eq | D.Unsigned_less -> I.I64_lt_u
let[@def] (extension @ total) (operation : D.word_operation @ immutable) = match operation with
  | D.Add | D.Subtract -> C.Empty | _ -> C.Next (I.Plain I.I64_extend_i32_u, C.Empty)
let[@def] (emit @ total) (operation : D.word_operation @ immutable) = C.Next (I.Plain (opcode operation), extension operation)
let[@def] (tag @ total) (operation : D.word_operation @ immutable) : W.t @ immutable =
  {W.lo = (match operation with D.Add | D.Subtract -> 1 | _ -> 0); hi = 0}
let (correct @ total) : (operation : D.word_operation) @ immutable -> (left : W.t) @ immutable -> (right : W.t) @ immutable ->
    (state : X.state) @ immutable -> (rest : S.stack) @ immutable ->
    {u : unit | state.X.machine.E.stack === S.Push (S.I64 right, S.Push (S.I64 left, rest))} ->
    {u : unit | X.run (emit operation) state === X.Done {X.memory = state.X.memory; machine = {E.locals = state.X.machine.E.locals;
        stack = S.Push (S.I64 (V.payload (Simple.primitive operation left right)), rest)}}
      && tag operation === V.tag (Simple.primitive operation left right)} @ ghost = fun operation left right state rest premise -> ghost_ (
    emit_def operation; opcode_def operation; extension_def operation; Simple.primitive_def operation left right;
    V.payload_def (Simple.primitive operation left right); V.tag_def (Simple.primitive operation left right); tag_def operation;
    X.run_def (emit operation) state; X.step_def (I.Plain (opcode operation)) state;
    E.step_def (I.Plain (opcode operation)) state.X.machine; S.step_def (I.Plain (opcode operation)) state.X.machine.E.stack;
    match operation with
    | D.Add | D.Subtract ->
      X.run_def C.Empty {state with X.machine = {state.X.machine with E.stack = S.Push (S.I64 (V.payload (Simple.primitive operation left right)), rest)}}
    | _ ->
      let condition = match operation with D.Equal_word -> W.equal left right | _ -> W.unsigned_less left right in
      S.boolean_def condition;
      let tested = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 (S.boolean condition), rest)}} in
      X.run_def (extension operation) tested; X.step_def (I.Plain I.I64_extend_i32_u) tested;
      E.step_def (I.Plain I.I64_extend_i32_u) tested.X.machine; S.step_def (I.Plain I.I64_extend_i32_u) tested.X.machine.E.stack;
      X.run_def C.Empty {state with X.machine = {state.X.machine with E.stack = S.Push (S.I64 (V.payload (Simple.primitive operation left right)), rest)}})
