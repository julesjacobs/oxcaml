module W = Hmc_word64
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar

type operation = Add | Subtract | Equal | Unsigned_less [@@inductive]
let[@def] (opcode @ total) (operation : operation @ immutable) =
  match operation with
  | Add -> I.I64_add | Subtract -> I.I64_sub
  | Equal -> I.I64_eq | Unsigned_less -> I.I64_lt_u
let[@def] (evaluate @ total) (operation : operation @ immutable)
    (a : W.t @ immutable) (b : W.t @ immutable) : S.value @ immutable =
  match operation with
  | Add -> S.I64 (W.add a b)
  | Subtract -> S.I64 (W.subtract a b)
  | Equal -> S.I32 (S.boolean (W.equal a b))
  | Unsigned_less -> S.I32 (S.boolean (W.unsigned_less a b))

let (lower @ total) : (operation : operation) @ immutable ->
    (a : W.t) @ immutable -> (b : W.t) @ immutable ->
    (tail : C.t) @ immutable -> (stack : S.stack) @ ghost ->
    {code : C.t | S.run code stack === S.run tail (S.Push (evaluate operation a b, stack))} @ immutable =
  fun operation a b tail stack ->
    let instruction = I.Plain (opcode operation) in
    let last = C.Next (instruction, tail) in
    let second = C.Next (I.I64_const b, last) in
    let code = C.Next (I.I64_const a, second) in
    ghost_ (
      opcode_def operation; evaluate_def operation a b;
      S.run_def code stack; S.step_def (I.I64_const a) stack;
      S.run_def second (S.Push (S.I64 a, stack));
      S.step_def (I.I64_const b) (S.Push (S.I64 a, stack));
      S.run_def last (S.Push (S.I64 b, S.Push (S.I64 a, stack)));
      S.step_def instruction (S.Push (S.I64 b, S.Push (S.I64 a, stack))));
    code
