module B = Wasm_u32
module W = Hmc_word64
module I = Wasm_instruction

type value = I32 of B.u32 | I64 of W.t [@@inductive]
type stack = Empty | Push of value * stack [@@inductive]
type result = Done of stack | Type_error | Not_scalar [@@inductive]

let[@def] (boolean @ total) (b : bool) : B.u32 = if b then 1 else 0
let[@def] (add32 @ total) (a : B.u32) (b : B.u32) : B.u32 =
  let sum = a + b in if sum >= 4294967296 then sum - 4294967296 else sum
let[@def] (sub32 @ total) (a : B.u32) (b : B.u32) : B.u32 =
  let difference = a - b in
  if difference < 0 then difference + 4294967296 else difference
let[@def] (same_type @ total) (a : value @ immutable) (b : value @ immutable) =
  match a, b with I32 _, I32 _ | I64 _, I64 _ -> true | _ -> false

let[@def] (step @ total) (instruction : I.t @ immutable) (stack : stack @ immutable)
    : result @ immutable =
  match instruction with
  | I.I32_const n -> Done (Push (I32 n, stack))
  | I.I64_const n -> Done (Push (I64 n, stack))
  | I.Plain I.Nop -> Done stack
  | I.Plain I.Drop -> (match stack with Empty -> Type_error | Push (_, rest) -> Done rest)
  | I.Plain I.Select -> (match stack with
    | Push (I32 condition, Push (second, Push (first, rest))) ->
      if same_type first second then Done (Push ((if condition <> 0 then first else second), rest))
      else Type_error
    | _ -> Type_error)
  | I.Plain I.I32_eqz -> (match stack with
    | Push (I32 n, rest) -> Done (Push (I32 (boolean (n = 0)), rest)) | _ -> Type_error)
  | I.Plain I.I64_eqz -> (match stack with
    | Push (I64 n, rest) -> Done (Push (I32 (boolean (n.W.lo = 0 && n.W.hi = 0)), rest))
    | _ -> Type_error)
  | I.Plain (I.I32_add | I.I32_sub | I.I32_eq | I.I32_ne | I.I32_lt_u | I.I32_gt_u | I.I32_le_u | I.I32_ge_u) ->
    (match stack with
    | Push (I32 b, Push (I32 a, rest)) ->
      let result = match instruction with
        | I.Plain I.I32_add -> add32 a b
        | I.Plain I.I32_sub -> sub32 a b
        | I.Plain I.I32_eq -> boolean (a = b)
        | I.Plain I.I32_ne -> boolean (a <> b)
        | I.Plain I.I32_lt_u -> boolean (a < b)
        | I.Plain I.I32_gt_u -> boolean (a > b)
        | I.Plain I.I32_le_u -> boolean (a <= b)
        | _ -> boolean (a >= b)
      in Done (Push (I32 result, rest))
    | _ -> Type_error)
  | I.Plain (I.I64_add | I.I64_sub) -> (match stack with
    | Push (I64 b, Push (I64 a, rest)) ->
      let result = match instruction with I.Plain I.I64_add -> W.add a b | _ -> W.subtract a b in
      Done (Push (I64 result, rest))
    | _ -> Type_error)
  | I.Plain (I.I64_eq | I.I64_lt_u) -> (match stack with
    | Push (I64 b, Push (I64 a, rest)) ->
      let result = match instruction with I.Plain I.I64_eq -> W.equal a b | _ -> W.unsigned_less a b in
      Done (Push (I32 (boolean result), rest))
    | _ -> Type_error)
  | I.Plain I.I32_wrap_i64 -> (match stack with
    | Push (I64 n, rest) -> Done (Push (I32 n.W.lo, rest)) | _ -> Type_error)
  | I.Plain I.I64_extend_i32_u -> (match stack with
    | Push (I32 n, rest) -> Done (Push (I64 {W.lo = n; hi = 0}, rest)) | _ -> Type_error)
  | _ -> Not_scalar

let[@def] rec (run @ total) (code : Wasm_code.t @ immutable) (stack : stack @ immutable)
    : result @ immutable =
  match code with
  | Wasm_code.Empty -> Done stack
  | Wasm_code.Next (instruction, rest) ->
    match step instruction stack with
    | Done stack -> run rest stack
    | Type_error -> Type_error
    | Not_scalar -> Not_scalar
