module B = Wasm_u32
module I = Wasm_instruction
module F = Wasm_functions
module G = Wasm_globals
module S = Wasm_scalar

type value = I32 | I64 | Unknown [@@inductive]
type stack = Empty | Push of value * stack [@@inductive]
type state = {stack : stack; unreachable : bool}
type context = {module_ : F.module_; globals : G.t; locals : F.local_types; result : F.result_type}
let[@def] (compatible @ total) (left : value @ immutable) (right : value @ immutable) =
  match left, right with I32, I32 | I64, I64 | Unknown, _ | _, Unknown -> true | _ -> false
let[@def] (pop @ total) (state : state @ immutable) = match state.stack with
  | Push (value, stack) -> Some (value, {state with stack})
  | Empty -> if state.unreachable then Some (Unknown, state) else None
let[@def] (take @ total) (expected : value @ immutable) (state : state @ immutable) =
  match pop state with Some (actual, rest) -> if compatible expected actual then Some rest else None | None -> None
let[@def] (push @ total) (value : value @ immutable) (state : state @ immutable) =
  {state with stack = Push (value, state.stack)}
let[@def] (unary @ total) (input : value @ immutable) (output : value @ immutable) (state : state @ immutable) =
  match take input state with None -> None | Some rest -> Some (push output rest)
let[@def] (binary @ total) (input : value @ immutable) (output : value @ immutable) (state : state @ immutable) =
  match take input state with None -> None | Some rest -> unary input output rest
let[@def] (consume_result @ total) (result : F.result_type @ immutable) (state : state @ immutable) =
  match result with F.Void -> Some state | F.I32 -> take I32 state | F.I64 -> take I64 state
let[@def] (produce_result @ total) (result : F.result_type @ immutable) (state : state @ immutable) =
  match result with F.Void -> state | F.I32 -> push I32 state | F.I64 -> push I64 state
let[@def] (finish @ total) (result : F.result_type @ immutable) (state : state @ immutable) =
  match consume_result result state with None -> false | Some rest -> match rest.stack with Empty -> true | _ -> false
let[@def] (dead @ total) (unit : unit) = {stack = Empty; unreachable = true}
let[@def] (initial @ total) (unit : unit) = {stack = Empty; unreachable = false}
let[@def] rec (local @ total) (locals : F.local_types @ immutable) (index : B.u32) = match locals with
  | F.No_locals -> None
  | F.Local32 rest -> if index = 0 then Some I32 else local rest (index - 1)
  | F.Local64 rest -> if index = 0 then Some I64 else local rest (index - 1)
let[@def] (global @ total) (globals : G.t @ immutable) (index : B.u32) = match G.get globals index with
  | None -> None | Some (S.I32 _) -> Some I32 | Some (S.I64 _) -> Some I64
let[@def] (select @ total) (state : state @ immutable) =
  match take I32 state with None -> None | Some rest ->
  match pop rest with None -> None | Some (right, rest) ->
  match pop rest with None -> None | Some (left, rest) ->
  if not (compatible left right) then None else
  Some (push (match left with Unknown -> right | _ -> left) rest)
let[@def] (store @ total) (value : value @ immutable) (state : state @ immutable) =
  match take value state with None -> None | Some rest -> take I32 rest
let[@def] (plain @ total) (op : I.plain @ immutable) (state : state @ immutable) = match op with
  | I.Unreachable -> Some (dead ())
  | I.Nop -> Some state
  | I.Drop -> (match pop state with None -> None | Some (_, rest) -> Some rest)
  | I.Select -> select state
  | I.I32_eqz -> unary I32 I32 state
  | I.I64_eqz -> unary I64 I32 state
  | I.I32_eq | I.I32_ne | I.I32_lt_u | I.I32_gt_u | I.I32_le_u | I.I32_ge_u
  | I.I32_add | I.I32_sub | I.I32_mul | I.I32_and | I.I32_or -> binary I32 I32 state
  | I.I64_eq | I.I64_lt_u -> binary I64 I32 state
  | I.I64_add | I.I64_sub | I.I64_and | I.I64_or | I.I64_shl | I.I64_shr_u -> binary I64 I64 state
  | I.I32_wrap_i64 -> unary I64 I32 state
  | I.I64_extend_i32_u -> unary I32 I64 state
  | I.Else | I.End | I.Return -> None
let[@def] (instruction @ total) (context : context @ immutable) (op : I.t @ immutable) (state : state @ immutable) = match op with
  | I.Plain I.Return -> (match consume_result context.result state with None -> None | Some _ -> Some (dead ()))
  | I.Plain op -> plain op state
  | I.I32_const _ -> Some (push I32 state)
  | I.I64_const _ -> Some (push I64 state)
  | I.Local_get index -> (match local context.locals index with None -> None | Some ty -> Some (push ty state))
  | I.Local_set index -> (match local context.locals index with None -> None | Some ty -> take ty state)
  | I.Local_tee index -> (match local context.locals index with None -> None | Some ty -> unary ty ty state)
  | I.Global_get index -> (match global context.globals index with None -> None | Some ty -> Some (push ty state))
  | I.Global_set index -> if not (G.writable context.globals.G.permissions index) then None else
    (match global context.globals index with None -> None | Some ty -> take ty state)
  | I.I32_load _ -> unary I32 I32 state
  | I.I64_load _ -> unary I32 I64 state
  | I.I32_store _ -> store I32 state
  | I.I64_store _ -> store I64 state
  | I.Call index -> (match F.lookup context.module_.F.functions index with None -> None | Some callee -> Some (produce_result callee.F.result state))
  | I.Call_indirect index -> (match F.signature context.module_.F.signatures index, take I32 state with
    | Some result, Some rest -> Some (produce_result result rest) | _ -> None)
  | I.Br _ | I.Br_if _ | I.Block | I.Loop | I.If -> None
