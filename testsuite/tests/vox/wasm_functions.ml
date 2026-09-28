module B = Wasm_u32
module S = Wasm_scalar
module T = Wasm_control

type result_type = Void | I32 | I64 [@@inductive]
type local_types = No_locals | Local32 of local_types | Local64 of local_types [@@inductive]
type function_ = {result : result_type; locals : local_types; code : T.code}
type functions = No_functions | Function of function_ * functions [@@inductive]
type signatures = No_signatures | Signature of result_type * signatures [@@inductive]
type table = No_elements | Element of B.u32 option * table [@@inductive]
type module_ = {functions : functions; signatures : signatures; table : table}
let[@def] (same_result @ total) (a : result_type @ immutable) (b : result_type @ immutable) =
  match a, b with Void, Void | I32, I32 | I64, I64 -> true | _ -> false
let[@def] rec (zero_locals @ total) (types : local_types @ immutable) : S.stack @ immutable =
  match types with
  | No_locals -> S.Empty
  | Local32 rest -> S.Push (S.I32 0, zero_locals rest)
  | Local64 rest -> S.Push (S.I64 {Hmc_word64.lo = 0; hi = 0}, zero_locals rest)
let[@def] rec (lookup @ total) (functions : functions @ immutable) (index : B.u32) =
  match functions with
  | No_functions -> None
  | Function (function_, rest) -> if index = 0 then Some function_ else lookup rest (index - 1)
let[@def] rec (signature @ total) (signatures : signatures @ immutable) (index : B.u32) =
  match signatures with
  | No_signatures -> None
  | Signature (result, rest) -> if index = 0 then Some result else signature rest (index - 1)
let[@def] rec (element @ total) (table : table @ immutable) (index : B.u32) =
  match table with
  | No_elements -> None
  | Element (value, rest) -> if index = 0 then value else element rest (index - 1)
let[@def] (take_result @ total) (result : result_type @ immutable) (stack : S.stack @ immutable)
    : S.value option option @ immutable =
  match result, stack with
  | Void, _ -> Some None
  | I32, S.Push ((S.I32 _ as value), _) | I64, S.Push ((S.I64 _ as value), _) -> Some (Some value)
  | _ -> None
let[@def] (complete @ total) (result : result_type @ immutable) (stack : S.stack @ immutable) =
  match result, stack with
  | Void, S.Empty | I32, S.Push (S.I32 _, S.Empty) | I64, S.Push (S.I64 _, S.Empty) -> true
  | _ -> false
let[@def] (deliver @ total) (value : S.value option @ immutable) (stack : S.stack @ immutable) : S.stack @ immutable =
  match value with None -> stack | Some value -> S.Push (value, stack)
