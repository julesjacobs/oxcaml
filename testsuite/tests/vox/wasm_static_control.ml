module B = Wasm_u32
module I = Wasm_instruction
module F = Wasm_functions
module G = Wasm_globals
module T = Wasm_control
module V = Wasm_static_types

type labels = Root of F.result_type | Label of labels [@@inductive]
let[@def] rec (label @ total) (labels : labels @ immutable) (depth : B.u32) = match labels with
  | Root result -> if depth = 0 then Some result else None
  | Label rest -> if depth = 0 then Some F.Void else label rest (depth - 1)
let[@def] (branch @ total) (labels : labels @ immutable) (depth : B.u32) (state : V.state @ immutable) =
  match label labels depth with None -> false | Some result ->
    match V.consume_result result state with None -> false | Some _ -> true
let[@def] rec (check @ total) (context : V.context @ immutable) (labels : labels @ immutable)
    (code : T.code @ immutable) (state : V.state @ immutable) : V.state option @ immutable = match code with
  | T.Empty -> Some state
  | T.Instruction (I.Br depth, tail) ->
    if branch labels depth state then check context labels tail (V.dead ()) else None
  | T.Instruction (I.Br_if depth, tail) -> (match V.take V.I32 state with
    | None -> None | Some rest ->
      match label labels depth with None -> None | Some result ->
      match V.consume_result result rest with None -> None | Some after ->
        check context labels tail (V.produce_result result after))
  | T.Instruction (op, tail) -> (match V.instruction context op state with
    | None -> None | Some after -> check context labels tail after)
  | T.Block (body, tail) | T.Loop (body, tail) ->
    (match check context (Label labels) body (V.initial ()) with
    | Some after -> if V.finish F.Void after then check context labels tail state else None
    | None -> None)
  | T.If (yes, no, tail) -> (match V.take V.I32 state with
    | None -> None | Some rest ->
      match check context (Label labels) yes (V.initial ()), check context (Label labels) no (V.initial ()) with
      | Some yes, Some no -> if V.finish F.Void yes && V.finish F.Void no then check context labels tail rest else None
      | _ -> None)
let[@def] (function_ @ total) (module_ : F.module_ @ immutable) (globals : G.t @ immutable) (function_ : F.function_ @ immutable) =
  let context = {V.module_; globals; locals = function_.F.locals; result = function_.F.result} in
  match check context (Root function_.F.result) function_.F.code (V.initial ()) with
  | None -> false | Some after -> V.finish function_.F.result after
let[@def] rec (functions @ total) (module_ : F.module_ @ immutable) (globals : G.t @ immutable) (entries : F.functions @ immutable) =
  match entries with F.No_functions -> true | F.Function (entry, rest) -> function_ module_ globals entry && functions module_ globals rest
let[@def] (function_bodies @ total) (module_ : F.module_ @ immutable) (globals : G.t @ immutable) = functions module_ globals module_.F.functions
