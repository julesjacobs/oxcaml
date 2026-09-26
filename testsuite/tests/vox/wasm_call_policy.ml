module B = Wasm_u32
module I = Wasm_instruction
module T = Wasm_control
module X = Wasm_memory_execution
module S = Wasm_scalar
module E = Wasm_execution
module P = Wasm_instance_control
module GE = Wasm_global_execution
type policy = No_calls | Indirect_calls [@@inductive]
let[@def] (instruction @ total) (policy : policy @ immutable) (op : I.t @ immutable) = match op with
  | I.Call _ -> false | I.Call_indirect _ -> (match policy with No_calls -> false | Indirect_calls -> true) | _ -> true
let[@def] rec (code @ total) (policy : policy @ immutable) (body : T.code @ immutable) = match body with
  | T.Empty -> true
  | T.Instruction (op, tail) -> instruction policy op && code policy tail
  | T.Block (body, tail) | T.Loop (body, tail) -> code policy body && code policy tail
  | T.If (yes, no, tail) -> code policy yes && code policy no && code policy tail
let[@def] (label @ total) (policy : policy @ immutable) (entry : T.label @ immutable) = code policy entry.T.continuation
  && match entry.T.restart with None -> true | Some body -> code policy body
let[@def] rec (labels @ total) (policy : policy @ immutable) (stack : T.labels @ immutable) = match stack with
  | T.No_labels -> true | T.Label (entry, rest) -> label policy entry && labels policy rest
let[@def] (configuration @ total) (policy : policy @ immutable) (state : T.configuration @ immutable) = code policy state.T.code && labels policy state.T.labels
let rec (branch @ total) : (policy : policy) @ immutable -> (depth : B.u32) -> (stack : T.labels) @ immutable -> (state : X.state) @ immutable ->
    (after : T.configuration) @ immutable ->
    {u : unit | labels policy stack && T.branch depth stack state === T.Running after} ->
    {u : unit | configuration policy after} @ ghost = fun policy depth stack state after premise -> ghost_ (
  labels_def policy stack; T.branch_def depth stack state;
  match stack with
  | T.No_labels -> ()
  | T.Label (entry, rest) ->
    label_def policy entry;
    if depth <> 0 then branch policy (depth - 1) rest state after () else
    (match entry.T.restart with
    | None -> configuration_def policy after
    | Some body -> configuration_def policy after))
let (enter @ total) : (policy : policy) @ immutable -> (body : T.code) @ immutable -> (tail : T.code) @ immutable -> (restart : T.code option) @ immutable ->
    (before : T.configuration) @ immutable -> (after : T.configuration) @ immutable ->
    {u : unit | code policy body && code policy tail && labels policy before.T.labels
      && (match restart with None -> true | Some body -> code policy body)
      && T.enter body tail restart before === T.Running after} ->
    {u : unit | configuration policy after} @ ghost = fun policy body tail restart before after premise -> ghost_ (
  T.enter_def body tail restart before;
  configuration_def policy after; labels_def policy after.T.labels;
  label_def policy {T.restart; continuation = tail; saved = before.T.state.X.machine.E.stack})
let (step @ total) : (policy : policy) @ immutable -> (before : T.configuration) @ immutable -> (after : T.configuration) @ immutable ->
    {u : unit | configuration policy before && T.step before === T.Running after} ->
    {u : unit | configuration policy after} @ ghost = fun policy before after premise -> ghost_ (
  configuration_def policy before; code_def policy before.T.code; T.step_def before;
  match before.T.code with
  | T.Empty ->
    labels_def policy before.T.labels;
    (match before.T.labels with
    | T.No_labels -> ()
    | T.Label (entry, rest) -> label_def policy entry; configuration_def policy after)
  | T.Block (body, tail) -> enter policy body tail None before after ()
  | T.Loop (body, tail) -> enter policy body tail (Some body) before after ()
  | T.If (yes, no, tail) ->
    (match before.T.state.X.machine.E.stack with
    | S.Push (S.I32 condition, rest) ->
      let state = {before with T.state = T.stack before.T.state rest} in
      enter policy (if condition <> 0 then yes else no) tail None state after ()
    | _ -> ())
  | T.Instruction (I.Br depth, _) -> branch policy depth before.T.labels before.T.state after ()
  | T.Instruction (I.Br_if depth, _) ->
    (match before.T.state.X.machine.E.stack with
    | S.Push (S.I32 condition, rest) ->
      if condition <> 0 then branch policy depth before.T.labels (T.stack before.T.state rest) after ()
      else configuration_def policy after
    | _ -> ())
  | T.Instruction (_, _) -> configuration_def policy after)
let (instance_step @ total) : (policy : policy) @ immutable -> (before : P.configuration) @ immutable -> (after : P.configuration) @ immutable ->
    {u : unit | configuration policy before.P.body && P.step before === P.Running after} ->
    {u : unit | configuration policy after.P.body} @ ghost = fun policy before after premise -> ghost_ (
  configuration_def policy before.P.body; code_def policy before.P.body.T.code; P.step_def before;
  match before.P.body.T.code with
  | T.Instruction ((I.Global_get _ | I.Global_set _), _) -> configuration_def policy after.P.body
  | _ -> step policy before.P.body after.P.body ())
