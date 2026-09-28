module I = Wasm_instruction
module C = Wasm_code
module T = Wasm_control
module G = Wasm_globals
module E = Wasm_global_execution
module Lift = Wasm_control_lift

type configuration = {body : T.configuration; globals : G.t}
type result = Running of configuration | Finished of E.state | Type_error | Trap | Not_supported [@@inductive]
let[@def] (step @ total) (configuration : configuration @ immutable) : result @ immutable =
  let body = configuration.body in
  match body.T.code with
  | T.Instruction ((I.Global_get _ | I.Global_set _) as instruction, tail) ->
    (match E.step instruction {E.execution = body.T.state; globals = configuration.globals} with
    | E.Done state -> Running {globals = state.E.globals;
        body = {T.code = tail; labels = body.T.labels; state = state.E.execution}}
    | E.Type_error -> Type_error | E.Trap -> Trap | E.Not_supported -> Not_supported)
  | _ -> (match T.step body with
    | T.Running body -> Running {body; globals = configuration.globals}
    | T.Finished execution -> Finished {E.execution; globals = configuration.globals}
    | T.Type_error -> Type_error | T.Trap -> Trap | T.Not_supported -> Not_supported)
let[@def] rec (run @ total) (fuel : C.count @ immutable) (configuration : configuration @ immutable) =
  match fuel with
  | C.Zero -> Running configuration
  | C.Succ fuel -> (match step configuration with Running next -> run fuel next | result -> result)
let rec (straight_line @ total) : (code : C.t) @ immutable -> (tail : T.code) @ immutable ->
    (labels : T.labels) @ immutable -> (before : E.state) @ immutable -> (after : E.state) @ immutable ->
    {u : unit | Lift.straight code && E.run code before === E.Done after} ->
    {u : unit | run (C.length code)
        {body = {T.code = Lift.embed code tail; labels; state = before.E.execution}; globals = before.E.globals}
      === Running {body = {T.code = tail; labels; state = after.E.execution}; globals = after.E.globals}} @ ghost =
  fun code tail labels before after premise -> ghost_ (
    Lift.straight_def code; Lift.embed_def code tail; C.length_def code; E.run_def code before;
    let body = {T.code = Lift.embed code tail; labels; state = before.E.execution} in
    let configuration = {body; globals = before.E.globals} in
    run_def (C.length code) configuration;
    match code with
    | C.Empty -> ()
    | C.Next (instruction, rest) ->
      Lift.ordinary_def instruction; step_def configuration; T.step_def body; E.step_def instruction before;
      (match E.step instruction before with
      | E.Done next -> straight_line rest tail labels next after ()
      | _ -> ()))
