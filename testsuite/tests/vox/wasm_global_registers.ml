module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module E = Wasm_execution
module X = Wasm_memory_execution
module G = Wasm_globals
module GE = Wasm_global_execution
module Copy = Wasm_global_local_transfer
module Lift = Wasm_control_lift
type plan = End | Binding of B.u32 * B.u32 * plan [@@inductive]
let[@def] rec (load_code @ total) (plan : plan @ immutable) = match plan with
  | End -> C.Empty
  | Binding (global, local, rest) -> C.Next (I.Global_get global, C.Next (I.Local_set local, load_code rest))
let rec (load_straight @ total) : (plan : plan) @ immutable -> {u : unit | Lift.straight (load_code plan)} @ ghost = fun plan -> ghost_ (
  load_code_def plan; Lift.straight_def (load_code plan);
  match plan with End -> () | Binding (global, local, rest) ->
    Lift.ordinary_def (I.Global_get global); Lift.straight_def (C.Next (I.Local_set local, load_code rest));
    Lift.ordinary_def (I.Local_set local); load_straight rest)
let rec (load @ total) : (plan : plan) @ immutable -> (state : GE.state) @ immutable ->
    {out : GE.state option | GE.run (load_code plan) state === (match out with None -> GE.Type_error | Some next -> GE.Done next)
      && (match out with None -> true | Some next -> next.GE.globals === state.GE.globals
        && next.GE.execution.X.memory === state.GE.execution.X.memory
        && next.GE.execution.X.machine.E.stack === state.GE.execution.X.machine.E.stack)} @ immutable = fun plan state ->
  ghost_ (load_code_def plan);
  match plan with
  | End -> ghost_ (GE.run_def C.Empty state); Some state
  | Binding (global, local, rest) ->
    ghost_ (Copy.load_code_def global local;
      E.append_def (Copy.load_code global local) (load_code rest);
      E.append_def (C.Next (I.Local_set local, C.Empty)) (load_code rest);
      E.append_def C.Empty (load_code rest);
      GE.append_correct (Copy.load_code global local) (load_code rest) state);
    match Copy.load global local state with None -> None | Some next -> load rest next
let[@def] rec (store_code @ total) (plan : plan @ immutable) = match plan with
  | End -> C.Empty
  | Binding (global, local, rest) -> C.Next (I.Local_get local, C.Next (I.Global_set global, store_code rest))
let rec (store_straight @ total) : (plan : plan) @ immutable -> {u : unit | Lift.straight (store_code plan)} @ ghost = fun plan -> ghost_ (
  store_code_def plan; Lift.straight_def (store_code plan);
  match plan with End -> () | Binding (global, local, rest) ->
    Lift.ordinary_def (I.Local_get local); Lift.straight_def (C.Next (I.Global_set global, store_code rest));
    Lift.ordinary_def (I.Global_set global); store_straight rest)
let rec (store @ total) : (plan : plan) @ immutable -> (state : GE.state) @ immutable ->
    {out : GE.state option | GE.run (store_code plan) state === (match out with None -> GE.Type_error | Some next -> GE.Done next)
      && (match out with None -> true | Some next -> next.GE.execution === state.GE.execution && next.GE.globals.G.permissions === state.GE.globals.G.permissions)} @ immutable = fun plan state ->
  ghost_ (store_code_def plan);
  match plan with
  | End -> ghost_ (GE.run_def C.Empty state); Some state
  | Binding (global, local, rest) ->
    ghost_ (Copy.store_code_def global local;
      E.append_def (Copy.store_code global local) (store_code rest);
      E.append_def (C.Next (I.Global_set global, C.Empty)) (store_code rest);
      E.append_def C.Empty (store_code rest);
      GE.append_correct (Copy.store_code global local) (store_code rest) state);
    match Copy.store global local state with None -> None | Some next -> store rest next
