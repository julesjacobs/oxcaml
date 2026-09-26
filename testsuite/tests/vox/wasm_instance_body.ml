module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module T = Wasm_control
module G = Wasm_globals
module P = Wasm_instance_control
module Fuel = Wasm_control_compose
let (step @ total) : (before : T.configuration) @ immutable -> (after : T.configuration) @ immutable -> (globals : G.t) @ immutable ->
    {u : unit | T.step before === T.Running after} ->
    {u : unit | P.step {P.body = before; globals} === P.Running {P.body = after; globals}} @ ghost =
  fun before after globals premise -> ghost_ (
    P.step_def {P.body = before; globals};
    match before.T.code with
    | T.Instruction (((I.Global_get _ | I.Global_set _) as instruction), _) ->
      T.step_def before; X.step_def instruction before.T.state; E.step_def instruction before.T.state.X.machine;
      S.step_def instruction before.T.state.X.machine.E.stack
    | _ -> ())
let rec (run @ total) : (fuel : C.count) @ immutable -> (before : T.configuration) @ immutable ->
    (after : T.configuration) @ immutable -> (globals : G.t) @ immutable ->
    {u : unit | T.run fuel before === T.Running after} ->
    {u : unit | P.run fuel {P.body = before; globals} === P.Running {P.body = after; globals}} @ ghost =
  fun fuel before after globals premise -> ghost_ (
    T.run_def fuel before; P.run_def fuel {P.body = before; globals};
    match fuel with C.Zero -> () | C.Succ rest ->
      match T.step before with
      | T.Running next -> step before next globals (); run rest next after globals ()
      | _ -> ())
let rec (compose @ total) : (first : C.count) @ immutable -> (second : C.count) @ immutable -> (configuration : P.configuration) @ immutable ->
    {u : unit | P.run (Fuel.add first second) configuration ===
      (match P.run first configuration with P.Running next -> P.run second next | terminal -> terminal)} @ ghost =
  fun first second configuration -> ghost_ (
    Fuel.add_def first second; P.run_def first configuration; P.run_def (Fuel.add first second) configuration;
    match first with C.Zero -> () | C.Succ rest ->
      match P.step configuration with P.Running next -> compose rest second next | _ -> ())
