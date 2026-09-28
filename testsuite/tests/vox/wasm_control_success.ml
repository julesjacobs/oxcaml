module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module Lift = Wasm_control_lift
let (ordinary @ total) : (instruction : I.t) @ immutable -> (state : X.state) @ immutable -> (after : X.state) @ immutable ->
    {u : unit | X.step instruction state === X.Done after} -> {u : unit | Lift.ordinary instruction} @ ghost =
  fun instruction state after premise -> ghost_ (
    Lift.ordinary_def instruction;
    match instruction with
    | I.Block | I.Loop | I.If | I.Br _ | I.Br_if _ | I.Plain I.End | I.Plain I.Else | I.Plain I.Unreachable ->
      X.step_def instruction state; E.step_def instruction state.X.machine; S.step_def instruction state.X.machine.E.stack
    | _ -> ())
let rec (straight @ total) : (code : C.t) @ immutable -> (state : X.state) @ immutable -> (after : X.state) @ immutable ->
    {u : unit | X.run code state === X.Done after} -> {u : unit | Lift.straight code} @ ghost =
  fun code state after premise -> ghost_ (
    X.run_def code state; Lift.straight_def code;
    match code with
    | C.Empty -> ()
    | C.Next (instruction, rest) -> (match X.step instruction state with
      | X.Done next -> ordinary instruction state next (); straight rest next after ()
      | _ -> ()))
