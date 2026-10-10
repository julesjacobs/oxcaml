module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module T = Wasm_control
module P = Wasm_instance_control
module F = Wasm_functions
module M = Wasm_calls
module Target = Wasm_control_branch_target
module Fuel = Wasm_control_compose
let rec (local_target @ total) : (depth : B.u32) -> (labels : T.labels) @ immutable ->
    {u : unit | Target.valid depth labels} -> {u : unit | M.target depth labels === M.Local_label} @ ghost =
  fun depth labels premise -> ghost_ (
    Target.valid_def depth labels; M.target_def depth labels;
    match labels with T.No_labels -> () | T.Label (_, rest) -> if depth = 0 then () else local_target (depth - 1) rest ())
let (step @ total) : (module_ : F.module_) @ immutable -> (before : M.configuration) @ immutable -> (after : P.configuration) @ immutable ->
    {u : unit | P.step before.M.current === P.Running after} ->
    {u : unit | M.step module_ before === M.Running {before with M.current = after}} @ ghost =
  fun module_ before after premise -> ghost_ (
    let current = before.M.current in
    let body = current.P.body in
    M.step_def module_ before; P.step_def current;
    match body.T.code with
    | T.Instruction (((I.Call _ | I.Call_indirect _ | I.Plain I.Return) as instruction), _) ->
      T.step_def body; X.step_def instruction body.T.state; E.step_def instruction body.T.state.X.machine;
      S.step_def instruction body.T.state.X.machine.E.stack
    | T.Instruction (I.Br depth, _) ->
      T.step_def body;
      Target.valid_of_running depth body.T.labels body.T.state after.P.body ();
      local_target depth body.T.labels ();
      M.branch_def depth before;
      let moved = {before with M.current = {P.globals = current.P.globals;
        body = {T.code = T.Instruction (I.Br depth, T.Empty); labels = body.T.labels; state = body.T.state}}} in
      M.advance_def moved; P.step_def moved.M.current; T.step_def moved.M.current.P.body
    | T.Instruction (I.Br_if depth, tail) ->
      T.step_def body;
      (match body.T.state.X.machine.E.stack with
      | S.Push (S.I32 condition, rest) ->
        M.with_stack_def current rest;
        let updated = T.stack body.T.state rest in
        if condition = 0 then () else (
          Target.valid_of_running depth body.T.labels updated after.P.body ();
          local_target depth body.T.labels ();
          let branching = {before with M.current = M.with_stack current rest} in
          M.branch_def depth branching;
          let moved = {branching with M.current = {P.globals = current.P.globals;
            body = {T.code = T.Instruction (I.Br depth, T.Empty); labels = body.T.labels; state = updated}}} in
          M.advance_def moved; P.step_def moved.M.current; T.step_def moved.M.current.P.body)
      | _ -> ())
    | _ -> M.advance_def before)
let rec (run @ total) : (fuel : C.count) @ immutable -> (module_ : F.module_) @ immutable ->
    (before : M.configuration) @ immutable -> (after : P.configuration) @ immutable ->
    {u : unit | P.run fuel before.M.current === P.Running after} ->
    {u : unit | M.run fuel module_ before === M.Running {before with M.current = after}} @ ghost =
  fun fuel module_ before after premise -> ghost_ (
    P.run_def fuel before.M.current; M.run_def fuel module_ before;
    match fuel with C.Zero -> () | C.Succ rest ->
      match P.step before.M.current with
      | P.Running next -> step module_ before next (); run rest module_ {before with M.current = next} after ()
      | _ -> ())
let rec (compose @ total) : (first : C.count) @ immutable -> (second : C.count) @ immutable ->
    (module_ : F.module_) @ immutable -> (configuration : M.configuration) @ immutable ->
    {u : unit | M.run (Fuel.add first second) module_ configuration ===
      (match M.run first module_ configuration with M.Running next -> M.run second module_ next | terminal -> terminal)} @ ghost =
  fun first second module_ configuration -> ghost_ (
    Fuel.add_def first second; M.run_def first module_ configuration; M.run_def (Fuel.add first second) module_ configuration;
    match first with C.Zero -> () | C.Succ rest ->
      match M.step module_ configuration with M.Running next -> compose rest second module_ next | _ -> ())
