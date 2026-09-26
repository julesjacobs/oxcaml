module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module L = Wasm_locals
module M = Wasm_memory
module T = Wasm_control
let (instruction @ total) : (op : I.t) @ immutable -> (before : X.state) @ immutable -> (after : X.state) @ immutable ->
    (local : B.u32) -> (value : S.value) @ immutable ->
    {u : unit | L.can_set before.X.machine.E.locals local value && X.step op before === X.Done after} ->
    {u : unit | L.can_set after.X.machine.E.locals local value} @ ghost = fun op before after local value premise -> ghost_ (
  X.step_def op before;
  match op with
  | I.I32_load (_, offset) -> X.read_def M.W32 offset before
  | I.I64_load (_, offset) -> X.read_def M.W64 offset before
  | I.I32_store (_, offset) -> X.write_def M.W32 offset before
  | I.I64_store (_, offset) -> X.write_def M.W64 offset before
  | _ ->
    E.step_def op before.X.machine;
    match op, before.X.machine.E.stack with
    | (I.Local_set destination | I.Local_tee destination), S.Push (written, _) ->
      (match L.set before.X.machine.E.locals destination written with
      | None -> ()
      | Some locals -> Wasm_snapshot_values.can_set before.X.machine.E.locals locals local value ())
    | _ -> ())
let rec (branch @ total) : (depth : B.u32) -> (labels : T.labels) @ immutable -> (state : X.state) @ immutable ->
    (after : T.configuration) @ immutable ->
    {u : unit | T.branch depth labels state === T.Running after} ->
    {u : unit | after.T.state.X.machine.E.locals === state.X.machine.E.locals} @ ghost = fun depth labels state after premise -> ghost_ (
  T.branch_def depth labels state;
  match labels with
  | T.No_labels -> ()
  | T.Label (label, rest) ->
    if depth <> 0 then branch (depth - 1) rest state after () else
    match label.T.restart with
    | None -> T.stack_def state label.T.saved
    | Some _ -> T.stack_def state S.Empty)
let (step @ total) : (before : T.configuration) @ immutable -> (after : T.configuration) @ immutable ->
    (local : B.u32) -> (value : S.value) @ immutable ->
    {u : unit | L.can_set before.T.state.X.machine.E.locals local value && T.step before === T.Running after} ->
    {u : unit | L.can_set after.T.state.X.machine.E.locals local value} @ ghost = fun before after local value premise -> ghost_ (
  T.step_def before;
  match before.T.code with
  | T.Empty -> (match before.T.labels with T.No_labels -> () | T.Label (label, _) -> T.stack_def before.T.state label.T.saved)
  | T.Block (body, tail) -> T.enter_def body tail None before; T.stack_def before.T.state S.Empty
  | T.Loop (body, tail) -> T.enter_def body tail (Some body) before; T.stack_def before.T.state S.Empty
  | T.If (yes, no, tail) -> (match before.T.state.X.machine.E.stack with
    | S.Push (S.I32 condition, rest) ->
      T.stack_def before.T.state rest;
      T.enter_def (if condition <> 0 then yes else no) tail None {before with T.state = T.stack before.T.state rest};
      T.stack_def (T.stack before.T.state rest) S.Empty
    | _ -> ())
  | T.Instruction (I.Br depth, _) -> branch depth before.T.labels before.T.state after ()
  | T.Instruction (I.Br_if depth, _) -> (match before.T.state.X.machine.E.stack with
    | S.Push (S.I32 condition, rest) -> T.stack_def before.T.state rest;
      if condition <> 0 then branch depth before.T.labels (T.stack before.T.state rest) after () else ()
    | _ -> ())
  | T.Instruction (I.Plain I.Unreachable, _) -> ()
  | T.Instruction (op, _) -> instruction op before.T.state after.T.state local value ())
let rec (run @ total) : (fuel : C.count) @ immutable -> (before : T.configuration) @ immutable -> (after : T.configuration) @ immutable ->
    (local : B.u32) -> (value : S.value) @ immutable ->
    {u : unit | L.can_set before.T.state.X.machine.E.locals local value && T.run fuel before === T.Running after} ->
    {u : unit | L.can_set after.T.state.X.machine.E.locals local value} @ ghost = fun fuel before after local value premise -> ghost_ (
  T.run_def fuel before;
  match fuel with
  | C.Zero -> ()
  | C.Succ rest -> match T.step before with
    | T.Running middle -> step before middle local value (); run rest middle after local value ()
    | _ -> ())
