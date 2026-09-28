module B = Wasm_u32
module I = Wasm_instruction
module T = Wasm_control
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module P = Wasm_instance_control
module C = Wasm_code
module F = Wasm_functions
module M = Wasm_calls
let[@def] rec (budget @ total) (callers : M.callers @ immutable) (capacity : C.count @ immutable)
    (total : C.count @ immutable) = ghost_ (
  match callers with
  | M.Root -> capacity === total
  | M.Caller (_, rest) -> (match total with C.Zero -> false | C.Succ total -> budget rest capacity total))
let rec (increase @ total) : (callers : M.callers) @ immutable -> (capacity : C.count) @ immutable -> (total : C.count) @ immutable ->
    {u : unit | budget callers capacity total} ->
    {u : unit | budget callers (C.Succ capacity) (C.Succ total)} @ ghost = fun callers capacity total premise -> ghost_ (
  budget_def callers capacity total; budget_def callers (C.Succ capacity) (C.Succ total);
  match callers, total with
  | M.Caller (_, rest), C.Succ total -> increase rest capacity total ()
  | _ -> ())
let rec (decrease @ total) : (callers : M.callers) @ immutable -> (capacity : C.count) @ immutable -> (total : C.count) @ immutable ->
    {u : unit | budget callers (C.Succ capacity) total} ->
    {u : unit | match total with C.Zero -> false | C.Succ rest -> budget callers capacity rest} @ ghost =
  fun callers capacity total premise -> ghost_ (
    budget_def callers (C.Succ capacity) total;
    match callers, total with
    | M.Root, C.Succ rest -> budget_def callers capacity rest
    | M.Caller (_, tail), C.Succ rest ->
      decrease tail capacity rest (); budget_def callers capacity rest
    | _ -> ())
let (enter @ total) : (function_ : F.function_) @ immutable -> (tail : T.code) @ immutable ->
    (configuration : M.configuration) @ immutable -> (total : C.count) @ immutable ->
    {u : unit | budget configuration.M.callers configuration.M.capacity total} ->
    {u : unit | match M.enter function_ tail configuration with
      | M.Running next -> budget next.M.callers next.M.capacity total
      | M.Host_limit -> configuration.M.capacity === C.Zero
      | _ -> false} @ ghost = fun function_ tail configuration total premise -> ghost_ (
  M.enter_def function_ tail configuration;
  match configuration.M.capacity with
  | C.Zero -> ()
  | C.Succ remaining ->
    decrease configuration.M.callers remaining total ();
    let current = configuration.M.current in
    let machine = current.P.body.T.state.X.machine in
    let caller = {M.code = tail; labels = current.P.body.T.labels;
      locals = machine.E.locals; stack = machine.E.stack; result = configuration.M.result} in
    budget_def (M.Caller (caller, configuration.M.callers)) remaining total;
    (match total with
    | C.Zero -> budget_def configuration.M.callers configuration.M.capacity total
    | C.Succ rest -> ()))

let (leave @ total) : (configuration : M.configuration) @ immutable -> (total : C.count) @ immutable ->
    {u : unit | budget configuration.M.callers configuration.M.capacity total} ->
    {u : unit | match M.leave configuration with
      | M.Running next -> budget next.M.callers next.M.capacity total
      | M.Finished _ | M.Type_error -> true
      | _ -> false} @ ghost = fun configuration total premise -> ghost_ (
  M.leave_def configuration;
  budget_def configuration.M.callers configuration.M.capacity total;
  match configuration.M.callers, total with
  | M.Caller (_, rest), C.Succ total -> increase rest configuration.M.capacity total ()
  | _ -> ())

let[@def] (preserves @ total) (result : M.result @ immutable) (total : C.count @ immutable) = ghost_ (
  match result with M.Running next -> budget next.M.callers next.M.capacity total | _ -> true)
let (advance @ total) : (configuration : M.configuration) @ immutable -> (total : C.count) @ immutable ->
    {u : unit | budget configuration.M.callers configuration.M.capacity total} ->
    {u : unit | preserves (M.advance configuration) total} @ ghost = fun configuration total premise -> ghost_ (
  leave configuration total (); M.advance_def configuration; preserves_def (M.advance configuration) total)
let (branch @ total) : (depth : B.u32) -> (configuration : M.configuration) @ immutable -> (total : C.count) @ immutable ->
    {u : unit | budget configuration.M.callers configuration.M.capacity total} ->
    {u : unit | preserves (M.branch depth configuration) total} @ ghost = fun depth configuration total premise -> ghost_ (
  let current = configuration.M.current in
  let next = {M.current = {P.globals = current.P.globals;
    body = {T.code = T.Instruction (I.Br depth, T.Empty);
      labels = current.P.body.T.labels; state = current.P.body.T.state}};
    result = configuration.M.result; callers = configuration.M.callers; capacity = configuration.M.capacity} in
  leave configuration total (); advance next total ();
  M.branch_def depth configuration; preserves_def (M.branch depth configuration) total;
  preserves_def (M.advance next) total)
let (step @ total) : (module_ : F.module_) @ immutable -> (configuration : M.configuration) @ immutable ->
    (total : C.count) @ immutable -> {u : unit | budget configuration.M.callers configuration.M.capacity total} ->
    {u : unit | preserves (M.step module_ configuration) total} @ ghost = fun module_ configuration total premise -> ghost_ (
  M.step_def module_ configuration; preserves_def (M.step module_ configuration) total;
  let current = configuration.M.current in
  match current.P.body.T.code with
  | T.Instruction (I.Call index, tail) ->
    (match F.lookup module_.F.functions index with None -> () | Some function_ -> enter function_ tail configuration total ())
  | T.Instruction (I.Call_indirect signature, tail) ->
    (match current.P.body.T.state.X.machine.E.stack with
    | S.Push (S.I32 index, rest) ->
      (match F.signature module_.F.signatures signature, F.element module_.F.table index with
      | Some expected, Some index -> (match F.lookup module_.F.functions index with
        | Some function_ -> enter function_ tail {configuration with M.current = M.with_stack current rest} total ()
        | None -> ())
      | _ -> ())
    | _ -> ())
  | T.Instruction (I.Plain I.Return, _) -> leave configuration total ()
  | T.Instruction (I.Br depth, _) ->
    branch depth configuration total (); preserves_def (M.branch depth configuration) total
  | T.Instruction (I.Br_if depth, _) ->
    (match current.P.body.T.state.X.machine.E.stack with
    | S.Push (S.I32 _, rest) ->
      let next = {configuration with M.current = M.with_stack current rest} in
      branch depth next total (); preserves_def (M.branch depth next) total
    | _ -> ())
  | _ -> advance configuration total (); preserves_def (M.advance configuration) total)
let rec (run @ total) : (fuel : C.count) @ immutable -> (module_ : F.module_) @ immutable ->
    (configuration : M.configuration) @ immutable -> (total : C.count) @ immutable ->
    {u : unit | budget configuration.M.callers configuration.M.capacity total} ->
    {u : unit | preserves (M.run fuel module_ configuration) total} @ ghost = fun fuel module_ configuration total premise -> ghost_ (
  M.run_def fuel module_ configuration;
  match fuel with
  | C.Zero -> preserves_def (M.run fuel module_ configuration) total
  | C.Succ fuel ->
    step module_ configuration total (); preserves_def (M.step module_ configuration) total;
    (match M.step module_ configuration with
    | M.Running next -> run fuel module_ next total ()
    | _ -> ()))

let (start @ total) : (module_ : F.module_) @ immutable -> (index : B.u32) ->
    (memory : B.bytes) @ immutable -> (globals : Wasm_globals.t) @ immutable -> (capacity : C.count) @ immutable ->
    {u : unit | preserves (M.start module_ index memory globals capacity) capacity} @ ghost =
  fun module_ index memory globals capacity -> ghost_ (
    M.start_def module_ index memory globals capacity;
    preserves_def (M.start module_ index memory globals capacity) capacity;
    budget_def M.Root capacity capacity)
