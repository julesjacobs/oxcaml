module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module G = Wasm_globals

type state = {execution : X.state; globals : G.t}
type result = Done of state | Type_error | Trap | Not_supported [@@inductive]
let[@def] (step @ total) (instruction : I.t @ immutable) (state : state @ immutable) : result @ immutable =
  let execution = state.execution in
  match instruction with
  | I.Global_get index -> (match G.get state.globals index with
    | None -> Type_error
    | Some value -> Done {globals = state.globals;
        execution = {X.memory = execution.X.memory;
          machine = {E.locals = execution.X.machine.E.locals; stack = S.Push (value, execution.X.machine.E.stack)}}})
  | I.Global_set index -> (match execution.X.machine.E.stack with
    | S.Empty -> Type_error
    | S.Push (value, rest) -> (match G.set state.globals index value with
      | None -> Type_error
      | Some globals -> Done {globals; execution = {X.memory = execution.X.memory;
          machine = {E.locals = execution.X.machine.E.locals; stack = rest}}}))
  | _ -> (match X.step instruction execution with
    | X.Done execution -> Done {execution; globals = state.globals}
    | X.Type_error -> Type_error | X.Trap -> Trap | X.Not_supported -> Not_supported)
let[@def] rec (run @ total) (code : C.t @ immutable) (state : state @ immutable) : result @ immutable =
  match code with
  | C.Empty -> Done state
  | C.Next (instruction, rest) -> (match step instruction state with
    | Done next -> run rest next
    | result -> result)
let rec (append_correct @ total) : (code : C.t) @ immutable -> (tail : C.t) @ immutable ->
    (state : state) @ immutable ->
    {u : unit | run (E.append code tail) state ===
      (match run code state with Done next -> run tail next
        | Type_error -> Type_error | Trap -> Trap | Not_supported -> Not_supported)} @ ghost =
  fun code tail state -> ghost_ (
    E.append_def code tail; run_def code state;
    match code with
    | C.Empty -> ()
    | C.Next (instruction, rest) -> run_def (E.append code tail) state;
      (match step instruction state with Done next -> append_correct rest tail next | _ -> ()))
