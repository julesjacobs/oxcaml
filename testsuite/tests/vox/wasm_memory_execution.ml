module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module M = Wasm_memory

type state = {machine : E.state; memory : B.bytes}
type result = Done of state | Type_error | Trap | Not_supported [@@inductive]
let[@def] (read @ total) (width : M.width @ immutable) (offset : B.u32) (state : state @ immutable) =
  match state.machine.E.stack with
  | S.Push (S.I32 base, rest) -> (match M.load state.memory base offset width with
    | None -> Trap
    | Some value -> Done {memory = state.memory;
        machine = {E.locals = state.machine.E.locals; stack = S.Push (value, rest)}})
  | _ -> Type_error
let[@def] (compatible @ total) (value : S.value @ immutable) (width : M.width @ immutable) =
  match M.width value, width with M.W32, M.W32 | M.W64, M.W64 -> true | _ -> false
let[@def] (write @ total) (width : M.width @ immutable) (offset : B.u32) (state : state @ immutable) =
  match state.machine.E.stack with
  | S.Push (value, S.Push (S.I32 base, rest)) ->
    if not (compatible value width) then Type_error else
      (match M.store state.memory base offset value with
      | None -> Trap
      | Some memory -> Done {memory; machine = {E.locals = state.machine.E.locals; stack = rest}})
  | _ -> Type_error
let[@def] (step @ total) (instruction : I.t @ immutable) (state : state @ immutable)
    : result @ immutable =
  match instruction with
  | I.I32_load (_, offset) -> read M.W32 offset state
  | I.I64_load (_, offset) -> read M.W64 offset state
  | I.I32_store (_, offset) -> write M.W32 offset state
  | I.I64_store (_, offset) -> write M.W64 offset state
  | _ -> (match E.step instruction state.machine with
    | E.Done machine -> Done {machine; memory = state.memory}
    | E.Type_error -> Type_error
    | E.Not_supported -> Not_supported)
let[@def] rec (run @ total) (code : C.t @ immutable) (state : state @ immutable)
    : result @ immutable =
  match code with
  | C.Empty -> Done state
  | C.Next (instruction, rest) -> (match step instruction state with
    | Done next -> run rest next
    | Type_error -> Type_error
    | Trap -> Trap
    | Not_supported -> Not_supported)

let rec (append_correct @ total) : (code : C.t) @ immutable -> (tail : C.t) @ immutable ->
    (state : state) @ immutable ->
    {u : unit | run (E.append code tail) state ===
      (match run code state with Done next -> run tail next
        | Type_error -> Type_error | Trap -> Trap | Not_supported -> Not_supported)} @ ghost =
  fun code tail state -> ghost_ (
    E.append_def code tail; run_def code state;
    match code with
    | C.Empty -> ()
    | C.Next (instruction, rest) ->
      run_def (E.append code tail) state;
      (match step instruction state with Done next -> append_correct rest tail next | _ -> ()))
let[@def] (non_memory @ total) (instruction : I.t @ immutable) =
  match instruction with I.I32_load _ | I.I64_load _ | I.I32_store _ | I.I64_store _ -> false | _ -> true
let[@def] rec (memory_free @ total) (code : C.t @ immutable) =
  match code with C.Empty -> true | C.Next (instruction, rest) -> non_memory instruction && memory_free rest
let rec (lift @ total) : (code : C.t) @ immutable -> (state : state) @ immutable -> (next : E.state) @ immutable ->
    {u : unit | memory_free code && E.run code state.machine === E.Done next} ->
    {u : unit | run code state === Done {machine = next; memory = state.memory}} @ ghost =
  fun code state next premise -> ghost_ (
    memory_free_def code; E.run_def code state.machine; run_def code state;
    match code with
    | C.Empty -> ()
    | C.Next (instruction, rest) ->
      non_memory_def instruction; step_def instruction state;
      (match E.step instruction state.machine with
      | E.Done machine -> lift rest {machine; memory = state.memory} next ()
      | _ -> ()))
