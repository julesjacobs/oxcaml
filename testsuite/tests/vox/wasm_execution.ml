module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module L = Wasm_locals

type state = {locals : S.stack; stack : S.stack}
type result = Done of state | Type_error | Not_supported [@@inductive]
let[@def] (step @ total) (instruction : I.t @ immutable) (state : state @ immutable)
    : result @ immutable =
  match instruction with
  | I.Local_get index -> (match L.get state.locals index with
    | None -> Type_error
    | Some value -> Done {locals = state.locals; stack = S.Push (value, state.stack)})
  | I.Local_set index | I.Local_tee index -> (match state.stack with
    | S.Empty -> Type_error
    | S.Push (value, rest) -> (match L.set state.locals index value with
      | None -> Type_error
      | Some locals ->
        let stack = match instruction with I.Local_tee _ -> state.stack | _ -> rest in
        Done {locals; stack}))
  | _ -> (match S.step instruction state.stack with
    | S.Type_error -> Type_error
    | S.Not_scalar -> Not_supported
    | S.Done stack -> Done {locals = state.locals; stack})
let[@def] rec (run @ total) (code : C.t @ immutable) (state : state @ immutable)
    : result @ immutable =
  match code with
  | C.Empty -> Done state
  | C.Next (instruction, rest) -> (match step instruction state with
    | Done next -> run rest next
    | Type_error -> Type_error
    | Not_supported -> Not_supported)

let[@def] rec (append @ total) (code : C.t @ immutable) (tail : C.t @ immutable) : C.t @ immutable =
  match code with C.Empty -> tail | C.Next (instruction, rest) -> C.Next (instruction, append rest tail)
let rec (append_correct @ total) : (code : C.t) @ immutable -> (tail : C.t) @ immutable ->
    (state : state) @ immutable ->
    {u : unit | run (append code tail) state ===
      (match run code state with Done next -> run tail next
        | Type_error -> Type_error | Not_supported -> Not_supported)} @ ghost =
  fun code tail state -> ghost_ (
    append_def code tail; run_def code state;
    match code with
    | C.Empty -> ()
    | C.Next (instruction, rest) ->
      run_def (append code tail) state;
      (match step instruction state with
       | Done next -> append_correct rest tail next
       | _ -> ()))
