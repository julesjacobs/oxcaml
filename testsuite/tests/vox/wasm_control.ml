module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution

type code = Empty | Instruction of I.t * code
  | Block of code * code | Loop of code * code | If of code * code * code [@@inductive]
type label = {restart : code option; continuation : code; saved : S.stack}
type labels = No_labels | Label of label * labels [@@inductive]
type configuration = {code : code; labels : labels; state : X.state}
type result = Running of configuration | Finished of X.state | Type_error | Trap | Not_supported [@@inductive]

let[@def] (stack @ total) (state : X.state @ immutable) (stack : S.stack @ immutable) : X.state @ immutable =
  {X.memory = state.X.memory; machine = {E.locals = state.X.machine.E.locals; stack}}
let[@def] (enter @ total) (body : code @ immutable) (tail : code @ immutable)
    (restart : code option @ immutable) (configuration : configuration @ immutable) =
  let label = {restart; continuation = tail; saved = configuration.state.X.machine.E.stack} in
  Running {code = body; labels = Label (label, configuration.labels);
    state = stack configuration.state S.Empty}
let[@def] rec (branch @ total) (depth : B.u32) (labels : labels @ immutable) (state : X.state @ immutable) =
  match labels with
  | No_labels -> Type_error
  | Label (label, outer) ->
    if depth <> 0 then branch (depth - 1) outer state else
      match label.restart with
      | None -> Running {code = label.continuation; labels = outer; state = stack state label.saved}
      | Some body -> Running {code = body; labels; state = stack state S.Empty}
let[@def] (step @ total) (configuration : configuration @ immutable) =
  let state = configuration.state in
  match configuration.code with
  | Empty -> (match configuration.labels with
    | No_labels -> Finished state
    | Label (label, outer) -> (match state.X.machine.E.stack with
      | S.Empty -> Running {code = label.continuation; labels = outer; state = stack state label.saved}
      | _ -> Type_error))
  | Block (body, tail) -> enter body tail None configuration
  | Loop (body, tail) -> enter body tail (Some body) configuration
  | If (yes, no, tail) -> (match state.X.machine.E.stack with
    | S.Push (S.I32 condition, rest) ->
      enter (if condition <> 0 then yes else no) tail None
        {code = configuration.code; labels = configuration.labels; state = stack state rest}
    | _ -> Type_error)
  | Instruction (I.Br depth, _) -> branch depth configuration.labels state
  | Instruction (I.Br_if depth, tail) -> (match state.X.machine.E.stack with
    | S.Push (S.I32 condition, rest) ->
      let state = stack state rest in
      if condition <> 0 then branch depth configuration.labels state
      else Running {code = tail; labels = configuration.labels; state}
    | _ -> Type_error)
  | Instruction (I.Plain I.Unreachable, _) -> Trap
  | Instruction (instruction, tail) -> (match X.step instruction state with
    | X.Done state -> Running {code = tail; labels = configuration.labels; state}
    | X.Type_error -> Type_error | X.Trap -> Trap | X.Not_supported -> Not_supported)
let[@def] rec (run @ total) (fuel : C.count @ immutable) (configuration : configuration @ immutable) =
  match fuel with
  | C.Zero -> Running configuration
  | C.Succ fuel -> (match step configuration with
    | Running next -> run fuel next
    | result -> result)
let[@def] rec (flatten @ total) (code : code @ immutable) (tail : C.t @ immutable) : C.t @ immutable =
  match code with
  | Empty -> tail
  | Instruction (instruction, rest) -> C.Next (instruction, flatten rest tail)
  | Block (body, rest) -> C.Next (I.Block, flatten body (C.Next (I.Plain I.End, flatten rest tail)))
  | Loop (body, rest) -> C.Next (I.Loop, flatten body (C.Next (I.Plain I.End, flatten rest tail)))
  | If (yes, no, rest) -> C.Next (I.If, flatten yes
      (C.Next (I.Plain I.Else, flatten no (C.Next (I.Plain I.End, flatten rest tail)))))
