module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module GE = Wasm_global_execution
module T = Wasm_control
module P = Wasm_instance_control
module F = Wasm_functions

type caller = {code : T.code; labels : T.labels; locals : S.stack; stack : S.stack; result : F.result_type}
type callers = Root | Caller of caller * callers [@@inductive]
type configuration = {current : P.configuration; result : F.result_type; callers : callers; capacity : C.count}
type result = Running of configuration | Finished of GE.state | Type_error | Trap | Host_limit | Not_supported [@@inductive]
type target = Local_label | Function_label | Invalid_label [@@inductive]
let[@def] rec (target @ total) (depth : B.u32) (labels : T.labels @ immutable) =
  match labels with
  | T.No_labels -> if depth = 0 then Function_label else Invalid_label
  | T.Label (_, outer) -> if depth = 0 then Local_label else target (depth - 1) outer
let[@def] (with_stack @ total) (current : P.configuration @ immutable) (stack : S.stack @ immutable) =
  {P.globals = current.P.globals; body = {T.code = current.P.body.T.code; labels = current.P.body.T.labels;
    state = T.stack current.P.body.T.state stack}}
let[@def] (enter @ total) (function_ : F.function_ @ immutable) (tail : T.code @ immutable)
    (configuration : configuration @ immutable) =
  match configuration.capacity with
  | C.Zero -> Host_limit
  | C.Succ capacity ->
    let current = configuration.current in
    let machine = current.P.body.T.state.X.machine in
    let caller = {code = tail; labels = current.P.body.T.labels; locals = machine.E.locals;
      stack = machine.E.stack; result = configuration.result} in
    Running {result = function_.F.result; callers = Caller (caller, configuration.callers); capacity;
      current = {P.globals = current.P.globals;
        body = {T.code = function_.F.code; labels = T.No_labels;
          state = {X.memory = current.P.body.T.state.X.memory;
            machine = {E.locals = F.zero_locals function_.F.locals; stack = S.Empty}}}}}
let[@def] (leave @ total) (configuration : configuration @ immutable) =
  let current = configuration.current in
  let execution = current.P.body.T.state in
  match F.take_result configuration.result execution.X.machine.E.stack with
  | None -> Type_error
  | Some value -> (match configuration.callers with
    | Root -> Finished {GE.globals = current.P.globals; execution = T.stack execution (F.deliver value S.Empty)}
    | Caller (caller, callers) ->
      Running {result = caller.result; callers; capacity = C.Succ configuration.capacity;
        current = {P.globals = current.P.globals;
          body = {T.code = caller.code; labels = caller.labels;
            state = {X.memory = execution.X.memory;
              machine = {E.locals = caller.locals; stack = F.deliver value caller.stack}}}}})
let[@def] (advance @ total) (configuration : configuration @ immutable) =
  match P.step configuration.current with
  | P.Running current -> Running {current; result = configuration.result;
      callers = configuration.callers; capacity = configuration.capacity}
  | P.Finished _ -> if F.complete configuration.result configuration.current.P.body.T.state.X.machine.E.stack
      then leave configuration else Type_error
  | P.Type_error -> Type_error | P.Trap -> Trap | P.Not_supported -> Not_supported
let[@def] (branch @ total) (depth : B.u32) (configuration : configuration @ immutable) =
  match target depth configuration.current.P.body.T.labels with
  | Function_label -> leave configuration
  | Invalid_label -> Type_error
  | Local_label ->
    let current = configuration.current in
    advance {current = {P.globals = current.P.globals;
      body = {T.code = T.Instruction (I.Br depth, T.Empty); labels = current.P.body.T.labels; state = current.P.body.T.state}};
      result = configuration.result; callers = configuration.callers; capacity = configuration.capacity}
let[@def] (step @ total) (module_ : F.module_ @ immutable) (configuration : configuration @ immutable) =
  let current = configuration.current in
  match current.P.body.T.code with
  | T.Instruction (I.Call index, tail) -> (match F.lookup module_.F.functions index with
    | None -> Type_error | Some function_ -> enter function_ tail configuration)
  | T.Instruction (I.Call_indirect signature, tail) ->
    (match current.P.body.T.state.X.machine.E.stack with
    | S.Push (S.I32 index, rest) -> (match F.signature module_.F.signatures signature with
      | None -> Type_error
      | Some expected -> (match F.element module_.F.table index with
        | None -> Trap
        | Some index -> (match F.lookup module_.F.functions index with
          | None -> Type_error
          | Some function_ -> if F.same_result expected function_.F.result then
              enter function_ tail {configuration with current = with_stack current rest}
            else Trap)))
    | _ -> Type_error)
  | T.Instruction (I.Plain I.Return, _) -> leave configuration
  | T.Instruction (I.Br depth, _) -> branch depth configuration
  | T.Instruction (I.Br_if depth, tail) -> (match current.P.body.T.state.X.machine.E.stack with
    | S.Push (S.I32 condition, rest) ->
      let current = with_stack current rest in
      if condition <> 0 then branch depth {configuration with current} else
        Running {configuration with current = {current with P.body = {current.P.body with T.code = tail}}}
    | _ -> Type_error)
  | _ -> advance configuration
let[@def] rec (run @ total) (fuel : C.count @ immutable) (module_ : F.module_ @ immutable)
    (configuration : configuration @ immutable) =
  match fuel with
  | C.Zero -> Running configuration
  | C.Succ fuel -> (match step module_ configuration with Running next -> run fuel module_ next | result -> result)

let[@def] (start @ total) (module_ : F.module_ @ immutable) (index : B.u32)
    (memory : B.bytes @ immutable) (globals : Wasm_globals.t @ immutable) (capacity : C.count @ immutable) =
  match F.lookup module_.F.functions index with
  | None -> Type_error
  | Some function_ ->
    Running {result = function_.F.result; callers = Root; capacity;
      current = {P.globals; body = {T.code = function_.F.code; labels = T.No_labels;
        state = {X.memory; machine = {E.locals = F.zero_locals function_.F.locals; stack = S.Empty}}}}}
