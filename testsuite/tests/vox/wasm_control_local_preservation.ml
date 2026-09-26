module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module L = Wasm_locals
module T = Wasm_control
module LP = Wasm_local_preservation
let[@def] rec (code @ total) (body : T.code @ immutable) (local : B.u32) = match body with
  | T.Empty -> true
  | T.Instruction (op, tail) -> LP.instruction_preserves op local && code tail local
  | T.Block (body, tail) | T.Loop (body, tail) -> code body local && code tail local
  | T.If (yes, no, tail) -> code yes local && code no local && code tail local
let[@def] rec (labels @ total) (stack : T.labels @ immutable) (local : B.u32) = match stack with
  | T.No_labels -> true
  | T.Label (label, rest) -> code label.T.continuation local && labels rest local
    && (match label.T.restart with None -> true | Some body -> code body local)
let rec (branch @ total) : (depth : B.u32) -> (stack : T.labels) @ immutable -> (state : X.state) @ immutable ->
    (after : T.configuration) @ immutable -> (local : B.u32) ->
    {u : unit | labels stack local && T.branch depth stack state === T.Running after} ->
    {u : unit | code after.T.code local && labels after.T.labels local && after.T.state.X.machine.E.locals === state.X.machine.E.locals} @ ghost =
  fun depth stack state after local premise -> ghost_ (
    labels_def stack local; T.branch_def depth stack state;
    match stack with
    | T.No_labels -> ()
    | T.Label (label, rest) -> if depth <> 0 then branch (depth - 1) rest state after local () else
      match label.T.restart with None -> T.stack_def state label.T.saved | Some _ -> T.stack_def state S.Empty)
let (step @ total) : (before : T.configuration) @ immutable -> (after : T.configuration) @ immutable -> (local : B.u32) ->
    {u : unit | code before.T.code local && labels before.T.labels local && T.step before === T.Running after} ->
    {u : unit | code after.T.code local && labels after.T.labels local
      && L.get after.T.state.X.machine.E.locals local === L.get before.T.state.X.machine.E.locals local} @ ghost =
  fun before after local premise -> ghost_ (
    code_def before.T.code local; labels_def before.T.labels local; T.step_def before;
    match before.T.code with
    | T.Empty -> (match before.T.labels with
      | T.No_labels -> ()
      | T.Label (label, _) -> T.stack_def before.T.state label.T.saved)
    | T.Block (body, tail) ->
      T.enter_def body tail None before; T.stack_def before.T.state S.Empty;
      labels_def (T.Label ({T.restart = None; continuation = tail; saved = before.T.state.X.machine.E.stack}, before.T.labels)) local
    | T.Loop (body, tail) ->
      T.enter_def body tail (Some body) before; T.stack_def before.T.state S.Empty;
      labels_def (T.Label ({T.restart = Some body; continuation = tail; saved = before.T.state.X.machine.E.stack}, before.T.labels)) local
    | T.If (yes, no, tail) -> (match before.T.state.X.machine.E.stack with
      | S.Push (S.I32 condition, rest) ->
        T.stack_def before.T.state rest;
        T.enter_def (if condition <> 0 then yes else no) tail None {before with T.state = T.stack before.T.state rest};
        T.stack_def (T.stack before.T.state rest) S.Empty;
        labels_def (T.Label ({T.restart = None; continuation = tail; saved = rest}, before.T.labels)) local
      | _ -> ())
    | T.Instruction (I.Br depth, _) -> branch depth before.T.labels before.T.state after local ()
    | T.Instruction (I.Br_if depth, _) -> (match before.T.state.X.machine.E.stack with
      | S.Push (S.I32 condition, rest) ->
        T.stack_def before.T.state rest;
        if condition <> 0 then branch depth before.T.labels (T.stack before.T.state rest) after local () else ()
      | _ -> ())
    | T.Instruction (I.Plain I.Unreachable, _) -> ()
    | T.Instruction (op, _) -> LP.step op before.T.state after.T.state local ())
let rec (run @ total) : (fuel : C.count) @ immutable -> (before : T.configuration) @ immutable -> (after : T.configuration) @ immutable -> (local : B.u32) ->
    {u : unit | code before.T.code local && labels before.T.labels local && T.run fuel before === T.Running after} ->
    {u : unit | code after.T.code local && labels after.T.labels local
      && L.get after.T.state.X.machine.E.locals local === L.get before.T.state.X.machine.E.locals local} @ ghost =
  fun fuel before after local premise -> ghost_ (
    T.run_def fuel before;
    match fuel with
    | C.Zero -> ()
    | C.Succ rest -> match T.step before with
      | T.Running middle -> step before middle local (); run rest middle after local ()
      | _ -> ())
