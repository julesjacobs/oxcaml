module D = Hm_declarative
module K = Hmc_closure_ir
module P = Hmc_closure_program
module R = Hmc_closure_semantics
module G = Hmc_cfg_ir
module C = Hmc_cfg_program

type temporaries = Empty | Environment of R.V.value * temporaries
  | Value of R.V.value * R.V.value * temporaries [@@inductive]
type activation = {pc : D.index; env : R.V.value; accumulator : R.V.value;
  temporaries : temporaries; current : R.V.value}
type frames = Halt | Frame of activation * frames [@@inductive]
type state = Running of activation * frames | Done of R.V.value | Stuck [@@inductive]
let[@def] rec (depth @ total) (frames : frames @ immutable) = match frames with
  | Halt -> D.Z | Frame (_, rest) -> D.S (depth rest)
let[@def] (load @ total) (globals : P.globals @ immutable) (env : R.V.value @ immutable) (atom : G.atom @ immutable) =
  match atom with
  | G.Local i -> R.lookup env i
  | G.Global i -> (match P.lookup globals i with None -> None | Some id -> Some (R.V.Closure (id, R.V.Empty)))
  | G.Closure id -> Some (R.V.Closure (id, env))
  | G.Truth -> Some R.V.True | G.False -> Some R.V.False | G.Word w -> Some (R.V.Word w) | G.Nil -> Some R.V.Nil
let[@def] (step @ total) (program : C.program @ immutable) (state : state @ immutable) = match state with
  | Done _ | Stuck -> state
  | Running (a, frames) -> (match G.lookup program.C.blocks a.pc with
    | None -> Stuck
    | Some block -> match block.G.instruction with
      | G.Load (atom, _, _, next) -> (match load program.C.origin.P.globals a.env atom with
        | None -> Stuck | Some accumulator -> Running ({a with pc = next; accumulator}, frames))
      | G.Jump next -> Running ({a with pc = next}, frames)
      | G.Save_environment next -> Running ({a with pc = next; temporaries = Environment (a.env, a.temporaries)}, frames)
      | G.Save_value next -> (match a.temporaries with
        | Environment (env, rest) -> Running ({a with pc = next; env; temporaries = Value (a.accumulator, env, rest)}, frames)
        | _ -> Stuck)
      | G.Bind next -> (match a.temporaries with
        | Environment (env, _) -> Running ({a with pc = next; env = R.V.Bind (a.accumulator, env)}, frames)
        | _ -> Stuck)
      | G.Restore next -> (match a.temporaries with
        | Environment (env, rest) -> Running ({a with pc = next; env; temporaries = rest}, frames)
        | _ -> Stuck)
      | G.Primitive (op, next) -> (match a.temporaries, a.accumulator with
        | Value (R.V.Word left, env, rest), R.V.Word right ->
          Running ({a with pc = next; env; temporaries = rest; accumulator = R.primitive op left right}, frames)
        | _ -> Stuck)
      | G.Cons next -> (match a.temporaries with
        | Value (head, env, rest) -> Running ({a with pc = next; env; temporaries = rest; accumulator = R.V.Cons (head, a.accumulator)}, frames)
        | _ -> Stuck)
      | G.Call next -> (match a.temporaries with
        | Value ((R.V.Closure (id, captured) as closure), env, rest) ->
          (match K.lookup program.C.origin.P.table id, C.lookup program.C.functions id with
          | Some callee, Some code ->
            let saved = {a with pc = next; env; temporaries = rest} in
            let captured = if callee.K.recursive then R.V.Bind (closure, captured) else captured in
            let entered = {pc = code.C.start; env = R.V.Bind (a.accumulator, captured); accumulator = R.V.Nil;
              temporaries = Empty; current = closure} in
            Running (entered, Frame (saved, frames))
          | _ -> Stuck)
        | _ -> Stuck)
      | G.Branch (yes, no) -> (match a.accumulator with
        | R.V.True -> Running ({a with pc = yes}, frames)
        | R.V.False -> Running ({a with pc = no}, frames) | _ -> Stuck)
      | G.List_branch (empty, full) -> (match a.accumulator with
        | R.V.Nil -> Running ({a with pc = empty}, frames)
        | R.V.Cons (head, tail) -> Running ({a with pc = full; env = R.V.Bind (head, R.V.Bind (tail, a.env));
            temporaries = Environment (a.env, a.temporaries)}, frames)
        | _ -> Stuck)
      | G.Return -> (match a.temporaries, frames with
        | Empty, Halt -> Done a.accumulator
        | Empty, Frame (saved, rest) -> Running ({saved with accumulator = a.accumulator}, rest)
        | _ -> Stuck))
let[@def] (initial @ total) (program : C.program @ immutable) (input : Hmc_word64.t @ immutable) =
  let id = program.C.origin.P.entry in
  match K.lookup program.C.origin.P.table id, C.lookup program.C.functions id with
  | Some entry, Some code ->
    let current = R.V.Closure (id, R.V.Empty) in
    let captured = if entry.K.recursive then R.V.Bind (current, R.V.Empty) else R.V.Empty in
    Running ({pc = code.C.start; env = R.V.Bind (R.V.Word input, captured); accumulator = R.V.Nil;
      temporaries = Empty; current}, Halt)
  | _ -> Stuck
let[@def] rec (advance @ total) (program : C.program @ immutable) (fuel : D.index @ immutable) (state : state @ immutable) =
  match fuel with D.Z -> state | D.S n -> advance program n (step program state)
