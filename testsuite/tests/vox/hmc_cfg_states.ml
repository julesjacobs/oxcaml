module D = Hm_declarative
module K = Hmc_closure_ir
module P = Hmc_closure_program
module R = Hmc_closure_semantics
module G = Hmc_cfg_ir
module C = Hmc_cfg_program
module O = Hmc_cfg_origin
module S = Hmc_cfg_semantics

type operation = Apply | Cons | Primitive of D.word_operation [@@inductive]
let[@def] (operation @ total) (op : operation @ immutable) (next : D.index @ immutable) = match op with
  | Apply -> G.Call next | Cons -> G.Cons next | Primitive op -> G.Primitive (op, next)
type continuation =
  | Halt of R.V.value * R.V.value * D.index
  | Left of operation * K.term * O.t * D.index * D.index * continuation
  | Right of operation * R.V.value * D.index * continuation
  | Let_body of K.term * O.t * D.index * D.index * continuation
  | Scope of R.V.value * D.index * continuation
  | Conditional of K.term * K.term * O.t * O.t * D.index * continuation
  | List_cases of K.term * K.term * O.t * O.t * D.index * D.index * continuation
  | Call_return of R.V.value * R.V.value * D.index * R.V.value * continuation
  [@@inductive]
let[@def] (resume @ total) (k : continuation @ immutable) = match k with
  | Halt (_, _, label) | Left (_, _, _, label, _, _) | Right (_, _, label, _)
  | Let_body (_, _, label, _, _) | Scope (_, label, _) | Conditional (_, _, _, _, label, _)
  | List_cases (_, _, _, _, label, _, _) | Call_return (_, _, label, _, _) -> label
let[@def] rec (environment @ total) (k : continuation @ immutable) = match k with
  | Halt (env, _, _) | Scope (env, _, _) | Call_return (env, _, _, _, _) -> env
  | Left (_, _, _, _, _, rest) | Right (_, _, _, rest) | Let_body (_, _, _, _, rest)
  | Conditional (_, _, _, _, _, rest) | List_cases (_, _, _, _, _, _, rest) -> environment rest
let[@def] rec (current @ total) (k : continuation @ immutable) = match k with
  | Halt (_, closure, _) | Call_return (_, closure, _, _, _) -> closure
  | Left (_, _, _, _, _, rest) | Right (_, _, _, rest) | Let_body (_, _, _, _, rest)
  | Scope (_, _, rest) | Conditional (_, _, _, _, _, rest) | List_cases (_, _, _, _, _, _, rest) -> current rest
let[@def] rec (temporaries @ total) (k : continuation @ immutable) = match k with
  | Halt _ | Call_return _ -> S.Empty
  | Left (_, _, _, _, _, rest) | Let_body (_, _, _, _, rest) | Scope (_, _, rest) ->
    S.Environment (environment rest, temporaries rest)
  | Right (_, value, _, rest) -> S.Value (value, environment rest, temporaries rest)
  | Conditional (_, _, _, _, _, rest) | List_cases (_, _, _, _, _, _, rest) -> temporaries rest
let[@def] (saved @ total) (k : continuation @ immutable) (argument : R.V.value @ immutable) =
  {S.pc = resume k; env = environment k; accumulator = argument; temporaries = temporaries k; current = current k}
let[@def] rec (frames @ total) (k : continuation @ immutable) = match k with
  | Halt _ -> S.Halt
  | Call_return (_, _, _, argument, rest) -> S.Frame (saved rest argument, frames rest)
  | Left (_, _, _, _, _, rest) | Right (_, _, _, rest) | Let_body (_, _, _, _, rest)
  | Scope (_, _, rest) | Conditional (_, _, _, _, _, rest) | List_cases (_, _, _, _, _, _, rest) -> frames rest
let[@def] rec (source_continuation @ total) (k : continuation @ immutable) = match k with
  | Halt _ -> R.Halt
  | Scope (_, _, rest) | Call_return (_, _, _, _, rest) -> source_continuation rest
  | Left (op, term, _, _, _, rest) -> (match op with
    | Apply -> R.Apply_function (environment rest, term, source_continuation rest)
    | Cons -> R.Cons_head (environment rest, term, source_continuation rest)
    | Primitive op -> R.Primitive_left (op, environment rest, term, source_continuation rest))
  | Right (op, value, _, rest) -> (match op with
    | Apply -> R.Apply_argument (value, source_continuation rest)
    | Cons -> R.Cons_tail (value, source_continuation rest)
    | Primitive op -> R.Primitive_right (op, value, source_continuation rest))
  | Let_body (term, _, _, _, rest) -> R.Let_body (environment rest, term, source_continuation rest)
  | Conditional (yes, no, _, _, _, rest) -> R.Conditional (environment rest, yes, no, source_continuation rest)
  | List_cases (empty, full, _, _, _, _, rest) -> R.List_cases (environment rest, empty, full, source_continuation rest)

let[@def] rec (continuation_valid @ total) (blocks : G.table @ immutable) (k : continuation @ immutable) = ghost_ (match k with
  | Halt (_, _, label) -> O.instruction blocks label G.Return
  | Left (op, term, trace, save, finish, rest) -> continuation_valid blocks rest
    && O.instruction blocks save (G.Save_value (O.entry trace))
    && O.generated blocks term finish trace && O.instruction blocks finish (operation op (resume rest))
  | Right (op, _, finish, rest) -> continuation_valid blocks rest && O.instruction blocks finish (operation op (resume rest))
  | Let_body (term, trace, bind, restore, rest) -> continuation_valid blocks rest
    && O.instruction blocks bind (G.Bind (O.entry trace)) && O.generated blocks term restore trace
    && O.instruction blocks restore (G.Restore (resume rest))
  | Scope (_, restore, rest) -> continuation_valid blocks rest && O.instruction blocks restore (G.Restore (resume rest))
  | Conditional (yes, no, yt, nt, branch, rest) -> continuation_valid blocks rest
    && O.instruction blocks branch (G.Branch (O.entry yt, O.entry nt))
    && O.generated blocks yes (resume rest) yt && O.generated blocks no (resume rest) nt
  | List_cases (empty, full, et, ft, branch, restore, rest) -> continuation_valid blocks rest
    && O.instruction blocks branch (G.List_branch (O.entry et, O.entry ft))
    && O.generated blocks empty (resume rest) et && O.generated blocks full restore ft
    && O.instruction blocks restore (G.Restore (resume rest))
  | Call_return (_, _, label, _, rest) -> continuation_valid blocks rest && O.instruction blocks label G.Return)

type control = Evaluate of K.term * O.t | Returning [@@inductive]
type state = Running of control * continuation * R.V.value | Done of R.V.value | Stuck [@@inductive]
let[@def] (source @ total) (state : state @ immutable) = match state with
  | Done value -> R.Done value | Stuck -> R.Stuck
  | Running (control, k, accumulator) -> R.Running ((match control with
    | Evaluate (term, _) -> R.Evaluate (environment k, term) | Returning -> R.Return accumulator), source_continuation k)
let[@def] (activation @ total) (control : control @ immutable) (k : continuation @ immutable) (accumulator : R.V.value @ immutable) =
  {S.pc = (match control with Evaluate (_, trace) -> O.entry trace | Returning -> resume k);
   env = environment k; accumulator; temporaries = temporaries k; current = current k}
let[@def] (target @ total) (state : state @ immutable) = match state with
  | Done value -> S.Done value | Stuck -> S.Stuck
  | Running (control, k, accumulator) -> S.Running (activation control k accumulator, frames k)
let[@def] (valid @ total) (blocks : G.table @ immutable) (state : state @ immutable) = ghost_ (match state with
  | Done _ | Stuck -> true
  | Running (control, k, _) -> continuation_valid blocks k && match control with
    | Returning -> true | Evaluate (term, trace) -> O.generated blocks term (resume k) trace)
let[@def] (source_steps @ total) (state : state @ immutable) = match state with
  | Running (Returning, (Scope _ | Call_return _), _) -> D.Z | _ -> D.S D.Z
