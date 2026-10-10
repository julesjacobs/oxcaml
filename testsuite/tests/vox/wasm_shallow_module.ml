module B = Wasm_u32
module C = Wasm_code
module F = Wasm_functions
module M = Wasm_calls
module T = Wasm_control
module P = Wasm_instance_control
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module G = Wasm_globals
module Policy = Wasm_call_policy
module Bound = Wasm_shallow_calls
type payload = {module_ : F.module_; entry : B.u32}
let[@def] (valid @ total) (program : payload @ immutable) = ghost_ (
  Bound.table program.module_.F.functions program.module_.F.table
  && match F.lookup program.module_.F.functions program.entry with
    | None -> false | Some function_ -> Policy.code Policy.Indirect_calls function_.F.code)
type program = {p : payload | valid p}
let (check @ total) : (module_ : F.module_) @ immutable -> (entry : B.u32) ->
    {out : program option | match out with None -> true | Some program -> program.module_ === module_ && program.entry = entry} @ immutable =
  fun module_ entry ->
  if not (Bound.table module_.F.functions module_.F.table) then None else
  match F.lookup module_.F.functions entry with
  | None -> None
  | Some function_ -> if not (Policy.code Policy.Indirect_calls function_.F.code) then None else
    let payload = {module_; entry} in
    ghost_ (valid_def payload); let program : program = refine_ payload in Some program
let[@def] (capacity @ total) (unit : unit) = C.Succ C.Zero
let (start @ total) : (program : program) @ immutable -> (memory : B.bytes) @ immutable -> (globals : G.t) @ immutable ->
    {out : M.configuration | Bound.bounded out
      && M.start program.module_ program.entry memory globals (capacity ()) === M.Running out} @ immutable =
  fun program memory globals ->
  ghost_ (valid_def program; capacity_def ());
  match F.lookup program.module_.F.functions program.entry with
  | None -> unreachable_ ()
  | Some function_ ->
    let out = {M.result = function_.F.result; callers = M.Root; capacity = capacity ();
      current = {P.globals; body = {T.code = function_.F.code; labels = T.No_labels;
        state = {X.memory; machine = {E.locals = F.zero_locals function_.F.locals; stack = S.Empty}}}}} in
    ghost_ (M.start_def program.module_ program.entry memory globals (capacity ());
      Bound.bounded_def out; Policy.configuration_def Policy.Indirect_calls out.M.current.P.body;
      Policy.labels_def Policy.Indirect_calls T.No_labels);
    out
let (execution_available @ total) : (program : program) @ immutable -> (initial : M.configuration) @ immutable ->
    (fuel : C.count) @ immutable -> {u : unit | Bound.bounded initial} ->
    {u : unit | not (M.run fuel program.module_ initial === M.Host_limit)} @ ghost = fun program initial fuel premise -> ghost_ (
  valid_def program; Bound.run_available fuel program.module_ initial ())
