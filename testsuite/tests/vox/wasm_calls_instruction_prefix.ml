module C = Wasm_code
module X = Wasm_memory_execution
module T = Wasm_control
module Lift = Wasm_control_lift
module Success = Wasm_control_success
module P = Wasm_instance_control
module Instance = Wasm_instance_body
module M = Wasm_calls
module Calls = Wasm_calls_body
module F = Wasm_functions
module Prefix = Wasm_instruction_prefix

let[@def] rec (drop @ total) (fuel : C.count @ immutable) (code : C.t @ immutable) = ghost_ (
  match fuel, code with C.Succ rest, C.Next (_, tail) -> drop rest tail | _ -> code)

let rec (split @ total) : (fuel : C.count) @ immutable -> (code : C.t) @ immutable -> (tail : T.code) @ immutable ->
    {u : unit | Lift.embed code tail === Lift.embed (Prefix.take fuel code) (Lift.embed (drop fuel code) tail)} @ ghost =
  fun fuel code tail -> ghost_ (
    Prefix.take_def fuel code; drop_def fuel code;
    Lift.embed_def code tail;
    Lift.embed_def (Prefix.take fuel code) (Lift.embed (drop fuel code) tail);
    match fuel, code with C.Succ rest, C.Next (_, next) -> split rest next tail | _ -> ())

let (correct @ total) : (fuel : C.count) @ immutable -> (code : C.t) @ immutable -> (tail : T.code) @ immutable ->
    (module_ : F.module_) @ immutable -> (configuration : M.configuration) @ immutable -> (after : X.state) @ immutable ->
    {u : unit | configuration.M.current.P.body.T.code === Lift.embed code tail
      && X.run (Prefix.take fuel code) configuration.M.current.P.body.T.state === X.Done after} ->
    {u : unit | M.run (C.length (Prefix.take fuel code)) module_ configuration === M.Running
      {configuration with M.current = {configuration.M.current with P.body =
        {T.code = Lift.embed (drop fuel code) tail; labels = configuration.M.current.P.body.T.labels; state = after}}}} @ ghost =
  fun fuel code tail module_ configuration after premise -> ghost_ (
    let prefix = Prefix.take fuel code in
    let remainder = Lift.embed (drop fuel code) tail in
    let before = configuration.M.current.P.body in
    let finished = {T.code = remainder; labels = before.T.labels; state = after} in
    split fuel code tail;
    Success.straight prefix before.T.state after ();
    Lift.correct prefix remainder before.T.labels before.T.state after ();
    Instance.run (C.length prefix) before finished configuration.M.current.P.globals ();
    Calls.run (C.length prefix) module_ configuration {P.body = finished; globals = configuration.M.current.P.globals} ())
