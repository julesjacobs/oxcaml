module I = Wasm_instruction
module C = Wasm_code
module X = Wasm_memory_execution
module T = Wasm_control

let[@def] (ordinary @ total) (instruction : I.t @ immutable) =
  match instruction with
  | I.Block | I.Loop | I.If | I.Br _ | I.Br_if _
  | I.Plain I.End | I.Plain I.Else | I.Plain I.Unreachable -> false
  | _ -> true
let[@def] rec (straight @ total) (code : C.t @ immutable) =
  match code with C.Empty -> true | C.Next (instruction, rest) -> ordinary instruction && straight rest
let[@def] rec (embed @ total) (code : C.t @ immutable) (tail : T.code @ immutable) : T.code @ immutable =
  match code with C.Empty -> tail | C.Next (instruction, rest) -> T.Instruction (instruction, embed rest tail)
let rec (correct @ total) : (code : C.t) @ immutable -> (tail : T.code) @ immutable ->
    (labels : T.labels) @ immutable -> (before : X.state) @ immutable -> (after : X.state) @ immutable ->
    {u : unit | straight code && X.run code before === X.Done after} ->
    {u : unit | T.run (C.length code) {T.code = embed code tail; labels; state = before}
      === T.Running {T.code = tail; labels; state = after}} @ ghost =
  fun code tail labels before after premise -> ghost_ (
    straight_def code; embed_def code tail; C.length_def code; X.run_def code before;
    let configuration = {T.code = embed code tail; labels; state = before} in
    T.run_def (C.length code) configuration;
    match code with
    | C.Empty -> ()
    | C.Next (instruction, rest) ->
      ordinary_def instruction; T.step_def configuration;
      (match X.step instruction before with
      | X.Done next -> correct rest tail labels next after ()
      | _ -> ()))
