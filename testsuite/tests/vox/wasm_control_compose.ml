module C = Wasm_code
module T = Wasm_control
let[@def] rec (add @ total) (first : C.count @ immutable) (second : C.count @ immutable) =
  match first with C.Zero -> second | C.Succ rest -> C.Succ (add rest second)
let rec (correct @ total) : (first : C.count) @ immutable -> (second : C.count) @ immutable -> (configuration : T.configuration) @ immutable ->
    {u : unit | T.run (add first second) configuration ===
      (match T.run first configuration with T.Running next -> T.run second next | terminal -> terminal)} @ ghost =
  fun first second configuration -> ghost_ (
    add_def first second; T.run_def first configuration; T.run_def (add first second) configuration;
    match first with
    | C.Zero -> ()
    | C.Succ rest -> (match T.step configuration with T.Running next -> correct rest second next | _ -> ()))
