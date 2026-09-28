module B = Wasm_u32
module T = Wasm_control
module X = Wasm_memory_execution
let[@def] rec (valid @ total) (depth : B.u32) (labels : T.labels @ immutable) = match labels with
  | T.No_labels -> false
  | T.Label (_, outer) -> if depth = 0 then true else valid (depth - 1) outer
let rec (target @ total) : (depth : B.u32) -> (labels : T.labels) @ immutable -> (state : X.state) @ immutable ->
    {u : unit | valid depth labels} ->
    {out : T.configuration | T.branch depth labels state === T.Running out} @ immutable =
  fun depth labels state premise ->
    ghost_ (valid_def depth labels; T.branch_def depth labels state);
    match labels with
    | T.No_labels -> unreachable_ ()
    | T.Label (label, outer) ->
      if depth <> 0 then target (depth - 1) outer state () else
      match label.T.restart with
      | None -> {T.code = label.T.continuation; labels = outer; state = T.stack state label.T.saved}
      | Some body -> {T.code = body; labels; state = T.stack state Wasm_scalar.Empty}
let rec (valid_of_running @ total) : (depth : B.u32) -> (labels : T.labels) @ immutable ->
    (state : X.state) @ immutable -> (out : T.configuration) @ immutable ->
    {u : unit | T.branch depth labels state === T.Running out} ->
    {u : unit | valid depth labels} @ ghost = fun depth labels state out premise -> ghost_ (
    T.branch_def depth labels state; valid_def depth labels;
    match labels with
    | T.No_labels -> ()
    | T.Label (_, outer) -> if depth = 0 then () else valid_of_running (depth - 1) outer state out ())
