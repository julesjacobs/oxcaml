module C = Wasm_code
module M = Wasm_calls
module F = Wasm_functions
module Budget = Wasm_execution_budget
let rec (running @ total) : (prefix : C.count) @ immutable -> (full : C.count) @ immutable ->
    (module_ : F.module_) @ immutable -> (before : M.configuration) @ immutable -> (after : M.configuration) @ immutable ->
    {u : unit | Budget.le prefix full && M.run full module_ before === M.Running after} ->
    {u : unit | match M.run prefix module_ before with M.Running _ -> true | _ -> false} @ ghost =
  fun prefix full module_ before after premise -> ghost_ (
    Budget.le_def prefix full; M.run_def prefix module_ before; M.run_def full module_ before;
    match prefix, full with
    | C.Succ prefix, C.Succ full ->
      (match M.step module_ before with M.Running next -> running prefix full module_ next after () | _ -> ())
    | _ -> ())
let rec (finished @ total) : (prefix : C.count) @ immutable -> (full : C.count) @ immutable ->
    (module_ : F.module_) @ immutable -> (before : M.configuration) @ immutable -> (after : Wasm_global_execution.state) @ immutable ->
    {u : unit | M.run full module_ before === M.Finished after} ->
    {u : unit | match M.run prefix module_ before with M.Running _ -> true | terminal -> terminal === M.Finished after} @ ghost =
  fun prefix full module_ before after premise -> ghost_ (
    M.run_def prefix module_ before; M.run_def full module_ before;
    match prefix, full with
    | C.Succ prefix, C.Succ full ->
      (match M.step module_ before with M.Running next -> finished prefix full module_ next after () | _ -> ())
    | _ -> ())
