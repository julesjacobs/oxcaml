module C = Wasm_code
module T = Wasm_control
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
let[@def] rec (related @ total) (count : C.count @ immutable) (outer : T.labels @ immutable) (inner : T.labels @ immutable) = ghost_ (
  match count with
  | C.Zero -> inner === outer
  | C.Succ rest -> match inner with
    | T.No_labels -> false
    | T.Label (label, tail) -> label.T.restart === None && label.T.continuation === T.Empty && label.T.saved === S.Empty
      && related rest outer tail)
let rec (correct @ total) : (count : C.count) @ immutable -> (outer : T.labels) @ immutable -> (inner : T.labels) @ immutable ->
    (state : X.state) @ immutable -> {u : unit | related count outer inner && state.X.machine.E.stack === S.Empty} ->
    {u : unit | T.run count {T.code = T.Empty; labels = inner; state} === T.Running {T.code = T.Empty; labels = outer; state}} @ ghost =
  fun count outer inner state premise -> ghost_ (
    related_def count outer inner; T.run_def count {T.code = T.Empty; labels = inner; state};
    match count with
    | C.Zero -> ()
    | C.Succ rest -> match inner with
      | T.No_labels -> ()
      | T.Label (_, tail) ->
        T.step_def {T.code = T.Empty; labels = inner; state}; T.stack_def state S.Empty;
        correct rest outer tail state ())
let rec (extend @ total) : (count : C.count) @ immutable -> (outer : T.labels) @ immutable -> (inner : T.labels) @ immutable ->
    {u : unit | related count (T.Label ({T.restart = None; continuation = T.Empty; saved = S.Empty}, outer)) inner} ->
    {u : unit | related (C.Succ count) outer inner} @ ghost = fun count outer inner premise -> ghost_ (
  related_def count (T.Label ({T.restart = None; continuation = T.Empty; saved = S.Empty}, outer)) inner;
  related_def (C.Succ count) outer inner;
  match count with
  | C.Zero -> related_def C.Zero outer outer
  | C.Succ rest -> match inner with
    | T.No_labels -> ()
    | T.Label (_, tail) -> extend rest outer tail ())
