module C = Wasm_code
module T = Wasm_control
module E = Wasm_execution
module Lift = Wasm_control_lift
module V = Wasm_static_types
module Check = Wasm_static_control
let[@def] rec (check @ total) (context : V.context @ immutable) (code : C.t @ immutable)
    (before : V.state @ immutable) : V.state option @ immutable = match code with
  | C.Empty -> Some before
  | C.Next (op, rest) -> match V.instruction context op before with None -> None | Some middle -> check context rest middle
let rec (append @ total) : (context : V.context) @ immutable -> (left : C.t) @ immutable -> (right : C.t) @ immutable ->
    (before : V.state) @ immutable ->
    {u : unit | check context (E.append left right) before ===
      (match check context left before with None -> None | Some middle -> check context right middle)} @ ghost =
  fun context left right before -> ghost_ (
    E.append_def left right; check_def context (E.append left right) before; check_def context left before;
    match left with
    | C.Empty -> ()
    | C.Next (op, rest) -> match V.instruction context op before with None -> () | Some middle -> append context rest right middle)
let rec (embed @ total) : (context : V.context) @ immutable -> (labels : Check.labels) @ immutable ->
    (code : C.t) @ immutable -> (tail : T.code) @ immutable -> (before : V.state) @ immutable ->
    {u : unit | Lift.straight code} ->
    {u : unit | Check.check context labels (Lift.embed code tail) before ===
      (match check context code before with None -> None | Some middle -> Check.check context labels tail middle)} @ ghost =
  fun context labels code tail before premise -> ghost_ (
    Lift.straight_def code; Lift.embed_def code tail; check_def context code before;
    match code with
    | C.Empty -> ()
    | C.Next (op, rest) ->
      Lift.ordinary_def op;
      Check.check_def context labels (Lift.embed code tail) before;
      match V.instruction context op before with None -> () | Some middle -> embed context labels rest tail middle ())
let rec (embed_checked @ total) : (context : V.context) @ immutable -> (labels : Check.labels) @ immutable ->
    (code : C.t) @ immutable -> (tail : T.code) @ immutable -> (before : V.state) @ immutable -> (after : V.state) @ immutable ->
    {u : unit | check context code before === Some after} ->
    {u : unit | Check.check context labels (Lift.embed code tail) before === Check.check context labels tail after} @ ghost =
  fun context labels code tail before after premise -> ghost_ (
    check_def context code before; Lift.embed_def code tail;
    match code with
    | C.Empty -> ()
    | C.Next (op, rest) ->
      V.instruction_def context op before;
      Check.check_def context labels (Lift.embed code tail) before;
      match V.instruction context op before with
      | None -> ()
      | Some middle -> embed_checked context labels rest tail middle after ())
