module C = Wasm_code
module E = Wasm_execution
module X = Wasm_memory_execution

let[@def] rec (take @ total) (fuel : C.count @ immutable) (code : C.t @ immutable) =
  ghost_ (match fuel, code with
    | C.Succ fuel, C.Next (instruction, rest) -> C.Next (instruction, take fuel rest)
    | _ -> C.Empty)

let[@def] rec (remaining @ total) (fuel : C.count @ immutable) (code : C.t @ immutable) =
  ghost_ (match code, fuel with
    | C.Empty, _ -> Some fuel
    | C.Next (_, rest), C.Succ fuel -> remaining fuel rest
    | _ -> None)

let rec (append @ total) : (fuel : C.count) @ immutable -> (first : C.t) @ immutable ->
    (second : C.t) @ immutable ->
    {u : unit | take fuel (E.append first second) === (match remaining fuel first with
      | None -> take fuel first | Some rest -> E.append first (take rest second))} @ ghost =
  fun fuel first second -> ghost_ (
    E.append_def first second; remaining_def fuel first;
    take_def fuel (E.append first second); take_def fuel first;
    match first, fuel with
    | C.Empty, _ -> E.append_def C.Empty (take fuel second)
    | C.Next (instruction, tail), C.Succ rest ->
      append rest tail second;
      (match remaining rest tail with None -> () | Some left -> E.append_def first (take left second))
    | _ -> ())

let (run_append @ total) : (fuel : C.count) @ immutable -> (first : C.t) @ immutable ->
    (second : C.t) @ immutable -> (state : X.state) @ immutable -> (middle : X.state) @ immutable ->
    {u : unit | X.run first state === X.Done middle} ->
    {u : unit | X.run (take fuel (E.append first second)) state === (match remaining fuel first with
      | None -> X.run (take fuel first) state | Some rest -> X.run (take rest second) middle)} @ ghost =
  fun fuel first second state middle premise -> ghost_ (
    append fuel first second;
    match remaining fuel first with None -> ()
    | Some rest -> X.append_correct first (take rest second) state)
