module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module L = Wasm_locals
module M = Wasm_memory
let[@def] (instruction_preserves @ total) (instruction : I.t @ immutable) (local : B.u32) =
  match instruction with I.Local_set destination | I.Local_tee destination -> destination <> local | _ -> true
let[@def] rec (preserves @ total) (code : C.t @ immutable) (local : B.u32) =
  match code with C.Empty -> true | C.Next (instruction, rest) -> instruction_preserves instruction local && preserves rest local
let (step @ total) : (instruction : I.t) @ immutable -> (before : X.state) @ immutable -> (after : X.state) @ immutable -> (local : B.u32) ->
    {u : unit | instruction_preserves instruction local && X.step instruction before === X.Done after} ->
    {u : unit | L.get before.X.machine.E.locals local === L.get after.X.machine.E.locals local} @ ghost =
  fun instruction before after local premise -> ghost_ (
    instruction_preserves_def instruction local; X.step_def instruction before;
    match instruction with
    | I.I32_load (_, offset) -> X.read_def M.W32 offset before
    | I.I64_load (_, offset) -> X.read_def M.W64 offset before
    | I.I32_store (_, offset) -> X.write_def M.W32 offset before
    | I.I64_store (_, offset) -> X.write_def M.W64 offset before
    | _ ->
      E.step_def instruction before.X.machine;
      match instruction, before.X.machine.E.stack with
      | (I.Local_set destination | I.Local_tee destination), S.Push (value, _) ->
        (match L.set before.X.machine.E.locals destination value with
        | None -> ()
        | Some locals -> L.other_local before.X.machine.E.locals destination value locals local ())
      | _ -> ())
let rec (correct @ total) : (code : C.t) @ immutable -> (before : X.state) @ immutable -> (after : X.state) @ immutable -> (local : B.u32) ->
    {u : unit | preserves code local && X.run code before === X.Done after} ->
    {u : unit | L.get before.X.machine.E.locals local === L.get after.X.machine.E.locals local} @ ghost =
  fun code before after local premise -> ghost_ (
    preserves_def code local; X.run_def code before;
    match code with C.Empty -> () | C.Next (instruction, rest) ->
      match X.step instruction before with X.Done middle -> step instruction before middle local (); correct rest middle after local () | _ -> ())
let rec (append @ total) : (first : C.t) @ immutable -> (second : C.t) @ immutable -> (local : B.u32) ->
    {u : unit | preserves first local && preserves second local} ->
    {u : unit | preserves (E.append first second) local} @ ghost =
  fun first second local premise -> ghost_ (
    preserves_def first local; E.append_def first second; preserves_def (E.append first second) local;
    match first with C.Empty -> () | C.Next (_, rest) -> append rest second local ())
