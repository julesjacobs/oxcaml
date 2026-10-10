module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module M = Wasm_memory
module X = Wasm_memory_execution
module Prefix = Wasm_instruction_prefix

type bounds = {read_low : B.u32; read_high : B.u32; write_low : B.u32; write_high : B.u32}

let[@def] (read @ total) (width : M.width @ immutable) (alignment : B.u32) (offset : B.u32)
    (state : X.state @ immutable) (bounds : bounds @ immutable) = ghost_ (
  alignment <= (match width with M.W32 -> 2 | M.W64 -> 3)
  && match state.X.machine.E.stack with
    | S.Push (S.I32 base, _) -> bounds.read_low <= base + offset
      && base + offset + M.size width <= bounds.read_high
    | _ -> false)

let[@def] (write @ total) (width : M.width @ immutable) (alignment : B.u32) (offset : B.u32)
    (state : X.state @ immutable) (bounds : bounds @ immutable) = ghost_ (
  alignment <= (match width with M.W32 -> 2 | M.W64 -> 3)
  && match state.X.machine.E.stack with
    | S.Push (value, S.Push (S.I32 base, _)) -> M.width value === width
      && bounds.write_low <= base + offset && base + offset + M.size width <= bounds.write_high
    | _ -> false)

let[@def] (access @ total) (instruction : I.t @ immutable) (state : X.state @ immutable)
    (bounds : bounds @ immutable) = ghost_ (match instruction with
  | I.I32_load (alignment, offset) -> read M.W32 alignment offset state bounds
  | I.I64_load (alignment, offset) -> read M.W64 alignment offset state bounds
  | I.I32_store (alignment, offset) -> write M.W32 alignment offset state bounds
  | I.I64_store (alignment, offset) -> write M.W64 alignment offset state bounds
  | _ -> true)

let[@def] rec (trace @ total) (code : C.t @ immutable) (state : X.state @ immutable)
    (bounds : bounds @ immutable) = ghost_ (match code with
  | C.Empty -> true
  | C.Next (instruction, rest) -> access instruction state bounds
    && match X.step instruction state with X.Done next -> trace rest next bounds | _ -> false)

let rec (append @ total) : (first : C.t) @ immutable -> (second : C.t) @ immutable ->
    (state : X.state) @ immutable -> (middle : X.state) @ immutable -> (bounds : bounds) @ immutable ->
    {u : unit | trace first state bounds && trace second middle bounds && X.run first state === X.Done middle} ->
    {u : unit | trace (E.append first second) state bounds} @ ghost = fun first second state middle bounds premise -> ghost_ (
      trace_def first state bounds; trace_def (E.append first second) state bounds;
      E.append_def first second; X.run_def first state;
      match first with C.Empty -> () | C.Next (instruction, rest) ->
        match X.step instruction state with X.Done next -> append rest second next middle bounds () | _ -> ())

let rec (prefix @ total) : (fuel : C.count) @ immutable -> (code : C.t) @ immutable ->
    (state : X.state) @ immutable -> (bounds : bounds) @ immutable ->
    {u : unit | trace code state bounds} ->
    {u : unit | trace (Prefix.take fuel code) state bounds} @ ghost = fun fuel code state bounds premise -> ghost_ (
      Prefix.take_def fuel code; trace_def code state bounds; trace_def (Prefix.take fuel code) state bounds;
      match fuel, code with
      | C.Succ fuel, C.Next (instruction, rest) ->
        (match X.step instruction state with X.Done next -> prefix fuel rest next bounds () | _ -> ())
      | _ -> ())
