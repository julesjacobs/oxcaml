module B = Wasm_u32
module W = Hmc_word64
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module L = Wasm_locals
module E = Wasm_execution
module M = Wasm_memory
module X = Wasm_memory_execution
module Write = Wasm_immediate_write
module Prefix = Wasm_instruction_prefix
module Region = Wasm_frame_write_prefix
module Access = Wasm_memory_region
module Bounds = Hmc_linear_bounds
module Bytes = Hmc_linear_bytes
module P = Hmc_linear_preservation
module V = Hmc_tagged_cell

let (accesses @ total) : (offset : B.u32) -> (base_local : B.u32) -> (word : W.t) @ immutable ->
    (state : X.state) @ immutable -> (base : B.u32) -> (memory : B.bytes) @ immutable -> (bounds : Access.bounds) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && M.store state.X.memory base offset (S.I64 word) === Some memory
      && bounds.Access.write_low <= base + offset && base + offset + 8 <= bounds.Access.write_high} ->
    {u : unit | Access.trace (Write.emit offset base_local word) state bounds} @ ghost =
  fun offset base_local word state base memory bounds premise -> ghost_ (
    let addressed = {X.memory = state.X.memory; machine = {E.locals = state.X.machine.E.locals;
      stack = S.Push (S.I32 base, state.X.machine.E.stack)}} in
    let operands = {X.memory = state.X.memory; machine = {E.locals = state.X.machine.E.locals;
      stack = S.Push (S.I64 word, addressed.X.machine.E.stack)}} in
    let write = C.Next (I.I64_store (3, offset), C.Empty) in
    let value = C.Next (I.I64_const word, write) in
    Write.emit_def offset base_local word;
    Access.trace_def (Write.emit offset base_local word) state bounds; Access.access_def (I.Local_get base_local) state bounds;
    X.step_def (I.Local_get base_local) state; E.step_def (I.Local_get base_local) state.X.machine;
    Access.trace_def value addressed bounds; Access.access_def (I.I64_const word) addressed bounds;
    X.step_def (I.I64_const word) addressed; E.step_def (I.I64_const word) addressed.X.machine;
    S.step_def (I.I64_const word) addressed.X.machine.E.stack;
    Access.trace_def write operands bounds; Access.access_def (I.I64_store (3, offset)) operands bounds;
    Access.write_def M.W64 3 offset operands bounds; M.size_def M.W64; M.width_def (S.I64 word);
    X.step_def (I.I64_store (3, offset)) operands; X.write_def M.W64 offset operands; X.compatible_def (S.I64 word) M.W64;
    Access.trace_def C.Empty {X.memory; machine = state.X.machine} bounds)

let (prefix @ total) : (offset : B.u32) -> (base_local : B.u32) -> (word : W.t) @ immutable ->
    (state : X.state) @ immutable -> (base : B.u32) -> (memory : B.bytes) @ immutable -> (fuel : C.count) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && M.store state.X.memory base offset (S.I64 word) === Some memory} ->
    {u : unit | match X.run (Prefix.take fuel (Write.emit offset base_local word)) state with
      | X.Done current -> current.X.machine.E.locals === state.X.machine.E.locals
        && (current.X.memory === state.X.memory || current.X.memory === memory)
      | _ -> false} @ ghost = fun offset base_local word state base memory fuel premise -> ghost_ (
    Write.emit_def offset base_local word; Prefix.take_def fuel (Write.emit offset base_local word);
    match fuel with
    | C.Zero -> X.run_def C.Empty state
    | C.Succ fuel1 ->
      let addressed = {X.memory = state.X.memory; machine = {E.locals = state.X.machine.E.locals;
        stack = S.Push (S.I32 base, state.X.machine.E.stack)}} in
      let write = C.Next (I.I64_store (3, offset), C.Empty) in
      let value = C.Next (I.I64_const word, write) in
      X.run_def (Prefix.take fuel (Write.emit offset base_local word)) state;
      X.step_def (I.Local_get base_local) state; E.step_def (I.Local_get base_local) state.X.machine;
      Prefix.take_def fuel1 value;
      match fuel1 with
      | C.Zero -> X.run_def C.Empty addressed
      | C.Succ fuel2 ->
        let operands = {X.memory = state.X.memory; machine = {E.locals = state.X.machine.E.locals;
          stack = S.Push (S.I64 word, addressed.X.machine.E.stack)}} in
        X.run_def (Prefix.take fuel1 value) addressed;
        X.step_def (I.I64_const word) addressed; E.step_def (I.I64_const word) addressed.X.machine;
        S.step_def (I.I64_const word) addressed.X.machine.E.stack;
        Prefix.take_def fuel2 write;
        match fuel2 with
        | C.Zero -> X.run_def C.Empty operands
        | C.Succ fuel3 ->
          Prefix.take_def fuel3 C.Empty; X.run_def write operands;
          X.step_def (I.I64_store (3, offset)) operands; X.write_def M.W64 offset operands;
          M.width_def (S.I64 word); X.compatible_def (S.I64 word) M.W64;
          X.run_def C.Empty {X.memory; machine = state.X.machine})

let (region @ total) : (offset : B.u32) -> (base_local : B.u32) -> (word : W.t) @ immutable ->
    (state : X.state) @ immutable -> (base : B.u32) -> (memory : B.bytes) @ immutable ->
    (low : B.u32) -> (high : B.u32) -> (fuel : C.count) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && M.store state.X.memory base offset (S.I64 word) === Some memory
      && low <= base + offset && base + offset + 8 <= high && Bounds.covers state.X.memory low} ->
    {u : unit | match X.run (Prefix.take fuel (Write.emit offset base_local word)) state with
      | X.Done current -> current.X.machine.E.locals === state.X.machine.E.locals
        && (current.X.memory === state.X.memory || current.X.memory === memory)
        && P.equal_prefix low state.X.memory current.X.memory
        && Bytes.drop state.X.memory high === Bytes.drop current.X.memory high
        && V.length current.X.memory === V.length state.X.memory
      | _ -> false} @ ghost = fun offset base_local word state base memory low high fuel premise -> ghost_ (
    prefix offset base_local word state base memory fuel ();
    Region.reflexive state.X.memory low ();
    Region.store_region state.X.memory memory base offset word low high ())

let (reflect @ total) : (offset : B.u32) -> (base_local : B.u32) -> (word : W.t) @ immutable ->
    (state : X.state) @ immutable -> (base : B.u32) -> (after : X.state) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && X.run (Write.emit offset base_local word) state === X.Done after} ->
    {u : unit | M.store state.X.memory base offset (S.I64 word) === Some after.X.memory
      && after.X.machine === state.X.machine} @ ghost = fun offset base_local word state base after premise -> ghost_ (
    let addressed = {X.memory = state.X.memory; machine = {E.locals = state.X.machine.E.locals;
      stack = S.Push (S.I32 base, state.X.machine.E.stack)}} in
    let operands = {X.memory = state.X.memory; machine = {E.locals = state.X.machine.E.locals;
      stack = S.Push (S.I64 word, addressed.X.machine.E.stack)}} in
    let write = C.Next (I.I64_store (3, offset), C.Empty) in
    let value = C.Next (I.I64_const word, write) in
    Write.emit_def offset base_local word;
    X.run_def (Write.emit offset base_local word) state; X.step_def (I.Local_get base_local) state;
    E.step_def (I.Local_get base_local) state.X.machine;
    X.run_def value addressed; X.step_def (I.I64_const word) addressed;
    E.step_def (I.I64_const word) addressed.X.machine; S.step_def (I.I64_const word) addressed.X.machine.E.stack;
    X.run_def write operands; X.step_def (I.I64_store (3, offset)) operands;
    M.width_def (S.I64 word); X.write_def M.W64 offset operands; X.compatible_def (S.I64 word) M.W64;
    match M.store state.X.memory base offset (S.I64 word) with
    | None -> ()
    | Some memory -> X.run_def C.Empty {X.memory; machine = state.X.machine})
