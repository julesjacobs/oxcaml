module B = Wasm_u32
module C = Wasm_code
module Plan = Wasm_parallel_copy
module Copy = Wasm_cross_copy
module Prefix = Wasm_cross_copy_prefix
module Instruction_prefix = Wasm_instruction_prefix
module Access = Wasm_memory_region
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module M = Wasm_memory
module Bounds = Hmc_linear_bounds
module Bytes = Hmc_linear_bytes
module P = Hmc_linear_preservation
module V = Hmc_tagged_cell

let rec (emit @ total) : (plan : Plan.plan) @ immutable -> (base_local : B.u32) ->
    {u : unit | Plan.emit plan base_local === Copy.emit plan base_local base_local} @ ghost = fun plan base_local -> ghost_ (
      Plan.emit_def plan base_local; Copy.emit_def plan base_local base_local;
      match plan with Plan.End -> () | Plan.Copy (_, _, rest) -> emit rest base_local)

let rec (apply @ total) : (plan : Plan.plan) @ immutable -> (memory : B.bytes) @ immutable -> (base : B.u32) ->
    {u : unit | Plan.apply plan memory base === Copy.apply plan memory base base} @ ghost = fun plan memory base -> ghost_ (
      Plan.apply_def plan memory base; Copy.apply_def plan memory base base;
      match plan with Plan.End -> () | Plan.Copy (_, _, rest) -> apply rest memory base)

let (region @ total) : (plan : Plan.plan) @ immutable -> (base_local : B.u32) ->
    (state : X.state) @ immutable -> (base : B.u32) -> (after : X.state) @ immutable ->
    (low : B.u32) -> (high : B.u32) -> (fuel : C.count) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && X.run (Plan.emit plan base_local) state === X.Done after
      && Prefix.bounded plan base low high && Bounds.covers state.X.memory low} ->
    {u : unit | match X.run (Instruction_prefix.take fuel (Plan.emit plan base_local)) state with
      | X.Done current -> current.X.machine.E.locals === state.X.machine.E.locals
        && Prefix.partial plan state.X.memory base base current.X.memory
        && P.equal_prefix low state.X.memory current.X.memory
        && Bytes.drop state.X.memory high === Bytes.drop current.X.memory high
        && V.length current.X.memory === V.length state.X.memory
      | _ -> false} @ ghost = fun plan base_local state base after low high fuel premise -> ghost_ (
        emit plan base_local;
        Prefix.reflect plan base_local base_local state base base after ();
        Prefix.region plan base_local base_local state base base after.X.memory low high fuel ())

let (accesses @ total) : (plan : Plan.plan) @ immutable -> (base_local : B.u32) ->
    (state : X.state) @ immutable -> (base : B.u32) -> (after : X.state) @ immutable ->
    (bounds : Access.bounds) @ immutable -> (fuel : C.count) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && X.run (Plan.emit plan base_local) state === X.Done after
      && Prefix.access_bounds plan base base bounds} ->
    {u : unit | Access.trace (Instruction_prefix.take fuel (Plan.emit plan base_local)) state bounds} @ ghost =
    fun plan base_local state base after bounds fuel premise -> ghost_ (
      emit plan base_local;
      Prefix.reflect plan base_local base_local state base base after ();
      Prefix.prefix_accesses plan base_local base_local state base base after.X.memory bounds fuel ())
