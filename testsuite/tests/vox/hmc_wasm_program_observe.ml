module I = Hmc_tail_ir
module Machine = Hmc_heap_machine
module Lower = Hmc_wasm_program_lower
module State = Hmc_wasm_program_state
module Registers = Hmc_wasm_program_registers
module Result = Hmc_wasm_program_step_result
module Resource = Hmc_wasm_program_resource_step
module E = Hmc_wasm_program_execution
module Entry = Hmc_wasm_program_state_entry
module Calls = Wasm_calls
module C = Wasm_code
module Budget = Wasm_execution_budget
module Prefix = Wasm_calls_prefix
let[@def] (matches @ total) (context : State.context @ immutable) (endpoint : E.endpoint @ immutable) (observed : Calls.result @ immutable) = ghost_ (
  match observed with
  | Calls.Running _ -> true
  | Calls.Finished _ -> (match endpoint with E.Stopped _ -> true | _ -> false) && observed === E.target context endpoint
  | _ -> false)
let (loop @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable ->
    (prefix : C.count) @ immutable -> {u : unit | State.valid program globals lowered context before} ->
    {out : E.execution | E.valid program globals lowered context out.E.endpoint
      && matches context out.E.endpoint (Calls.run prefix (State.module_ program lowered context) (State.loop context before))
      && Calls.run out.E.prefix (State.module_ program lowered context) (State.loop context before)
        === Calls.Running (E.checkpoint context out.E.endpoint)} @ immutable =
  fun program globals lowered context before prefix premise ->
    let out = E.run (Budget.to_index prefix) program globals lowered context before () in
    ghost_ (E.covers_def (Budget.to_index prefix) out; Budget.inverse prefix;
      E.valid_def program globals lowered context out.E.endpoint;
      E.target_def context out.E.endpoint;
      matches_def context out.E.endpoint (Calls.run prefix (State.module_ program lowered context) (State.loop context before));
      match out.E.endpoint with
      | E.Paused state -> Prefix.running prefix out.E.fuel (State.module_ program lowered context) (State.loop context before) (State.loop context state) ()
      | E.Stopped (last, result) ->
        Result.correct_def program globals lowered context last result;
        match result with
        | Result.Continued _ -> ()
        | Result.Returned final ->
          Result.finished_def last final.Result.registers;
          (match Result.finished last final.Result.registers with
          | Calls.Finished after -> Prefix.finished prefix out.E.fuel (State.module_ program lowered context) (State.loop context before) after ()
          | _ -> ())
        | Result.Exhausted final ->
          Result.finished_def last final.Resource.state.State.registers;
          (match Result.finished last final.Resource.state.State.registers with
          | Calls.Finished after -> Prefix.finished prefix out.E.fuel (State.module_ program lowered context) (State.loop context before) after ()
          | _ -> ()));
    out
let (entry @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable ->
    (prefix : C.count) @ immutable -> {u : unit | State.valid program globals lowered context before} ->
    {out : E.execution | E.valid program globals lowered context out.E.endpoint
      && matches context out.E.endpoint (Calls.run prefix (State.module_ program lowered context) (Entry.configuration context before))
      && Calls.run (C.Succ out.E.prefix) (State.module_ program lowered context) (Entry.configuration context before)
        === Calls.Running (E.checkpoint context out.E.endpoint)} @ immutable =
  fun program globals lowered context before prefix premise ->
    match prefix with
    | C.Zero ->
      let out = E.run Hm_declarative.Z program globals lowered context before () in
      ghost_ (Calls.run_def C.Zero (State.module_ program lowered context) (Entry.configuration context before);
        matches_def context out.E.endpoint (Calls.Running (Entry.configuration context before));
        Entry.execution program globals lowered context before out.E.prefix ()); out
    | C.Succ rest ->
      let out = loop program globals lowered context before rest () in
      ghost_ (Entry.execution program globals lowered context before rest ();
        Entry.execution program globals lowered context before out.E.prefix ()); out
module M = Hmc_monomorphic
module Cfg = Hmc_cfg_program
module Closure = Hmc_closure_program
module Cell = Hmc_tagged_cell
module F = Hmc_heap_frame
module GE = Wasm_global_execution
module S = Wasm_scalar
module Source = Hmc_wasm_program_source_execution
let (reflection @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable ->
    (prefix : C.count) @ immutable -> (after : GE.state) @ immutable -> (registers : Registers.registers) @ immutable ->
    (word : Hmc_word64.t) @ immutable ->
    {u : unit | State.valid program globals lowered context before
      && Calls.run prefix (State.module_ program lowered context) (Entry.configuration context before) === Calls.Finished after
      && after.GE.globals === Registers.globals registers && registers.Registers.status = 1
      && registers.Registers.tag === Cell.tag (Cell.Word word) && registers.Registers.payload === word
      && after.GE.execution.Wasm_memory_execution.machine.Wasm_execution.stack === S.Push (S.I32 registers.Registers.status, S.Empty)} ->
    {n : Hm_declarative.index | Hmc_source_semantics.advance n
      (Hmc_monomorphic_simulation.source_start program.I.origin.Cfg.origin.Closure.origin context.State.input)
      === Hmc_source_semantics.Done (Hm_interpreter_typing.Word word)} @ immutable =
  fun program globals lowered context before prefix after registers word premise ->
    let out = entry program globals lowered context before prefix () in
    let source = program.I.origin.Cfg.origin.Closure.origin in
    ghost_ (M.ready_def source;
      matches_def context out.E.endpoint (Calls.Finished after);
      E.valid_def program globals lowered context out.E.endpoint;
      E.target_def context out.E.endpoint);
    let definitions : {d : M.definitions | M.origins d} = refine_ source.M.definitions in
    match out.E.endpoint with
    | E.Stopped (last, Result.Returned final) ->
      ghost_ (Result.correct_def program globals lowered context last (Result.Returned final);
        Result.finished_def last final.Result.registers;
        Registers.globals_def registers; Registers.globals_def final.Result.registers;
        Registers.values_def registers; Registers.values_def final.Result.registers;
        Cell.tag_def last.State.activation.F.accumulator; Cell.tag_def (Cell.Word word);
        Cell.payload_def last.State.activation.F.accumulator;
        Source.returned_def out.E.endpoint word);
      Source.reflection program definitions globals lowered context out.E.endpoint word ()
    | E.Stopped (last, Result.Exhausted final) ->
      ghost_ (Result.correct_def program globals lowered context last (Result.Exhausted final);
        Result.finished_def last final.Resource.state.State.registers);
      unreachable_ ()
    | _ -> unreachable_ ()

module Guard = Hmc_failed_guard_calls
module Failed = Hmc_failed_guard_model
module X = Wasm_memory_execution
module Exec = Wasm_execution
let (exhaustion @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable ->
    (prefix : C.count) @ immutable -> (after : GE.state) @ immutable -> (resource : Failed.resource) @ immutable ->
    {u : unit | State.valid program globals lowered context before
      && Calls.run prefix (State.module_ program lowered context) (Entry.configuration context before) === Calls.Finished after
      && after.GE.execution.X.machine.Exec.stack === S.Push (S.I32 (Failed.status resource), S.Empty)} ->
    {out : Guard.witness | Guard.reaches resource (State.module_ program lowered context)
      (Entry.configuration context before) out} @ immutable ghost =
  fun program globals lowered context before prefix after resource premise -> ghost_ (
    let out = entry program globals lowered context before prefix () in
    matches_def context out.E.endpoint (Calls.Finished after);
    E.valid_def program globals lowered context out.E.endpoint;
    E.target_def context out.E.endpoint; E.checkpoint_def context out.E.endpoint;
    Failed.status_def resource;
    match out.E.endpoint with
    | E.Stopped (last, Result.Exhausted final) ->
      Result.correct_def program globals lowered context last (Result.Exhausted final);
      Result.finished_def last final.Resource.state.State.registers;
      (match final.Resource.exhausted, final.Resource.failed_guard with
      | Some reason, Guard.Present guard ->
        Failed.status_def Failed.Heap; Failed.status_def Failed.Stack;
        Guard.prepend resource (State.module_ program lowered context) (C.Succ out.E.prefix)
          (Entry.configuration context before) (State.loop context last) guard ()
      | _ -> unreachable_ ())
    | E.Stopped (last, Result.Returned final) ->
      Result.correct_def program globals lowered context last (Result.Returned final);
      Result.finished_def last final.Result.registers;
      unreachable_ ()
    | _ -> unreachable_ ())
