module D = Hm_declarative
module W = Hmc_word64
module I = Hmc_tail_ir
module M = Hmc_monomorphic
module C = Hmc_cfg_program
module O = Hmc_closure_program
module Source = Hmc_source_semantics
module V = Hm_interpreter_typing
module Origin = Hmc_monomorphic_simulation
module Tail = Hmc_tail_simulation
module U = Hmc_tail_semantics
module State = Hmc_wasm_program_state
module Registers = Hmc_wasm_program_registers
module Resources = Hmc_wasm_program_resources
module Lower = Hmc_wasm_program_lower
module Machine = Hmc_heap_machine
module Inv = Hmc_heap_invariant
module Runs = Hmc_heap_runs
module E = Hmc_wasm_program_execution
module Result = Hmc_wasm_program_step_result
module Resource = Hmc_wasm_program_resource_step
module Q = Hmc_heap_state
module F = Hmc_heap_frame
module Cell = Hmc_tagged_cell
let[@def] (returned @ total) (endpoint : E.endpoint @ immutable) (word : W.t @ immutable) = ghost_ (
  match endpoint with
  | E.Stopped (before, Result.Returned _) -> before.State.activation.F.accumulator === Cell.Word word
  | _ -> false)
let[@def] (exhausted @ total) (endpoint : E.endpoint @ immutable) =
  match endpoint with E.Stopped (_, Result.Exhausted _) -> true | _ -> false
let (reflection @ total) : (program : I.program) @ immutable ->
    (definitions : {d : M.definitions | M.origins d}) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable ->
    (endpoint : E.endpoint) @ immutable -> (word : W.t) @ immutable ->
    {u : unit | program.I.origin.C.origin.O.origin.M.definitions === definitions
      && E.valid program globals lowered context endpoint && returned endpoint word} ->
    {n : D.index | Source.advance n (Origin.source_start program.I.origin.C.origin.O.origin context.State.input)
      === Source.Done (V.Word word)} @ immutable =
  fun program definitions globals lowered context endpoint word premise ->
    ghost_ (E.valid_def program globals lowered context endpoint; returned_def endpoint word);
    match endpoint with
    | E.Stopped (before, Result.Returned final) ->
      ghost_ (State.valid_def program globals lowered context before;
        Result.correct_def program globals lowered context before (Result.Returned final);
        Inv.valid_def program globals before.State.registers.Registers.heap_limit
          {Machine.heap = before.State.heap; state = Q.Done before.State.activation.F.accumulator} (U.step program before.State.abstract);
        Hmc_heap_execute.word_agreement before.State.heap (Q.Done before.State.activation.F.accumulator) word;
        Hmc_heap_reachable_operands.advance_next program context.State.input before.State.elapsed);
      Tail.source_reflection program definitions context.State.input word (D.S before.State.elapsed) ()
    | _ -> unreachable_ ()
let (preservation @ total) : (program : I.program) @ immutable ->
    (definitions : {d : M.definitions | M.origins d}) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable ->
    (word : W.t) @ immutable -> (source_fuel : D.index) @ immutable ->
    {u : unit | program.I.origin.C.origin.O.origin.M.definitions === definitions
      && State.valid program globals lowered context before && before.State.elapsed === D.Z
      && Source.advance source_fuel (Origin.source_start program.I.origin.C.origin.O.origin context.State.input)
        === Source.Done (V.Word word)} ->
    {out : E.execution | E.valid program globals lowered context out.E.endpoint
      && (returned out.E.endpoint word || exhausted out.E.endpoint)
      && Wasm_calls.run out.E.fuel (State.module_ program lowered context) (State.loop context before)
        === E.target context out.E.endpoint
      && Wasm_calls.run out.E.prefix (State.module_ program lowered context) (State.loop context before)
        === Wasm_calls.Running (E.checkpoint context out.E.endpoint)} @ immutable =
  fun program definitions globals lowered context before word source_fuel premise ->
    let budget = Tail.source_preservation program definitions context.State.input word source_fuel () in
    ghost_ (State.valid_def program globals lowered context before;
      U.advance_def program D.Z (U.initial program context.State.input);
      State.configuration_def before;
      Resources.valid_def program globals lowered.Lower.width context.State.stack_base before.State.frame_end before.State.abstract
        before.State.heap before.State.activation before.State.frames before.State.registers before.State.memory;
      Runs.correct program globals before.State.registers.Registers.heap_limit context.State.stack_capacity budget
        (State.configuration before) before.State.abstract ());
    let out = E.run budget program globals lowered context before () in
    ghost_ (E.valid_def program globals lowered context out.E.endpoint;
      E.source_def out.E.endpoint out.E.steps; returned_def out.E.endpoint word; exhausted_def out.E.endpoint;
      match out.E.endpoint with
      | E.Paused state ->
        State.configuration_def state;
        Inv.valid_def program globals before.State.registers.Registers.heap_limit (State.configuration state)
          (U.advance program budget before.State.abstract);
        Hmc_heap_execute.word_agreement state.State.heap (Q.Running (state.State.activation, state.State.frames)) word
      | E.Stopped (last, result) ->
        Result.correct_def program globals lowered context last result;
        match result with
        | Result.Returned _ ->
          Inv.valid_def program globals before.State.registers.Registers.heap_limit
            {Machine.heap = last.State.heap; state = Q.Done last.State.activation.F.accumulator} (U.advance program budget before.State.abstract);
          Hmc_heap_execute.word_agreement last.State.heap (Q.Done last.State.activation.F.accumulator) word
        | _ -> ());
    out
let (normal @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable ->
    (word : W.t) @ immutable -> (budget : D.index) @ immutable ->
    {u : unit | State.valid program globals lowered context before
      && U.advance program budget before.State.abstract === Hmc_cfg_semantics.Done (Hmc_closure_semantics.V.Word word)
      && Hmc_heap_extent.fits (Hmc_heap_demand.heap_plan program budget before.State.abstract)
        (Hmc_heap_objects.used before.State.heap) before.State.registers.Registers.heap_limit
      && Hmc_frame_capacity.le (Hmc_heap_demand.stack_plan program budget before.State.abstract) context.State.stack_capacity} ->
    {out : E.execution | E.valid program globals lowered context out.E.endpoint && returned out.E.endpoint word
      && Wasm_calls.run out.E.fuel (State.module_ program lowered context) (State.loop context before)
        === E.target context out.E.endpoint
      && Wasm_calls.run out.E.prefix (State.module_ program lowered context) (State.loop context before)
        === Wasm_calls.Running (E.checkpoint context out.E.endpoint)} @ immutable =
  fun program globals lowered context before word budget premise ->
    ghost_ (State.valid_def program globals lowered context before; State.configuration_def before;
      Resources.valid_def program globals lowered.Lower.width context.State.stack_base before.State.frame_end before.State.abstract
        before.State.heap before.State.activation before.State.frames before.State.registers before.State.memory);
    let final = Hmc_heap_resources.sufficient program globals before.State.registers.Registers.heap_limit context.State.stack_capacity
      budget (State.configuration before) before.State.abstract () in
    let out = E.run budget program globals lowered context before () in
    ghost_ (Inv.valid_def program globals before.State.registers.Registers.heap_limit final (U.advance program budget before.State.abstract);
      Hmc_heap_execute.word_agreement final.Machine.heap final.Machine.state word;
      E.valid_def program globals lowered context out.E.endpoint;
      E.source_def out.E.endpoint out.E.steps; returned_def out.E.endpoint word;
      match out.E.endpoint with
      | E.Paused state -> State.configuration_def state
      | E.Stopped (last, result) -> Result.correct_def program globals lowered context last result);
    out
