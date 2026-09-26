module D = Hm_declarative
module I = Hmc_tail_ir
module Machine = Hmc_heap_machine
module Lower = Hmc_wasm_program_lower
module State = Hmc_wasm_program_state
module Step = Hmc_wasm_program_step
module Result = Hmc_wasm_program_step_result
module Resource = Hmc_wasm_program_resource_step
module Calls = Wasm_calls
module C = Wasm_code
module Fuel = Wasm_control_compose
module Budget = Wasm_execution_budget
module Runs = Hmc_heap_runs
module Registers = Hmc_wasm_program_registers
module Q = Hmc_heap_state
module F = Hmc_heap_frame
type endpoint = Paused of State.running | Stopped of State.running * Result.result
type execution = {endpoint : endpoint; fuel : C.count; steps : D.index; prefix : C.count @@ ghost}
let[@def] (checkpoint @ total) (context : State.context @ immutable) (endpoint : endpoint @ immutable) = ghost_ (
  match endpoint with Paused state | Stopped (state, _) -> State.loop context state)
let[@def] (target @ total) (context : State.context @ immutable) (endpoint : endpoint @ immutable) =
  match endpoint with
  | Paused state -> Calls.Running (State.loop context state)
  | Stopped (before, result) ->
    match result with
    | Result.Continued next -> Calls.Running (State.loop context next.State.state)
    | Result.Returned final -> Result.finished before final.Result.registers
    | Result.Exhausted final -> Result.finished before final.Resource.state.State.registers
let[@def] (valid @ total) (program : I.program @ immutable) (globals : Machine.globals @ immutable)
    (lowered : Lower.program @ immutable) (context : State.context @ immutable) (endpoint : endpoint @ immutable) = ghost_ (
  match endpoint with
  | Paused state -> State.valid program globals lowered context state
  | Stopped (before, result) -> State.valid program globals lowered context before
    && Result.correct program globals lowered context before result
    && (match result with Result.Continued _ -> false | _ -> true))
let[@def] (source @ total) (endpoint : endpoint @ immutable) (steps : D.index @ immutable) =
  match endpoint with
  | Paused state -> Runs.Finished (State.configuration state)
  | Stopped (before, result) ->
    match result with
    | Result.Continued next -> Runs.Finished (State.configuration next.State.state)
    | Result.Returned _ -> Runs.Finished {Machine.heap = before.State.heap; state = Q.Done before.State.activation.F.accumulator}
    | Result.Exhausted final ->
      match final.Resource.exhausted with
      | None -> Runs.Finished (State.configuration before)
      | Some reason -> Runs.Blocked (State.configuration before, reason, steps)
let rec (done_run @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (limit : Hmc_word64.limb) -> (capacity : D.index) @ immutable -> (fuel : D.index) @ immutable ->
    (heap : Hmc_heap_objects.heap) @ immutable -> (value : Hmc_tagged_cell.value) @ immutable ->
    {u : unit | Runs.run program globals limit capacity fuel {Machine.heap; state = Q.Done value}
      === Runs.Finished {Machine.heap; state = Q.Done value}} @ ghost =
  fun program globals limit capacity fuel heap value -> ghost_ (
    Runs.run_def program globals limit capacity fuel {Machine.heap; state = Q.Done value};
    match fuel with
    | D.Z -> ()
    | D.S rest ->
      Machine.step_def program globals limit capacity {Machine.heap; state = Q.Done value};
      done_run program globals limit capacity rest heap value)
let[@def] (covers @ total) (budget : D.index @ immutable) (out : execution @ immutable) = ghost_ (
  match out.endpoint with Paused _ -> Budget.le (Budget.of_index budget) out.fuel | Stopped _ -> true)
let rec (run @ total) : (budget : D.index) @ immutable -> (program : I.program) @ immutable ->
    (globals : Machine.globals) @ immutable -> (lowered : Lower.program) @ immutable ->
    (context : State.context) @ immutable -> (before : State.running) @ immutable ->
    {u : unit | State.valid program globals lowered context before} ->
    {out : execution | valid program globals lowered context out.endpoint && covers budget out
      && Runs.run program globals before.State.registers.Registers.heap_limit context.State.stack_capacity budget (State.configuration before)
        === source out.endpoint out.steps
      && Calls.run out.fuel (State.module_ program lowered context) (State.loop context before) === target context out.endpoint
      && Calls.run out.prefix (State.module_ program lowered context) (State.loop context before)
        === Calls.Running (checkpoint context out.endpoint)} @ immutable =
  fun budget program globals lowered context before premise ->
    match budget with
    | D.Z ->
      let endpoint = Paused before in
      ghost_ (valid_def program globals lowered context endpoint; target_def context endpoint; checkpoint_def context endpoint;
        Calls.run_def C.Zero (State.module_ program lowered context) (State.loop context before));
        ghost_ (source_def endpoint D.Z;
          Runs.run_def program globals before.State.registers.Registers.heap_limit context.State.stack_capacity budget (State.configuration before));
      let out = {endpoint; fuel = C.Zero; steps = D.Z; prefix = ghost_ C.Zero} in
      ghost_ (Budget.of_index_def budget; Budget.le_def C.Zero C.Zero; covers_def budget out); out
    | D.S rest ->
      let result = Step.step program globals lowered context before () in
      ghost_ (Result.correct_def program globals lowered context before result;
        Runs.run_def program globals before.State.registers.Registers.heap_limit context.State.stack_capacity budget (State.configuration before));
      match result with
      | Result.Continued next ->
        let later = run rest program globals lowered context next.State.state () in
        ghost_ (source_def later.endpoint later.steps; source_def later.endpoint (D.S later.steps);
          Wasm_calls_body.compose next.State.fuel later.prefix (State.module_ program lowered context) (State.loop context before);
          Wasm_calls_body.compose next.State.fuel later.fuel (State.module_ program lowered context) (State.loop context before));
        let out = {endpoint = later.endpoint; fuel = Fuel.add next.State.fuel later.fuel; steps = D.S later.steps; prefix = ghost_ (Fuel.add next.State.fuel later.prefix)} in
        ghost_ (covers_def rest later; Budget.of_index_def budget;
          (match later.endpoint with Paused _ -> Budget.positive_add next.State.fuel later.fuel (Budget.of_index rest) () | _ -> ());
          covers_def budget out); out
      | Result.Returned final ->
        let endpoint = Stopped (before, result) in
        ghost_ (valid_def program globals lowered context endpoint; target_def context endpoint; checkpoint_def context endpoint;
          Calls.run_def C.Zero (State.module_ program lowered context) (State.loop context before));
        ghost_ (source_def endpoint (D.S D.Z);
          done_run program globals before.State.registers.Registers.heap_limit context.State.stack_capacity rest before.State.heap before.State.activation.F.accumulator);
        let out = {endpoint; fuel = final.Result.fuel; steps = D.S D.Z; prefix = ghost_ C.Zero} in ghost_ (covers_def budget out); out
      | Result.Exhausted final ->
        let endpoint = Stopped (before, result) in
        ghost_ (valid_def program globals lowered context endpoint; target_def context endpoint; checkpoint_def context endpoint;
          Calls.run_def C.Zero (State.module_ program lowered context) (State.loop context before));
        ghost_ (source_def endpoint D.Z);
        let out = {endpoint; fuel = final.Resource.fuel; steps = D.Z; prefix = ghost_ C.Zero} in ghost_ (covers_def budget out); out
