module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module Machine = Hmc_heap_machine
module Lower = Hmc_wasm_program_lower
module Registers = Hmc_wasm_program_registers
module State = Hmc_wasm_program_state
module Resources = Hmc_wasm_program_resources
module Frame = Hmc_wasm_program_frame
module Selection = Hmc_wasm_program_selection
module Shape = Hmc_wasm_program_case_shape
module Result = Hmc_wasm_program_step_result
module Block = Hmc_wasm_program_block
module Structured = Hmc_wasm_structured_block
module Straight = Hmc_wasm_block_lower
module Simple = Hmc_wasm_simple_lower
module Branch = Hmc_wasm_program_source_branch
let (step @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable ->
    {u : unit | State.valid program globals lowered context before} ->
    {out : Result.result | Result.correct program globals lowered context before out} @ immutable =
  fun program globals lowered context before premise ->
    ghost_ (State.valid_def program globals lowered context before;
      Resources.valid_def program globals lowered.Lower.width context.State.stack_base before.State.frame_end before.State.abstract
        before.State.heap before.State.activation before.State.frames before.State.registers before.State.memory;
      Frame.valid_def before.State.block.G.signature before.State.activation before.State.registers before.State.memory
        before.State.frame_end before.State.pc before.State.cells before.State.padding before.State.bytes before.State.suffix before.State.cell_count;
      State.configuration_def before);
    let selected = Selection.select_reachable program globals lowered context.State.max_pc context.State.block_count
      before.State.registers.Registers.heap_limit (State.configuration before) before.State.abstract before.State.activation before.State.frames
      before.State.pc context.State.table_base context.State.stack_base () in
    let instruction = selected.Selection.selected.Selection.instruction in
    let fragment = selected.Selection.selected.Selection.fragment in
    ghost_ (Shape.correct globals selected.Selection.block.G.signature instruction lowered.Lower.capacity context.State.max_pc fragment ();
      Shape.matches_def instruction fragment);
    match instruction, fragment with
      | (I.Keep (G.Branch (yes, no))), (Block.Structured (Structured.Straight (Straight.Simple (Simple.Branch (yes_pc, no_pc))))) ->
        ghost_ (Branch.fragment_def yes_pc no_pc);
        Result.continued program globals lowered context before (Hmc_wasm_program_step_branch.step program globals lowered context before yes no yes_pc no_pc ())
      | (I.Keep (G.Load (G.Closure id, ty, derivation, next))), (Block.Structured (Structured.Closure plan)) ->
        ghost_ (Hmc_wasm_program_source_closure.fragment_def plan.Hmc_wasm_closure_lower.object_ plan.Hmc_wasm_closure_lower.pc);
        let result = Hmc_wasm_program_step_closure.step program globals lowered context before id ty derivation next plan () in
        Result.resource program globals lowered context before result
      | (I.Keep (G.Cons next)), (Block.Structured (Structured.Cons plan)) ->
        ghost_ (Hmc_wasm_program_source_cons.fragment_def plan);
        let result = Hmc_wasm_program_step_cons.step program globals lowered context before next plan () in
        Result.resource program globals lowered context before result
      | (I.Keep (G.List_branch (empty, next))), (Block.Structured (Structured.List_branch plan)) ->
        ghost_ (Hmc_wasm_program_source_list.fragment_def plan);
        Result.continued program globals lowered context before (Hmc_wasm_program_step_list.step program globals lowered context before empty next plan ())
      | (I.Keep (G.Save_environment next)), (Block.Structured (Structured.Straight (Straight.Relayout plan))) ->
        ghost_ (Hmc_wasm_program_source_save_environment.fragment_def plan);
        Result.continued program globals lowered context before (Hmc_wasm_program_step_save_environment.step program globals lowered context before next plan ())
      | (I.Keep ((G.Save_value next | G.Bind next | G.Restore next) as instruction)), (Block.Structured (Structured.Straight (Straight.Relayout plan))) ->
        ghost_ (Hmc_wasm_program_source_saved_environment.fragment_def plan);
        Result.continued program globals lowered context before (Hmc_wasm_program_step_saved_environment.step program globals lowered context before instruction next plan ())
      | (I.Keep (G.Primitive (operation, next))), (Block.Structured (Structured.Straight (Straight.Primitive plan))) ->
        ghost_ (Hmc_wasm_program_source_primitive.fragment_def plan);
        Result.continued program globals lowered context before (Hmc_wasm_program_step_primitive.step program globals lowered context before operation next plan ())
      | (I.Keep (G.Jump next)), (Block.Structured (Structured.Straight (Straight.Simple (Simple.Jump pc)))) ->
        ghost_ (Hmc_wasm_program_source_jump.fragment_def pc);
        Result.continued program globals lowered context before (Hmc_wasm_program_step_jump.step program globals lowered context before next pc ())
      | (I.Keep (G.Load (G.Global index, ty, derivation, next))), (Block.Structured (Structured.Straight (Straight.Global plan))) ->
        ghost_ (Hmc_wasm_program_source_global.fragment_def plan.Hmc_wasm_global_lower.value plan.Hmc_wasm_global_lower.pc);
        Result.continued program globals lowered context before (Hmc_wasm_program_step_global.step program globals lowered context before index ty derivation next
          plan.Hmc_wasm_global_lower.value plan.Hmc_wasm_global_lower.pc ())
      | (I.Keep (G.Load (G.Local index, ty, derivation, next))), (Block.Structured (Structured.Straight (Straight.Simple (Simple.Local (number, pc))))) ->
        ghost_ (Hmc_wasm_program_source_local.fragment_def number pc);
        Result.continued program globals lowered context before (Hmc_wasm_program_step_local.step program globals lowered context before index number ty derivation next pc ())
      | (I.Keep (G.Load (atom, ty, derivation, next))), (Block.Structured (Structured.Straight (Straight.Simple (Simple.Literal (value, pc))))) ->
        ghost_ (Hmc_wasm_program_source_literal.fragment_def value pc);
        Result.continued program globals lowered context before (Hmc_wasm_program_step_literal.step program globals lowered context before atom ty derivation next value pc ())
      | (I.Keep (G.Call next)), (Block.Call call) ->
        let result = Hmc_wasm_program_step_call.step program globals lowered context before next call () in
        Result.resource program globals lowered context before result
      | I.Tail_call, (Block.Tail_call env_count) ->
        Result.continued program globals lowered context before (Hmc_wasm_program_step_tail.step program globals lowered context before env_count ())
      | (I.Keep G.Return), Block.Return ->
        (match before.State.frames with
        | Q.Halt -> Result.returned program globals lowered context before
          (Hmc_wasm_program_step_root.step program globals lowered context before ())
        | Q.Frame (saved, frames) -> Result.continued program globals lowered context before (Hmc_wasm_program_step_caller.step program globals lowered context before saved frames ()))
      | _ -> unreachable_ ()
