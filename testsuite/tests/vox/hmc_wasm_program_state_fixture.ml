module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module Machine = Hmc_heap_machine
module Inv = Hmc_heap_invariant
module Lower = Hmc_wasm_program_lower
module Index = Hmc_u32_index
module Codec = Hmc_pointer_frame_codec
module Cap = Hmc_frame_capacity
module Pad = Hmc_wasm_frame_padding
module Bounds = Hmc_linear_bounds
module Bytes = Hmc_linear_bytes
module Registers = Hmc_wasm_program_registers
module Resources = Hmc_wasm_program_resources
module Frame = Hmc_wasm_program_frame
module Store = Hmc_wasm_program_frame_store
module Descriptors = Hmc_wasm_program_descriptors
module Runtime = Hmc_runtime_closures
module State = Hmc_wasm_program_state
module Branch = Hmc_wasm_program_source_branch
module Step = Hmc_wasm_program_step_branch
module Table = Hmc_wasm_program_table
module Block = Hmc_wasm_program_block
module Structured = Hmc_wasm_structured_block
module Straight = Hmc_wasm_block_lower
module Simple = Hmc_wasm_simple_lower
let[@def] (limit @ total) (u : unit) : B.u32 = 8192
let rec zeros n = if n = 0 then B.End else B.Byte (0, zeros (n - 1))
let rec (above @ total) : (heap : H.heap) @ immutable -> (boundary : B.u32) ->
    {out : bool | out === Hmc_wasm_heap_suffix.above heap boundary} =
  fun heap boundary -> ghost_ (Hmc_wasm_heap_suffix.above_def heap boundary);
    match heap with H.Empty_heap base -> boundary <= base
    | H.Allocate (a, rest) -> boundary <= a.H.address && above rest boundary
type prepared = {lowered : Lower.program; context : State.context; state : State.running}
let prepare : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (configuration : Machine.configuration) @ immutable -> (abstract : Hmc_cfg_semantics.state) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (elapsed : D.index) @ immutable -> (base : B.u32) ->
    {u : unit | Inv.valid program globals (limit ()) configuration abstract
      && abstract === U.advance program elapsed (U.initial program input)} ->
    {out : prepared | State.valid program globals out.lowered out.context out.state
      && out.state.State.elapsed === elapsed && out.context.State.input === input
      && State.configuration out.state === configuration
      && out.state.State.abstract === abstract && out.state.State.activation.Hmc_heap_frame.pc ===
        (match configuration.Machine.state with Q.Running (a, _) -> a.F.pc | _ -> D.Z)} @ immutable =
  fun program globals configuration abstract input elapsed base premise ->
    ghost_ (limit_def (); Inv.valid_def program globals 8192 configuration abstract);
    if base < 256 || base > 263 then failwith "shared frame base" else
    match configuration.Machine.state, Lower.lower program globals 1000 with
    | Q.Running (activation, frames), Some lowered ->
      let heap = configuration.Machine.heap in
      let blocks = program.I.origin.Hmc_cfg_program.blocks in
      let table = program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.table in
      (match G.lookup blocks activation.F.pc, Index.encode 1000 activation.F.pc, Index.encode 1000 (G.size blocks),
        Runtime.lower 1000 table program.I.origin.Hmc_cfg_program.functions with
      | Some block, Some pc, Some block_count, Some runtime ->
        ghost_ (Hmc_wasm_program_selection.frame_shape program globals 8192 configuration abstract activation frames block ();
          Lower.corresponds_def program globals 1000 lowered;
          Cap.lookup blocks activation.F.pc block (); Hmc_pointer_frame_shape.size block.G.signature);
        let padding = Pad.cells (Cap.remaining (Cap.capacity blocks) (Codec.size block.G.signature) ()) in
        let cells = Codec.encode block.G.signature activation padding () in
        (match Index.encode 1024 (H.length cells), Index.encode 7 (Runtime.size runtime) with
        | Some count, Some table_count ->
          if count <> lowered.Lower.capacity then failwith "shared frame capacity" else
          (match Hmc_memory_stack_capacity.reserve lowered.Lower.width (D.S (D.S (D.S D.Z))) 1024 2048 () with
          | None -> failwith "shared stack reservation"
          | Some stack_limit ->
          let _ = ghost_ (Hmc_memory_stack_capacity.ordered lowered.Lower.width (D.S (D.S (D.S D.Z))) 1024 stack_limit ()) in
          let wire_count : B.u32 = count + 1 in
          let frame_end : B.u32 = base + 16 * wire_count in
          if frame_end > 1024 || H.used heap < 2048 || not (above heap frame_end && above heap stack_limit)
            || not (Hmc_heap_image.encodable 1000 heap) then failwith "shared layout" else
          let initial = zeros 8192 in
          (match Bytes.drop initial 8192 with
          | None -> failwith "shared memory size"
          | Some _ ->
            ghost_ (Bounds.covers_def initial 8192);
            let heap_memory = Hmc_heap_image.materialize table 1000 initial heap 8192 () in
            ghost_ (Hmc_memory_saved_frame.slots_def blocks; Hmc_memory_stack.zero_def ();
              Index.represents_def (D.S (Cap.capacity blocks)) (lowered.Lower.capacity + 1);
              let _ = Bounds.suffix heap_memory 8192 stack_limit () in Bounds.covers_def heap_memory stack_limit);
            (match Hmc_wasm_reservation.reserve (Hmc_memory_saved_frame.slots blocks) (lowered.Lower.capacity + 1) 0 lowered.Lower.width () with
            | None -> failwith "shared stack width"
            | Some _ ->
            (match Hmc_memory_stack_image.materialize blocks 1000 lowered.Lower.width heap_memory 1024 stack_limit frames () with
            | None -> failwith "shared stack image"
            | Some stack ->
            let memory = stack.Hmc_memory_stack.memory in
            ghost_ (let _ = Bounds.suffix heap_memory 8192 stack.Hmc_memory_stack.top () in
              Bounds.covers_def heap_memory stack.Hmc_memory_stack.top;
              Hmc_wasm_program_step_caller.objects_below heap stack_limit stack.Hmc_memory_stack.top ();
              Hmc_heap_image_suffix.preserve heap_memory memory heap stack.Hmc_memory_stack.top ();
              Bounds.same_length heap_memory memory 8192 ());
            let registers = {Registers.frame = base; heap = H.used heap; heap_limit = 8192; top = stack.Hmc_memory_stack.top; stack_limit;
              status = 99; tag = Hmc_wasm_header_update.number 123; payload = Hmc_wasm_header_update.number 456} in
            ghost_ (let _ = Bounds.suffix memory 8192 frame_end () in
              let _ = Bounds.suffix memory 8192 stack_limit () in
              Bounds.covers_def memory frame_end; Bounds.covers_def memory stack_limit;
              Resources.valid_def program globals lowered.Lower.width 1024 frame_end abstract heap activation frames registers memory;
              Index.represents_def (D.S (H.length cells)) wire_count);
            let stored = Store.store program globals lowered.Lower.width 1024 frame_end abstract heap activation frames registers memory
              block.G.signature padding pc wire_count () in
            let installed = Descriptors.install program globals lowered.Lower.width 1024 frame_end abstract heap activation frames registers stored
              block.G.signature padding pc wire_count runtime 0 table_count () in
            let context = {State.input; max_pc = 1000; block_count; table_base = 0; table_count; runtime;
              stack_base = 1024; stack_capacity = D.S (D.S (D.S D.Z)); host_capacity = Wasm_code.Zero} in
            let state = {State.abstract; elapsed; heap; activation; frames; block; registers; memory = installed.Store.memory; pc;
              cells = installed.Store.cells; padding; bytes = installed.Store.bytes; suffix = installed.Store.suffix; frame_end; cell_count = wire_count} in
            ghost_ (Frame.valid_def block.G.signature activation registers installed.Store.memory frame_end pc installed.Store.cells padding installed.Store.bytes installed.Store.suffix wire_count;
              Index.represents_def (D.S (H.length installed.Store.cells)) wire_count;
              State.valid_def program globals lowered context state; State.configuration_def state);
            {lowered; context; state}))))
        | _ -> failwith "shared encoding")
      | _ -> failwith "shared lowering")
    | _ -> failwith "shared running state"
let check_composed : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable ->
    {u : unit | State.valid program globals lowered context before} -> unit =
  fun program globals lowered context before premise ->
    let budget = D.S (D.S D.Z) in
    let composed = Hmc_wasm_program_execution.run budget program globals lowered context before () in
    if Wasm_calls.run composed.Hmc_wasm_program_execution.fuel (State.module_ program lowered context) (State.loop context before)
      <> Hmc_wasm_program_execution.target context composed.Hmc_wasm_program_execution.endpoint
      then failwith "shared composed execution";
    if Hmc_heap_runs.run program globals before.State.registers.Registers.heap_limit context.State.stack_capacity budget (State.configuration before)
      <> Hmc_wasm_program_execution.source composed.Hmc_wasm_program_execution.endpoint composed.Hmc_wasm_program_execution.steps
      then failwith "shared composed source"
let rec index n = if n = 0 then D.Z else D.S (index (n - 1))
let check_observation : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable ->
    (prefix : Wasm_code.count) @ immutable -> {u : unit | State.valid program globals lowered context before} -> unit =
  fun program globals lowered context before prefix premise ->
    let witness = Hmc_wasm_program_observe.entry program globals lowered context before prefix () in
    let module_ = State.module_ program lowered context in
    let initial = Hmc_wasm_program_state_entry.configuration context before in
    match Wasm_calls.run prefix module_ initial with
    | Wasm_calls.Running _ -> ()
    | Wasm_calls.Finished after ->
      (match Registers.read after.Wasm_global_execution.globals with
      | Some registers ->
        (match after.Wasm_global_execution.execution.Wasm_memory_execution.machine.Wasm_execution.stack with
        | Wasm_scalar.Push (Wasm_scalar.I32 status, Wasm_scalar.Empty) ->
          if status <> registers.Registers.status then failwith "observed status register" else
          if status = 1 then (
            if registers.Registers.tag.Hmc_word64.lo <> 1 || registers.Registers.tag.Hmc_word64.hi <> 0
              then failwith "observed result tag" else
            let word = registers.Registers.payload in
            let _ = ghost_ (Hmc_tagged_cell.tag_def (Hmc_tagged_cell.Word word)) in
            let steps = Hmc_wasm_program_observe.reflection program globals lowered context before prefix after registers word () in
            if Hmc_source_semantics.advance steps
                (Hmc_monomorphic_simulation.source_start program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.origin context.State.input)
              <> Hmc_source_semantics.Done (Hm_interpreter_typing.Word word) then failwith "observed source reflection")
          else if status <> 2 && status <> 3 then failwith "observed invalid status"
        | _ -> failwith "observed result stack")
      | None -> failwith "observed registers");
      if Hmc_wasm_program_execution.target context witness.Hmc_wasm_program_execution.endpoint <> Wasm_calls.Finished after
        then failwith "observed terminal agreement"
    | _ -> failwith "observed unsafe target prefix"
let check_original : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable ->
    {u : unit | State.valid program globals lowered context before && before.State.elapsed === D.Z} -> unit =
  fun program globals lowered context before premise ->
    let source = program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.origin in
    ghost_ (Hmc_monomorphic.ready_def source);
    let definitions : {d : Hmc_monomorphic.definitions | Hmc_monomorphic.origins d} = refine_ source.Hmc_monomorphic.definitions in
    let fuel = index 2000 in
    let initial = Hmc_monomorphic_simulation.source_start source context.State.input in
    match Hmc_source_semantics.advance fuel initial with
    | Hmc_source_semantics.Done (Hm_interpreter_typing.Word word) ->
      let budget = Hmc_tail_simulation.source_preservation program definitions context.State.input word fuel () in
      let out = Hmc_wasm_program_source_execution.preservation program definitions globals lowered context before word fuel () in
      check_observation program globals lowered context before (Wasm_code.Succ out.Hmc_wasm_program_execution.fuel) ();
      check_observation program globals lowered context before (Wasm_code.Succ (Wasm_code.Succ (Wasm_code.Succ out.Hmc_wasm_program_execution.fuel))) ();
      if Wasm_calls.run out.Hmc_wasm_program_execution.fuel (State.module_ program lowered context) (State.loop context before)
        <> Hmc_wasm_program_execution.target context out.Hmc_wasm_program_execution.endpoint
        then failwith "original-source preservation execution" else
      (match out.Hmc_wasm_program_execution.endpoint with
      | Hmc_wasm_program_execution.Stopped (last, Hmc_wasm_program_step_result.Returned _) ->
        ghost_ (Hmc_wasm_program_source_execution.exhausted_def out.Hmc_wasm_program_execution.endpoint);
        let witness = Hmc_wasm_program_source_execution.reflection program definitions globals lowered context out.Hmc_wasm_program_execution.endpoint word () in
        if last.State.activation.F.accumulator <> Hmc_tagged_cell.Word word then failwith "original-source result";
        if Hmc_source_semantics.advance witness initial <> Hmc_source_semantics.Done (Hm_interpreter_typing.Word word)
          then failwith "original-source reflection witness"
      | Hmc_wasm_program_execution.Stopped (_, Hmc_wasm_program_step_result.Exhausted _) -> ()
      | _ -> failwith "original-source preservation endpoint");
      if Hmc_heap_extent.fits (Hmc_heap_demand.heap_plan program budget before.State.abstract)
          (Hmc_heap_objects.used before.State.heap) before.State.registers.Registers.heap_limit
        && Hmc_frame_capacity.le (Hmc_heap_demand.stack_plan program budget before.State.abstract) context.State.stack_capacity then (
        ghost_ (State.valid_def program globals lowered context before;
          U.advance_def program D.Z (U.initial program context.State.input));
        let normal = Hmc_wasm_program_source_execution.normal program globals lowered context before word budget () in
        match normal.Hmc_wasm_program_execution.endpoint with
        | Hmc_wasm_program_execution.Stopped (last, Hmc_wasm_program_step_result.Returned _) ->
          if last.State.activation.F.accumulator <> Hmc_tagged_cell.Word word then failwith "sufficient-resource result";
          if Wasm_calls.run normal.Hmc_wasm_program_execution.fuel (State.module_ program lowered context) (State.loop context before)
            <> Hmc_wasm_program_execution.target context normal.Hmc_wasm_program_execution.endpoint
            then failwith "sufficient-resource execution"
        | _ -> failwith "sufficient-resource exhaustion")
    | _ -> failwith "original-source fixture did not terminate"
let check_pipeline program result =
  match result with
  | Hmc_wasm_program_run.Execution execution ->
    let prepared = execution.Hmc_wasm_program_run.prepared in
    let context = prepared.Hmc_wasm_program_initialize.context in
    let state = prepared.Hmc_wasm_program_initialize.state in
    let module_ = State.module_ program prepared.Hmc_wasm_program_initialize.lowered context in
    if Wasm_functions.signature module_.Wasm_functions.signatures 0 <> Some Wasm_functions.Void
      || Wasm_functions.signature module_.Wasm_functions.signatures 1 <> Some Wasm_functions.I32
      then failwith "assembled function signatures";
    let expected_entry = Hmc_wasm_program_state_entry.configuration context state in
    if Wasm_calls.start module_ context.State.block_count state.State.memory (Registers.globals state.State.registers)
        (Wasm_code.Succ context.State.host_capacity) <> Wasm_calls.Running expected_entry
      then failwith "initialized dispatcher entry";
    let run = execution.Hmc_wasm_program_run.run in
    if Wasm_calls.run (Wasm_code.Succ run.Hmc_wasm_program_execution.fuel) module_ expected_entry
      <> Hmc_wasm_program_execution.target context run.Hmc_wasm_program_execution.endpoint
      then failwith "initialized dispatcher execution"
  | _ -> failwith "initialized pipeline unexpectedly rejected"
let check_initialization program input =
  let memory = zeros 8192 in
  match Bytes.drop memory 8192 with
  | None -> failwith "initializer fixture memory"
  | Some _ ->
    ghost_ (Bounds.covers_def memory 8192);
    List.iter (fun base -> List.iter (fun capacity ->
      let layout = {Hmc_wasm_program_initialize.table_base = 0; frame_base = base; stack_base = 1024;
        heap_base = 4096; heap_limit = 8192; max_pc = 1000; stack_capacity = capacity; host_capacity = Wasm_code.Zero} in
      if base < 256 || base > 263 then failwith "initializer fixture base" else
      let _ = ghost_ (Hmc_wasm_program_initialize.valid_layout_def layout memory) in
      check_pipeline program (Hmc_wasm_program_run.run program layout input memory (D.S (D.S D.Z)) ());
      let source_fuel = index 2000 in
      (match Hmc_source_semantics.advance source_fuel
          (Hmc_monomorphic_simulation.source_start program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.origin input) with
      | Hmc_source_semantics.Done (Hm_interpreter_typing.Word word) ->
        let out = Hmc_wasm_program_run.preservation program layout input memory word source_fuel () in
        check_pipeline program out;
        (match out with
        | Hmc_wasm_program_run.Execution execution ->
          (match execution.Hmc_wasm_program_run.run.Hmc_wasm_program_execution.endpoint with
          | Hmc_wasm_program_execution.Stopped (last, Hmc_wasm_program_step_result.Returned _) ->
            if last.State.activation.F.accumulator <> Hmc_tagged_cell.Word word then failwith "initialized source result"
          | Hmc_wasm_program_execution.Stopped (_, Hmc_wasm_program_step_result.Exhausted _) -> ()
          | _ -> failwith "initialized source endpoint")
        | _ -> failwith "initialized source rejection")
      | _ -> failwith "initialized source fixture fuel");
      let result = Hmc_wasm_program_initialize.initialize program layout input memory () in
      ghost_ (Hmc_wasm_program_initialize.correct_def program layout input memory result);
      match result with
      | Hmc_wasm_program_initialize.Initialized (start, prepared) ->
        ghost_ (Hmc_wasm_program_initialize.installed_def program layout input memory start prepared);
        check_composed program start.Hmc_heap_initialize.globals prepared.Hmc_wasm_program_initialize.lowered
          prepared.Hmc_wasm_program_initialize.context prepared.Hmc_wasm_program_initialize.state ();
        List.iter (fun prefix ->
          check_observation program start.Hmc_heap_initialize.globals prepared.Hmc_wasm_program_initialize.lowered
            prepared.Hmc_wasm_program_initialize.context prepared.Hmc_wasm_program_initialize.state prefix ())
          [Wasm_code.Zero; Wasm_code.Succ Wasm_code.Zero;
            Wasm_execution_budget.of_index (index 3); Wasm_execution_budget.of_index (index 31)];
        check_original program start.Hmc_heap_initialize.globals prepared.Hmc_wasm_program_initialize.lowered
          prepared.Hmc_wasm_program_initialize.context prepared.Hmc_wasm_program_initialize.state ();
        if prepared.Hmc_wasm_program_initialize.state.State.registers.Registers.frame <> base
          then failwith "initializer frame address"
      | _ -> failwith "initializer rejected fixture") [D.Z; D.S (D.S (D.S D.Z))]) [256; 263];
    let layout = {Hmc_wasm_program_initialize.table_base = 0; frame_base = 256; stack_base = 1024;
      heap_base = 4096; heap_limit = 4096; max_pc = 1000; stack_capacity = D.Z; host_capacity = Wasm_code.Zero} in
    let _ = ghost_ (let _ = Bounds.suffix memory 8192 4096 () in Bounds.covers_def memory 4096;
      Hmc_wasm_program_initialize.valid_layout_def layout memory) in
    (match Hmc_wasm_program_initialize.initialize program layout input memory () with
    | Hmc_wasm_program_initialize.Heap_exhausted _ -> ()
    | _ -> failwith "initializer missing heap exhaustion");
    let rejected = {layout with Hmc_wasm_program_initialize.heap_limit = 8192; frame_base = 1024} in
    let _ = ghost_ (Hmc_wasm_program_initialize.valid_layout_def rejected memory) in
    match Hmc_wasm_program_initialize.initialize program rejected input memory () with
    | Hmc_wasm_program_initialize.Layout_rejected -> ()
    | _ -> failwith "initializer accepted overlapping frame"
let rec audit : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable ->
    int -> {u : unit | State.valid program globals lowered context before} -> int =
  fun program globals lowered context before remaining premise ->
    if remaining = 0 then failwith "shared target fuel" else
    check_composed program globals lowered context before ();
    let result = Hmc_wasm_program_step.step program globals lowered context before () in
    ghost_ (Hmc_wasm_program_step_result.correct_def program globals lowered context before result);
    match result with
    | Hmc_wasm_program_step_result.Returned final ->
      if Wasm_calls.run final.Hmc_wasm_program_step_result.fuel (State.module_ program lowered context) (State.loop context before)
        <> Hmc_wasm_program_step_result.finished before final.Hmc_wasm_program_step_result.registers
        then failwith "shared root execution";
      if Machine.step program globals before.State.registers.Registers.heap_limit context.State.stack_capacity (State.configuration before)
        <> Machine.Advanced {Machine.heap = before.State.heap; state = Q.Done before.State.activation.F.accumulator}
        then failwith "shared root successor";
      1
    | Hmc_wasm_program_step_result.Exhausted _ -> failwith "shared execution unexpectedly exhausted"
    | Hmc_wasm_program_step_result.Continued transition ->
      let target = State.module_ program lowered context in
      if Wasm_calls.run transition.State.fuel target (State.loop context before)
        <> Wasm_calls.Running (State.loop context transition.State.state) then failwith "shared step execution";
      if Machine.step program globals before.State.registers.Registers.heap_limit context.State.stack_capacity (State.configuration before)
        <> Machine.Advanced (State.configuration transition.State.state) then failwith "shared source successor";
      1 + audit program globals lowered context transition.State.state (remaining - 1) ()
let limit_heap : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable -> (new_limit : B.u32) ->
    {u : unit | State.valid program globals lowered context before} ->
    {out : State.running | State.valid program globals lowered context out
      && out === {before with State.registers = {before.State.registers with Registers.heap_limit = new_limit}}} @ immutable =
  fun program globals lowered context before new_limit premise ->
    if new_limit < before.State.registers.Registers.heap || new_limit > before.State.registers.Registers.heap_limit
      then failwith "shared allocation limit" else
    let registers = {before.State.registers with Registers.heap_limit = new_limit} in
    let limited = {before with State.registers} in
    ghost_ (State.valid_def program globals lowered context before;
      State.configuration_def before; State.configuration_def limited;
      Resources.valid_def program globals lowered.Lower.width context.State.stack_base before.State.frame_end before.State.abstract
        before.State.heap before.State.activation before.State.frames before.State.registers before.State.memory;
      Inv.valid_def program globals before.State.registers.Registers.heap_limit (State.configuration before) before.State.abstract);
    (match Bytes.drop before.State.memory registers.Registers.heap_limit with
    | None -> failwith "shared exhausted heap extent"
    | Some _ ->
      ghost_ (Bounds.covers_def before.State.memory registers.Registers.heap_limit;
        Inv.valid_def program globals registers.Registers.heap_limit (State.configuration limited) before.State.abstract;
        Resources.valid_def program globals lowered.Lower.width context.State.stack_base before.State.frame_end before.State.abstract
          before.State.heap before.State.activation before.State.frames registers before.State.memory;
        Frame.valid_def before.State.block.G.signature before.State.activation before.State.registers before.State.memory before.State.frame_end
          before.State.pc before.State.cells before.State.padding before.State.bytes before.State.suffix before.State.cell_count;
        Frame.valid_def before.State.block.G.signature before.State.activation registers before.State.memory before.State.frame_end
          before.State.pc before.State.cells before.State.padding before.State.bytes before.State.suffix before.State.cell_count;
        Descriptors.valid_def program before.State.registers before.State.memory context.State.runtime context.State.table_base context.State.table_count;
        Descriptors.valid_def program registers before.State.memory context.State.runtime context.State.table_base context.State.table_count;
        State.valid_def program globals lowered context limited);
      limited)
let check_exhaustion program globals lowered context limited result =
  match result.Hmc_wasm_program_resource_step.exhausted with
  | None | Some Machine.Stack -> failwith "shared allocation missing heap exhaustion"
  | Some Machine.Heap ->
    let after = result.Hmc_wasm_program_resource_step.state in
    let expected = Wasm_calls.Finished {Wasm_global_execution.globals = Registers.globals after.State.registers;
      execution = {Wasm_memory_execution.memory = limited.State.memory; machine = {Wasm_execution.locals = Wasm_scalar.Empty;
        stack = Wasm_scalar.Push (Wasm_scalar.I32 after.State.registers.Registers.status, Wasm_scalar.Empty)}}} in
    if Wasm_calls.run result.Hmc_wasm_program_resource_step.fuel (State.module_ program lowered context) (State.loop context limited)
      <> expected then failwith "shared allocation exhaustion execution";
    if Machine.step program globals limited.State.registers.Registers.heap_limit context.State.stack_capacity (State.configuration limited)
      <> Machine.Exhausted Machine.Heap then failwith "shared allocation exhaustion source"
let check_allocation program globals lowered context before result =
  match result.Hmc_wasm_program_resource_step.exhausted with
  | Some _ -> failwith "shared allocation exact fit exhausted"
  | None ->
    let after = result.Hmc_wasm_program_resource_step.state in
    if Wasm_calls.run result.Hmc_wasm_program_resource_step.fuel (State.module_ program lowered context) (State.loop context before)
      <> Wasm_calls.Running (State.loop context after) then failwith "shared allocation exact fit execution";
    if Machine.step program globals before.State.registers.Registers.heap_limit context.State.stack_capacity (State.configuration before)
      <> Machine.Advanced (State.configuration after) then failwith "shared allocation exact fit source"
let rec collect : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (configuration : Machine.configuration) @ immutable -> (abstract : Hmc_cfg_semantics.state) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (elapsed : D.index) @ immutable -> int ->
    {u : unit | Inv.valid program globals (limit ()) configuration abstract
      && abstract === U.advance program elapsed (U.initial program input)} -> int =
  fun program globals configuration abstract input elapsed remaining premise ->
    ghost_ (limit_def ());
    if remaining = 0 then failwith "shared source fuel" else
    match configuration.Machine.state with
    | Q.Done _ -> 0
    | Q.Stuck -> failwith "shared source stuck"
    | Q.Running (activation, _) ->
      let count = match I.lookup program.I.code activation.F.pc with
      | Some (I.Keep (G.Branch (yes, no))) ->
        List.iter (fun base ->
          let prepared = prepare program globals configuration abstract input elapsed base () in
          if audit program globals prepared.lowered prepared.context prepared.state 100 () < 2 then failwith "shared branch segment") [256; 263];
        2
      | Some (I.Keep G.Return) ->
        List.iter (fun base ->
          let prepared = prepare program globals configuration abstract input elapsed base () in
          if audit program globals prepared.lowered prepared.context prepared.state 100 () <> 1 then failwith "shared root segment") [256; 263];
        2
      | Some (I.Keep (G.Load (G.Closure id, ty, derivation, next))) ->
        List.iter (fun base ->
          let prepared = prepare program globals configuration abstract input elapsed base () in
          if audit program globals prepared.lowered prepared.context prepared.state 100 () < 3 then failwith "shared closure segment";
          match Table.lookup prepared.lowered.Lower.blocks prepared.state.State.pc with
          | Some (Block.Structured (Structured.Closure plan)) ->
            ghost_ (State.configuration_def prepared.state;
              Hmc_wasm_program_source_closure.fragment_def plan.Hmc_wasm_closure_lower.object_ plan.Hmc_wasm_closure_lower.pc);
            let needed = plan.Hmc_wasm_closure_lower.object_.Hmc_wasm_closure_write.bytes in
            List.iter (fun available ->
              let used = prepared.state.State.registers.Registers.heap in
              if available < 0 || available > prepared.state.State.registers.Registers.heap_limit - used
                then failwith "shared allocation fixture budget" else
              let limited = limit_heap program globals prepared.lowered prepared.context prepared.state (used + available) () in
              check_composed program globals prepared.lowered prepared.context limited ();
              let result = Hmc_wasm_program_step_closure.step program globals prepared.lowered prepared.context limited id ty derivation next plan () in
              if available < needed then check_exhaustion program globals prepared.lowered prepared.context limited result
              else check_allocation program globals prepared.lowered prepared.context limited result) [0; needed - 1; needed]
          | _ -> failwith "shared closure fragment") [256; 263];
        2
      | Some (I.Keep (G.Cons next)) ->
        List.iter (fun base ->
          let prepared = prepare program globals configuration abstract input elapsed base () in
          if audit program globals prepared.lowered prepared.context prepared.state 100 () < 3 then failwith "shared cons segment";
          match Table.lookup prepared.lowered.Lower.blocks prepared.state.State.pc with
          | Some (Block.Structured (Structured.Cons plan)) ->
            ghost_ (State.configuration_def prepared.state; Hmc_wasm_program_source_cons.fragment_def plan);
            let needed = 32 in
            List.iter (fun available ->
              let used = prepared.state.State.registers.Registers.heap in
              if available < 0 || available > prepared.state.State.registers.Registers.heap_limit - used
                then failwith "shared allocation fixture budget" else
              let limited = limit_heap program globals prepared.lowered prepared.context prepared.state (used + available) () in
              check_composed program globals prepared.lowered prepared.context limited ();
              let result = Hmc_wasm_program_step_cons.step program globals prepared.lowered prepared.context limited next plan () in
              if available < needed then check_exhaustion program globals prepared.lowered prepared.context limited result
              else check_allocation program globals prepared.lowered prepared.context limited result) [0; needed - 1; needed]
          | _ -> failwith "shared cons fragment") [256; 263];
        2
      | Some (I.Keep (G.List_branch _)) ->
        List.iter (fun base ->
          let prepared = prepare program globals configuration abstract input elapsed base () in
          if audit program globals prepared.lowered prepared.context prepared.state 100 () < 2 then failwith "shared list segment") [256; 263];
        2
      | Some (I.Keep (G.Save_environment _)) ->
        List.iter (fun base ->
          let prepared = prepare program globals configuration abstract input elapsed base () in
          if audit program globals prepared.lowered prepared.context prepared.state 100 () < 5 then failwith "shared saved-environment segment") [256; 263];
        0
      | Some (I.Keep (G.Primitive _)) ->
        List.iter (fun base ->
          let prepared = prepare program globals configuration abstract input elapsed base () in
          if audit program globals prepared.lowered prepared.context prepared.state 100 () < 1 then failwith "shared primitive segment") [256; 263];
        2
      | Some (I.Keep (G.Load (G.Global _, _, _, _))) ->
        List.iter (fun base ->
          let prepared = prepare program globals configuration abstract input elapsed base () in
          if audit program globals prepared.lowered prepared.context prepared.state 100 () < 2 then failwith "shared global segment") [256; 263];
        2
      | Some (I.Keep (G.Load (G.Local _, _, _, _))) ->
        List.iter (fun base ->
          let prepared = prepare program globals configuration abstract input elapsed base () in
          if audit program globals prepared.lowered prepared.context prepared.state 100 () < 1 then failwith "shared local segment") [256; 263];
        2
      | Some (I.Keep (G.Jump _)) ->
        List.iter (fun base ->
          let prepared = prepare program globals configuration abstract input elapsed base () in
          if audit program globals prepared.lowered prepared.context prepared.state 100 () < 1 then failwith "shared jump segment") [256; 263];
        0
      | Some (I.Keep (G.Load ((G.Truth | G.False), _, _, _))) ->
        List.iter (fun base ->
          let prepared = prepare program globals configuration abstract input elapsed base () in
          if audit program globals prepared.lowered prepared.context prepared.state 100 () < 3 then failwith "shared literal segment") [256; 263];
        0
      | _ -> 0 in
      ghost_ (Inv.step program globals 8192 (D.S (D.S (D.S D.Z))) configuration abstract ();
        Hmc_heap_reachable_operands.advance_next program input elapsed);
      (match Machine.step program globals 8192 (D.S (D.S (D.S D.Z))) configuration with
      | Machine.Exhausted _ -> failwith "shared source exhaustion"
      | Machine.Advanced next -> count + collect program globals next (U.step program abstract) input (D.S elapsed) (remaining - 1) ())
let cases source =
  let program = Hmc_wasm_global_fixture.build source in
  let input = Hmc_wasm_header_update.number 42 in
  check_initialization program input;
  match Hmc_heap_initialize.initialize program 4096 8192 input () with
  | Hmc_heap_initialize.Heap_exhausted _ -> failwith "shared source initialization"
  | Hmc_heap_initialize.Initialized start ->
    ghost_ (limit_def (); Hmc_heap_initialize.correct_def program 4096 8192 input (Hmc_heap_initialize.Initialized start);
      U.advance_def program D.Z (U.initial program input));
    let prepared = prepare program start.Hmc_heap_initialize.globals start.Hmc_heap_initialize.configuration (U.initial program input) input D.Z 256 () in
    check_original program start.Hmc_heap_initialize.globals prepared.lowered prepared.context prepared.state ();
    let limited = limit_heap program start.Hmc_heap_initialize.globals prepared.lowered prepared.context prepared.state prepared.state.State.registers.Registers.heap () in
    check_original program start.Hmc_heap_initialize.globals prepared.lowered prepared.context limited ();
    collect program start.Hmc_heap_initialize.globals start.Hmc_heap_initialize.configuration (U.initial program input) input D.Z 500 ()
let stack_budget : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable -> (extra : bool) ->
    {u : unit | State.valid program globals lowered context before} ->
    {out : prepared | out.lowered === lowered
      && out.context === {context with State.stack_capacity = (if extra then D.S (Q.depth before.State.frames) else Q.depth before.State.frames)}
      && out.state === {before with State.registers = {before.State.registers with Registers.stack_limit = (if extra then Wasm_scalar.add32 before.State.registers.Registers.top lowered.Lower.width else before.State.registers.Registers.top)}}
      && State.valid program globals out.lowered out.context out.state} @ immutable =
  fun program globals lowered context before extra premise ->
    ghost_ (State.valid_def program globals lowered context before;
      Resources.valid_def program globals lowered.Lower.width context.State.stack_base before.State.frame_end before.State.abstract
        before.State.heap before.State.activation before.State.frames before.State.registers before.State.memory);
    if extra && lowered.Lower.width > before.State.registers.Registers.stack_limit - before.State.registers.Registers.top
      then failwith "shared fixture needs one free stack frame" else
    let registers = {before.State.registers with Registers.stack_limit = (if extra then Wasm_scalar.add32 before.State.registers.Registers.top lowered.Lower.width else before.State.registers.Registers.top)} in
    let limited = {before with State.registers} in
    let context = {context with State.stack_capacity = (if extra then D.S (Q.depth before.State.frames) else Q.depth before.State.frames)} in
    if not (above before.State.heap registers.Registers.stack_limit) then failwith "shared tail stack boundary" else
    let _ = ghost_ (Wasm_scalar.add32_def before.State.registers.Registers.top lowered.Lower.width;
      let _ = Bounds.suffix before.State.memory before.State.registers.Registers.stack_limit registers.Registers.stack_limit () in
      Bounds.covers_def before.State.memory registers.Registers.stack_limit;
      Hmc_memory_stack_capacity.depth program.I.origin.Hmc_cfg_program.blocks lowered.Lower.width before.State.memory
        context.State.stack_base registers.Registers.top before.State.frames ();
      Hmc_memory_stack_capacity.ordered lowered.Lower.width (Q.depth before.State.frames) context.State.stack_base registers.Registers.top ();
      Wasm_scalar.add32_def before.State.registers.Registers.top lowered.Lower.width;
      Hmc_memory_stack_capacity.region_def lowered.Lower.width context.State.stack_capacity context.State.stack_base registers.Registers.stack_limit;
      Hmc_memory_stack.previous_def lowered.Lower.width registers.Registers.stack_limit;
      Resources.valid_def program globals lowered.Lower.width context.State.stack_base before.State.frame_end before.State.abstract
        before.State.heap before.State.activation before.State.frames registers before.State.memory;
      Frame.valid_def before.State.block.G.signature before.State.activation before.State.registers before.State.memory before.State.frame_end
        before.State.pc before.State.cells before.State.padding before.State.bytes before.State.suffix before.State.cell_count;
      Frame.valid_def before.State.block.G.signature before.State.activation registers before.State.memory before.State.frame_end
        before.State.pc before.State.cells before.State.padding before.State.bytes before.State.suffix before.State.cell_count;
      Descriptors.valid_def program before.State.registers before.State.memory context.State.runtime context.State.table_base context.State.table_count;
      Descriptors.valid_def program registers before.State.memory context.State.runtime context.State.table_base context.State.table_count;
      State.valid_def program globals lowered context limited) in
    {lowered; context; state = limited}
let tail_without_stack : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable ->
    (env_count : Hmc_wasm_relayout.count) ->
    {u : unit | State.valid program globals lowered context before
      && I.lookup program.I.code before.State.activation.F.pc === Some I.Tail_call
      && Table.lookup lowered.Lower.blocks before.State.pc === Some (Block.Tail_call env_count)} -> unit =
  fun program globals lowered context before env_count premise ->
    let prepared = stack_budget program globals lowered context before false () in
    let context = prepared.context in
    let limited = prepared.state in
    check_composed program globals lowered context limited ();
    let result = Hmc_wasm_program_step_tail.step program globals lowered context limited env_count () in
    if Wasm_calls.run result.State.fuel (State.module_ program lowered context) (State.loop context limited)
      <> Wasm_calls.Running (State.loop context result.State.state) then failwith "shared tail zero-stack execution";
    if Machine.step program globals limited.State.registers.Registers.heap_limit context.State.stack_capacity (State.configuration limited)
      <> Machine.Advanced (State.configuration result.State.state) then failwith "shared tail zero-stack source";
    if result.State.state.State.registers.Registers.top <> limited.State.registers.Registers.top
      || result.State.state.State.frames <> limited.State.frames then failwith "shared tail changed stack"
let call_without_stack : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable ->
    (next : D.index) @ immutable -> (call : Block.call) @ immutable ->
    {u : unit | State.valid program globals lowered context before
      && I.lookup program.I.code before.State.activation.F.pc === Some (I.Keep (G.Call next))
      && Table.lookup lowered.Lower.blocks before.State.pc === Some (Block.Call call)} -> unit =
  fun program globals lowered context before next call premise ->
    let prepared = stack_budget program globals lowered context before false () in
    let context = prepared.context in
    let limited = prepared.state in
    check_composed program globals lowered context limited ();
    let result = Hmc_wasm_program_step_call.step program globals lowered context limited next call () in
    match result.Hmc_wasm_program_resource_step.exhausted with
    | None | Some Machine.Heap -> failwith "shared call missing stack exhaustion"
    | Some Machine.Stack ->
      let after = result.Hmc_wasm_program_resource_step.state in
      let expected = Wasm_calls.Finished {Wasm_global_execution.globals = Registers.globals after.State.registers;
        execution = {Wasm_memory_execution.memory = limited.State.memory; machine = {Wasm_execution.locals = Wasm_scalar.Empty;
          stack = Wasm_scalar.Push (Wasm_scalar.I32 after.State.registers.Registers.status, Wasm_scalar.Empty)}}} in
      if Wasm_calls.run result.Hmc_wasm_program_resource_step.fuel (State.module_ program lowered context) (State.loop context limited)
        <> expected then failwith "shared call exhaustion execution";
      if Machine.step program globals limited.State.registers.Registers.heap_limit context.State.stack_capacity (State.configuration limited)
        <> Machine.Exhausted Machine.Stack then failwith "shared call exhaustion source"
let call_one_frame : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable ->
    (next : D.index) @ immutable -> (call : Block.call) @ immutable ->
    {u : unit | State.valid program globals lowered context before
      && I.lookup program.I.code before.State.activation.F.pc === Some (I.Keep (G.Call next))
      && Table.lookup lowered.Lower.blocks before.State.pc === Some (Block.Call call)} -> unit =
  fun program globals lowered context before next call premise ->
    let prepared = stack_budget program globals lowered context before true () in
    let context = prepared.context in
    let limited = prepared.state in
    check_composed program globals lowered context limited ();
    let result = Hmc_wasm_program_step_call.step program globals lowered context limited next call () in
    (match result.Hmc_wasm_program_resource_step.exhausted with
    | Some _ -> failwith "shared call one-frame budget exhausted"
    | None ->
      let after = result.Hmc_wasm_program_resource_step.state in
      if after.State.registers.Registers.top <> after.State.registers.Registers.stack_limit then failwith "shared call did not consume one frame";
      if Wasm_calls.run result.Hmc_wasm_program_resource_step.fuel (State.module_ program lowered context) (State.loop context limited)
        <> Wasm_calls.Running (State.loop context after) then failwith "shared call one-frame execution";
      if Machine.step program globals limited.State.registers.Registers.heap_limit context.State.stack_capacity (State.configuration limited)
        <> Machine.Advanced (State.configuration after) then failwith "shared call one-frame source")
let rec caller_collect : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (configuration : Machine.configuration) @ immutable -> (abstract : Hmc_cfg_semantics.state) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (elapsed : D.index) @ immutable -> int ->
    {u : unit | Inv.valid program globals (limit ()) configuration abstract
      && abstract === U.advance program elapsed (U.initial program input)} -> int =
  fun program globals configuration abstract input elapsed remaining premise ->
    ghost_ (limit_def ());
    if remaining = 0 then failwith "shared caller source fuel" else
    match configuration.Machine.state with
    | Q.Done _ -> 0
    | Q.Stuck -> failwith "shared caller source stuck"
    | Q.Running (activation, frames) ->
      let count = match I.lookup program.I.code activation.F.pc, frames with
      | Some (I.Keep (G.Call next)), _ ->
        List.iter (fun base ->
          let prepared = prepare program globals configuration abstract input elapsed base () in
          if audit program globals prepared.lowered prepared.context prepared.state 1000 () < 3 then failwith "shared ordinary-call segment";
          match Table.lookup prepared.lowered.Lower.blocks prepared.state.State.pc with
          | Some (Block.Call call) ->
            ghost_ (State.configuration_def prepared.state);
            call_without_stack program globals prepared.lowered prepared.context prepared.state next call ();
            call_one_frame program globals prepared.lowered prepared.context prepared.state next call ()
          | _ -> failwith "shared ordinary-call fragment") [256; 263];
        2
      | Some I.Tail_call, _ ->
        List.iter (fun base ->
          let prepared = prepare program globals configuration abstract input elapsed base () in
          if audit program globals prepared.lowered prepared.context prepared.state 1000 () < 2 then failwith "shared tail segment";
          match Table.lookup prepared.lowered.Lower.blocks prepared.state.State.pc with
          | Some (Block.Tail_call env_count) ->
            ghost_ (State.configuration_def prepared.state);
            tail_without_stack program globals prepared.lowered prepared.context prepared.state env_count ()
          | _ -> failwith "shared tail fragment") [256; 263];
        2
      | Some (I.Keep G.Return), Q.Frame _ ->
        List.iter (fun base ->
          let prepared = prepare program globals configuration abstract input elapsed base () in
          if audit program globals prepared.lowered prepared.context prepared.state 100 () < 2 then failwith "shared caller segment") [256; 263];
        2
      | _ -> 0 in
      ghost_ (Inv.step program globals 8192 (D.S (D.S (D.S D.Z))) configuration abstract ();
        Hmc_heap_reachable_operands.advance_next program input elapsed);
      (match Machine.step program globals 8192 (D.S (D.S (D.S D.Z))) configuration with
      | Machine.Exhausted _ -> failwith "shared caller source exhaustion"
      | Machine.Advanced next -> count + caller_collect program globals next (U.step program abstract) input (D.S elapsed) (remaining - 1) ())
let caller_cases source number =
  let program = Hmc_wasm_global_fixture.build source in
  let input = Hmc_wasm_header_update.number number in
  check_initialization program input;
  match Hmc_heap_initialize.initialize program 4096 8192 input () with
  | Hmc_heap_initialize.Heap_exhausted _ -> failwith "shared caller initialization"
  | Hmc_heap_initialize.Initialized start ->
    ghost_ (limit_def (); Hmc_heap_initialize.correct_def program 4096 8192 input (Hmc_heap_initialize.Initialized start);
      U.advance_def program D.Z (U.initial program input));
    let prepared = prepare program start.Hmc_heap_initialize.globals start.Hmc_heap_initialize.configuration (U.initial program input) input D.Z 256 () in
    check_original program start.Hmc_heap_initialize.globals prepared.lowered prepared.context prepared.state ();
    let limited = limit_heap program start.Hmc_heap_initialize.globals prepared.lowered prepared.context prepared.state prepared.state.State.registers.Registers.heap () in
    check_original program start.Hmc_heap_initialize.globals prepared.lowered prepared.context limited ();
    caller_collect program start.Hmc_heap_initialize.globals start.Hmc_heap_initialize.configuration (U.initial program input) input D.Z 500 ()
let divergent_prefixes () =
  let word n = D.Word (Hmc_wasm_header_update.number n) in
  let source = D.Recursive (D.If (D.Primitive (D.Equal_word, D.Bound D.Z, word 0), word 0,
    D.Apply (D.Bound (D.S D.Z), D.Bound D.Z))) in
  let program = Hmc_wasm_global_fixture.build source in
  let input = Hmc_wasm_header_update.number 1 in
  let memory = zeros 8192 in
  let layout = {Hmc_wasm_program_initialize.table_base = 0; frame_base = 263; stack_base = 1024;
    heap_base = 4096; heap_limit = 8192; max_pc = 1000; stack_capacity = D.Z; host_capacity = Wasm_code.Zero} in
  match Bytes.drop memory 8192 with
  | None -> failwith "divergent fixture memory"
  | Some _ ->
    let _ = ghost_ (Bounds.covers_def memory 8192; Hmc_wasm_program_initialize.valid_layout_def layout memory) in
    let initialized = Hmc_wasm_program_initialize.initialize program layout input memory () in
    ghost_ (Hmc_wasm_program_initialize.correct_def program layout input memory initialized);
    match initialized with
    | Hmc_wasm_program_initialize.Initialized (start, prepared) ->
      ghost_ (Hmc_wasm_program_initialize.installed_def program layout input memory start prepared);
      let lowered = prepared.Hmc_wasm_program_initialize.lowered in
      let context = prepared.Hmc_wasm_program_initialize.context in
      let before = prepared.Hmc_wasm_program_initialize.state in
      let limited = limit_heap program start.Hmc_heap_initialize.globals lowered context before before.State.registers.Registers.heap () in
      List.iter (fun count ->
        let prefix = Wasm_execution_budget.of_index (index count) in
        ghost_ (Hmc_wasm_program_run.safe program layout input memory start prepared prefix ());
        check_observation program start.Hmc_heap_initialize.globals lowered context limited prefix ();
        match Wasm_calls.run prefix (State.module_ program lowered context) (Hmc_wasm_program_state_entry.configuration context limited) with
        | Wasm_calls.Running current ->
          (match Registers.read current.Wasm_calls.current.Wasm_instance_control.globals with
          | Some registers ->
            if registers.Registers.top <> limited.State.registers.Registers.top
              || registers.Registers.heap <> limited.State.registers.Registers.heap then failwith "divergent loop consumed resources"
          | None -> failwith "divergent loop registers")
        | _ -> failwith "divergent loop stopped") [0; 1; 3; 31; 127; 511]
    | _ -> failwith "divergent initializer"
let binary_prefixes () =
  let module Init = Hmc_wasm_program_initialize in
  let module Binary = Hmc_wasm_program_binary in
  let module Execute = Wasm_binary_execution in
  let source = D.Lambda (D.Bound D.Z) in
  let input = Hmc_wasm_header_update.number 42 in
  let memory = zeros 65536 in
  let pages : B.u32 = 1 in
  let layout = {Init.table_base = 0; frame_base = 263; stack_base = 1024;
    heap_base = 4096; heap_limit = 8192; max_pc = 1000; stack_capacity = D.Z; host_capacity = Wasm_code.Zero} in
  match Bytes.drop memory 8192 with None -> failwith "binary fixture memory" | Some _ ->
    let _ = ghost_ (Bounds.covers_def memory 8192; Init.valid_layout_def layout memory) in
    let compilation = Hmc_compiler.compile source layout input memory pages () in
    match compilation with
    | Hmc_compiler.Compiled artifact ->
      ghost_ (Hmc_compiler.correct_def source layout input memory pages artifact);
      let program : I.program = match Hmc_specialization.compile source with
        | Hmc_specialization.Compiled monomorphic ->
          I.build (Hmc_cfg_program.build (Hmc_closure_program.build monomorphic))
        | _ -> failwith "diagnostic source compilation" in
      let compiled : {c : Binary.compiled | Binary.compilable program layout input memory pages
        && Binary.accepted program layout input memory pages c} =
        match Binary.compile program layout input memory pages () with
        | Binary.Compiled compiled -> compiled
        | _ -> failwith "diagnostic binary compilation" in
      if compiled.Binary.bytes <> artifact.Hmc_compiler.bytes then failwith "public and diagnostic bytes differ";
      ghost_ (Binary.accepted_def program layout input memory pages compiled);
      let start = compiled.Binary.start in
      let prepared = compiled.Binary.prepared in
      let _ = ghost_ (Hmc_wasm_program_static.dispatcher_typed program prepared.Init.lowered prepared.Init.context
        prepared.Init.state.State.registers) in
      let compiled_again = Binary.sufficient program layout input memory pages () in
      if compiled_again.Binary.bytes <> compiled.Binary.bytes then failwith "binary acceptance completeness";
      ghost_ (Init.correct_def program layout input memory (Init.Initialized (start, prepared)));
      let start_again, prepared_again = Init.sufficient program layout input memory () in
      if start_again <> start || prepared_again <> prepared then failwith "initializer acceptance completeness";
      if Binary.emit program prepared.Init.lowered prepared.Init.context prepared.Init.state 0 <> None
        then failwith "incorrect memory size accepted";
      let bytes = artifact.Hmc_compiler.bytes in
        let _ = ghost_ (Hmc_compiler.static_validity source layout input memory pages artifact ()) in
        if not (Wasm_static_control.function_bodies (State.module_ program prepared.Init.lowered prepared.Init.context)
            (Registers.globals prepared.Init.state.State.registers)) then failwith "binary body typing";
        if not (Wasm_static_module.bytes_valid bytes) then failwith "binary module validity";

        let source_fuel = index 100 in
        (match Hmc_source_semantics.advance source_fuel
            (Hmc_source_semantics.initial (D.Apply (source, D.Word input))) with
        | Hmc_source_semantics.Done (Hm_interpreter_typing.Word word) ->
          let _preserved = ghost_ (Hmc_compiler.preservation source layout input memory pages artifact word source_fuel ()) in ()
        | _ -> failwith "binary source fixture fuel");
        let budget = index 100 in
        (match U.advance program budget prepared.Init.state.State.abstract with
        | Hmc_cfg_semantics.Done (Hmc_closure_semantics.V.Word word) ->
          if Hmc_heap_extent.fits (Hmc_heap_demand.heap_plan program budget prepared.Init.state.State.abstract)
              (H.used prepared.Init.state.State.heap) prepared.Init.state.State.registers.Registers.heap_limit &&
              Hmc_frame_capacity.le (Hmc_heap_demand.stack_plan program budget prepared.Init.state.State.abstract) prepared.Init.context.State.stack_capacity then (
            let normal = Binary.normal program layout input memory start prepared pages compiled.Binary.bytes word budget () in
            if Execute.run (Wasm_code.Succ normal.Hmc_wasm_program_execution.fuel) compiled.Binary.bytes
                (Wasm_code.Succ prepared.Init.context.State.host_capacity) <>
                Execute.Result (Hmc_wasm_program_execution.target prepared.Init.context normal.Hmc_wasm_program_execution.endpoint)
              then failwith "binary sufficient-resource execution")
          else failwith "binary fixture resource bounds"
        | _ -> failwith "binary abstract fixture fuel");
        List.iter (fun n ->
          let prefix = Wasm_execution_budget.of_index (index n) in
          ghost_ (Hmc_compiler.safe source layout input memory pages artifact prefix ());
          match Execute.run prefix bytes (Wasm_code.Succ layout.host_capacity) with
          | Execute.Result (Wasm_calls.Running _) -> if n = 511 then failwith "binary fixture did not return"
          | Execute.Result (Wasm_calls.Finished after) ->
            (match Registers.read after.Wasm_global_execution.globals with None -> failwith "binary result registers" | Some registers ->
              match after.Wasm_global_execution.execution.Wasm_memory_execution.machine.Wasm_execution.stack with
              | Wasm_scalar.Push (Wasm_scalar.I32 status, Wasm_scalar.Empty) ->
                if status <> registers.Registers.status || status <> 1 then failwith "binary result status" else
                if registers.Registers.tag.Hmc_word64.lo <> 1 || registers.Registers.tag.Hmc_word64.hi <> 0 then failwith "binary result tag" else
                let word = registers.Registers.payload in
                let _ = ghost_ (Hmc_tagged_cell.tag_def (Hmc_tagged_cell.Word word)) in
                let _source_steps = ghost_ (Hmc_compiler.reflection source layout input memory pages artifact prefix after registers word ()) in
                if word <> input || Hmc_source_semantics.advance source_fuel
                    (Hmc_source_semantics.initial (D.Apply (source, D.Word input)))
                    <> Hmc_source_semantics.Done (Hm_interpreter_typing.Word word) then failwith "binary source reflection"
              | _ -> failwith "binary result stack")
          | _ -> failwith "binary unsafe prefix") [0; 1; 31; 511]
    | _ -> failwith "binary fixture initialization"
let fixtures () =
  let module Capacity = Hmc_memory_stack_capacity in
  if Capacity.reserve 16 (index 3) 32 80 () <> Some 80 then failwith "exact stack capacity rejected";
  if Capacity.reserve 16 (index 3) 32 79 () <> None then failwith "insufficient stack capacity accepted";
  if Capacity.reserve 16 D.Z 32 32 () <> Some 32 then failwith "empty stack capacity rejected";
  if Capacity.remaining_capacity 16 (index 3) 48 <> Some 0 then failwith "exact stack remaining bytes";
  binary_prefixes ();
  divergent_prefixes ();
  let word n = D.Word (Hmc_wasm_header_update.number n) in
  let recursive = D.Recursive (D.If (D.Primitive (D.Equal_word, D.Bound D.Z, word 0), word 17,
    D.Primitive (D.Add, word 1, D.Apply (D.Bound (D.S D.Z), D.Primitive (D.Subtract, D.Bound D.Z, word 1))))) in
  let callers = caller_cases (D.Lambda (D.Primitive (D.Add, D.Bound D.Z, D.Apply (recursive, word 2)))) 42 in
  let tail = caller_cases (D.Recursive (D.If (D.Primitive (D.Equal_word, D.Bound D.Z, word 0), word 17,
    D.Apply (D.Bound (D.S D.Z), D.Primitive (D.Subtract, D.Bound D.Z, word 1))))) 8 in
  let polymorphic = caller_cases (D.Let (D.Lambda (D.Bound D.Z), D.Lambda (D.If (
    D.Apply (D.Bound (D.S D.Z), D.Truth), D.Apply (D.Bound (D.S D.Z), D.Bound D.Z), word 0)))) 42 in
  let captured_call = caller_cases (D.Lambda (D.Let (D.Lambda (D.Primitive (D.Add, D.Bound D.Z, D.Bound (D.S D.Z))),
    D.Apply (D.Bound D.Z, word 1)))) 42 in
  let yes = cases (D.Lambda (D.If (D.Truth, word 1, word 2))) in
  let no = cases (D.Lambda (D.If (D.False, word 1, word 2))) in
  let local = cases (D.Lambda (D.Bound D.Z)) in
  let outer = cases (D.Lambda (D.Let (word 7, D.Bound (D.S D.Z)))) in
  let global = cases (D.Let (D.Lambda (D.Primitive (D.Add, D.Bound D.Z, word 1)),
    D.Lambda (D.Let (D.Bound (D.S D.Z), D.Bound (D.S D.Z))))) in
  let nil = cases (D.Lambda (D.CaseList (D.Nil, word 0, D.Bound D.Z))) in
  let cons = cases (D.Lambda (D.CaseList (D.Cons (word 7, D.Nil), word 0, D.Bound D.Z))) in
  let closure = cases (D.Lambda (D.Let (D.Lambda (D.Primitive (D.Add, D.Bound D.Z, D.Bound (D.S D.Z))), D.Bound (D.S D.Z)))) in
  let nested = cases (D.Lambda (D.CaseList (D.Cons (word 7, D.Cons (word 8, D.Nil)), word 0, D.Bound D.Z))) in
  let captured_list = cases (D.Lambda (D.CaseList (
    D.Cons (D.Lambda (D.Primitive (D.Add, D.Bound D.Z, D.Bound (D.S D.Z))), D.Nil), word 0, word 1))) in
  let max_word = D.Word {Hmc_word64.lo = 4294967295; hi = 4294967295} in
  let add = cases (D.Lambda (D.Primitive (D.Add, max_word, word 1))) in
  let sub = cases (D.Lambda (D.Primitive (D.Subtract, word 0, word 1))) in
  let equal = cases (D.Lambda (D.If (D.Primitive (D.Equal_word, max_word, max_word), word 1, word 2))) in
  let less = cases (D.Lambda (D.If (D.Primitive (D.Unsigned_less, word 0, max_word), word 1, word 2))) in
  if polymorphic <> 8 || captured_call <> 4 || tail <> 16 || callers <> 12 || nested <> 10 || captured_list <> 8 || closure <> 6 || nil <> 4 || cons <> 8 || global <> 6 || yes <> 4 || no <> 4 || local <> 4 || outer <> 4 || add <> 4 || sub <> 4 || equal <> 6 || less <> 6
    then failwith "shared instruction coverage"
