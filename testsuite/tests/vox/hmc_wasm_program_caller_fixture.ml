module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module Program = Hmc_tail_ir
module Machine = Hmc_heap_machine
module Inv = Hmc_heap_invariant
module Abstract = Hmc_tail_semantics
module Codec = Hmc_pointer_frame_codec
module Cap = Hmc_frame_capacity
module Index = Hmc_u32_index
module Pad = Hmc_wasm_frame_padding
module Header = Hmc_wasm_header_update
module Cells = Hmc_wasm_call_save_memory
module Check = Hmc_memory_frame_check
module Lower = Hmc_wasm_program_lower
module Stack = Hmc_memory_stack
module Saved = Hmc_memory_saved_frame
module Image = Hmc_heap_image
module Above = Hmc_heap_image_suffix
module Bounds = Hmc_linear_bounds
module Bytes = Hmc_linear_bytes
module Runtime = Hmc_runtime_closures
module Table = Hmc_runtime_descriptor_table
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module T = Wasm_control
module I = Wasm_instruction
module C = Wasm_code
module L = Wasm_locals
module New = Hmc_wasm_program_caller
module Full = Hmc_wasm_return_frame
module Emit = Hmc_wasm_program_emit
module Block = Hmc_wasm_structured_block
module Assembly = Hmc_wasm_program_functions
module Round = Hmc_wasm_program_roundtrip
module R = Wasm_global_registers
module WG = Wasm_globals
module GE = Wasm_global_execution
module Func = Wasm_functions
module Calls = Wasm_calls
module P = Wasm_instance_control
type fixture = {memory : B.bytes; expected : B.bytes; block : T.code; globals : WG.t; depth : D.index; top : B.u32}
let rec fill n = if n = 0 then B.End else B.Byte (173, fill (n - 1))
let[@def] (bound @ total) (unit : unit) : B.u32 = 8192
let rec (region_above @ total) : (heap : H.heap) @ immutable -> (boundary : B.u32) ->
    {out : bool | out === Hmc_wasm_heap_suffix.above heap boundary} = fun heap boundary ->
  ghost_ (Hmc_wasm_heap_suffix.above_def heap boundary);
  match heap with H.Empty_heap base -> boundary <= base | H.Allocate (a, rest) -> boundary <= a.H.address && region_above rest boundary
let fixture : (stack_base : B.u32) -> (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (configuration : Machine.configuration) @ immutable -> (abstract : Hmc_cfg_semantics.state) @ immutable ->
    (offset : B.u32) -> {u : unit | Inv.valid program globals (bound ()) configuration abstract} -> fixture =
  fun stack_base program globals configuration abstract offset premise ->
  if stack_base < 1024 || stack_base > 8192 then failwith "caller stack base" else
  if offset > 7 then failwith "caller alignment" else
  let source : B.u32 = 256 + offset in
  let heap = configuration.Machine.heap in
  let blocks = program.Program.origin.Hmc_cfg_program.blocks in
  let table = program.Program.origin.Hmc_cfg_program.origin.Hmc_closure_program.table in
  let _ = ghost_ (bound_def (); Inv.valid_def program globals 8192 configuration abstract) in
  match configuration.Machine.state, Lower.lower program globals 1000 with
  | Q.Running (activation, Q.Frame (saved, frames)), Some lowered ->
    let capacity = lowered.Lower.capacity in
    let width = lowered.Lower.width in
    if capacity < 2 || capacity > 32 || width = 0 || H.used heap > 8192 then failwith "caller layout bounds" else
    (match Program.lookup program.Program.code activation.F.pc, activation.F.temporaries,
      G.lookup blocks activation.F.pc, G.lookup blocks saved.F.pc,
      Index.encode 1000 activation.F.pc, Index.encode 1000 saved.F.pc, Index.encode 1000 (G.size blocks) with
    | Some (Program.Keep G.Return), F.Empty, Some active_block, Some saved_block, Some source_pc, Some pc, Some block_count ->
      if Hmc_wasm_program_table.lookup lowered.Lower.blocks source_pc <> Some Hmc_wasm_program_block.Return then failwith "caller lowered fragment" else
      let signature = saved_block.G.signature in
      if not (Codec.shape signature saved && Codec.shape active_block.G.signature activation) then failwith "caller source frame shapes" else
      let _ = ghost_ (Lower.corresponds_def program globals 1000 lowered;
        Cap.lookup blocks saved.F.pc saved_block (); Hmc_pointer_frame_shape.size signature;
        Cap.lookup blocks activation.F.pc active_block (); Hmc_pointer_frame_shape.size active_block.G.signature) in
      let padding = Pad.cells (Cap.remaining (Cap.capacity blocks) (Codec.size signature) ()) in
      let active_padding = Pad.cells (Cap.remaining (Cap.capacity blocks) (Codec.size active_block.G.signature) ()) in
      ghost_ (Pad.length (Cap.remaining (Cap.capacity blocks) (Codec.size signature) ());
        Pad.length (Cap.remaining (Cap.capacity blocks) (Codec.size active_block.G.signature) ()));
      let saved_cells = Codec.encode signature saved padding () in
      let active_cells = Codec.encode active_block.G.signature activation active_padding () in
      (match saved_cells, active_cells with
      | H.Cell (_, H.Cell (_, rest)), H.Cell (_, H.Cell (_, source_rest)) ->
        ghost_ (Codec.decode_def signature saved.F.pc saved_cells; Codec.decode_def active_block.G.signature activation.F.pc active_cells);
        let active_full = Cells.cells source_pc activation.F.current activation.F.accumulator source_rest in
        let saved_full = Cells.cells pc saved.F.current saved.F.accumulator rest in
        let source_stop : B.u32 = source + width in
        if source_stop > 1024 || not (Hmc_wasm_reached_cons_fixture.above heap source_stop && Image.encodable 1000 heap) then failwith "caller heap region" else
        let initial = fill 16384 in
        (match Bytes.drop initial 16384 with
        | None -> failwith "caller initial memory"
        | Some _ ->
          ghost_ (Bounds.covers_def initial 16384);
          let heap_memory = Image.materialize table 1000 initial heap 16384 () in
          (match Runtime.lower 1000 table program.Program.origin.Hmc_cfg_program.functions with
          | None -> failwith "caller runtime descriptors"
          | Some runtime ->
            (match Index.encode 7 (Runtime.size runtime) with
            | None -> failwith "caller descriptor count"
            | Some table_count ->
              let descriptor_memory = Table.store runtime heap_memory 0 table_count 16384 () in
              if not (Hmc_wasm_reached_cons_fixture.above heap (32 * table_count)) then failwith "caller descriptor overlap" else
              let _ = ghost_ (Table.address_def 0 table_count; S.add32_def 0 (32 * table_count);
                let _ = Bounds.suffix heap_memory 16384 (32 * table_count) () in
                Above.preserve heap_memory descriptor_memory heap (32 * table_count) ();
                Cells.cells_def source_pc activation.F.current activation.F.accumulator source_rest;
                H.length_def active_full; Saved.slots_def blocks; Stack.zero_def ();
                Index.represents_def (D.S (Cap.capacity blocks)) (1 + capacity)) in
              (match Hmc_wasm_reservation.reserve (Saved.slots blocks) (1 + capacity) 0 width (),
                Hmc_wasm_reservation.reserve (H.length active_full) (1 + capacity) source source_stop () with
              | Some _, Some _ ->
                let frame_memory = Hmc_memory_cells.store descriptor_memory 16384 source source_stop active_full () in
                let _ = ghost_ (let _ = Bounds.suffix descriptor_memory 16384 source_stop () in
                  Above.preserve descriptor_memory frame_memory heap source_stop ();
                  Table.preserve runtime descriptor_memory frame_memory 0 table_count source ()) in
                (match Hmc_memory_stack_image.materialize blocks 1000 width frame_memory stack_base 16384 (Q.Frame (saved, frames)) () with
                | None -> failwith "caller stack materialization"
                | Some stack ->
                  let memory = stack.Stack.memory in
                  let top = stack.Stack.top in
                  let base = Stack.previous width top in
                  if H.used heap > stack_base && not (Hmc_wasm_reached_cons_fixture.above heap top) then failwith "caller stack heap separation" else
                  let _ = ghost_ ((if H.used heap <= stack_base then Image.preserve table frame_memory memory heap stack_base ()
                    else (let _ = Bounds.suffix frame_memory 16384 top () in Above.preserve frame_memory memory heap top ()));

                    Table.preserve runtime frame_memory memory 0 table_count stack_base ();
                    Stack.related_def blocks width memory stack_base top (Q.Frame (saved, frames)); Stack.previous_def width top;
                    Hmc_wasm_frame_restore.width_def capacity;
                    S.add32_def source width;
                    Index.injective (H.length saved_cells) (Cap.capacity blocks) capacity ()) in
                  (match Check.check memory source active_full, Check.check memory base saved_full with
                  | Some active, Some stored ->
                    if Check.check memory source (H.Cell (V.Word (Header.number 4294967295), active_cells)) <> None then failwith "corrupt frame header accepted";
                    let globals_values = S.Push (S.I32 source, S.Push (S.I32 top, S.Push (S.I32 99, S.Empty))) in
                    let wasm_globals = {WG.values = globals_values; permissions = WG.Global (false, WG.Global (true, WG.Global (true, WG.Empty)))} in
                    let local_config = {Emit.structured = {Block.frame = 0; heap = 0; limit = 0; object_ = 0;
                      scratch = {Hmc_wasm_cons_capture.head_tag = 3; head_payload = 4; tail_tag = 3; tail_payload = 4}};
                      top = 1; stack_limit = 0; code = 0; address = 0; descriptor = {Hmc_wasm_descriptor_load.start = 0; captures = 0; recursive = 0};
                      status = 2; result_tag = 3; result_payload = 4} in
                    let config = {Assembly.locals = local_config;
                      local_types = Func.Local32 (Func.Local32 (Func.Local32 (Func.Local64 (Func.Local64 Func.No_locals))));
                      loads = R.Binding (0, 0, R.Binding (1, 1, R.Binding (2, 2, R.End)));
                      stores = R.Binding (1, 1, R.Binding (2, 2, R.End)); table_base = 0; stack_base} in
                    let initial = {GE.globals = wasm_globals; execution = {X.memory; machine = {E.locals = Func.zero_locals config.Assembly.local_types; stack = S.Empty}}} in
                    (match R.load config.Assembly.loads initial with
                    | None -> failwith "caller import"
                    | Some imported ->
                      let state = imported.GE.execution in
                      (match L.get state.X.machine.E.locals 0, L.get state.X.machine.E.locals 1 with
                      | Some (S.I32 actual_source), Some (S.I32 actual_top) ->
                        if actual_source <> source || actual_top <> top || not (L.can_set state.X.machine.E.locals 2 (S.I32 0)) then failwith "caller imported layout" else
                        let _ = ghost_ (Hmc_wasm_program_status.zero_def ()) in
                        let result = New.correct lowered local_config runtime 0 table_count (Round.labels config) program globals heap 8192 (D.S (D.S (D.S (D.S (D.S D.Z))))) activation saved frames signature pc source_pc rest padding source_rest lowered.Lower.restore capacity state source base top stack_base 16384 stored.Check.bytes stored.Check.suffix active.Check.bytes active.Check.suffix 0 1 () in
                        let resumed = {saved with F.accumulator = activation.F.accumulator} in
                        if not (Codec.shape signature resumed) then failwith "caller successor shape" else
                        let resumed_cells = Codec.encode signature resumed padding () in
                        let resumed_full = H.Cell (V.Word (Header.number pc), resumed_cells) in
                        let _ = ghost_ (H.length_def resumed_full;
                          Hmc_memory_saved_frame.slots_def blocks;
                          Index.injective (H.length resumed_cells) (Cap.capacity blocks) capacity ()) in
                        let expected_saved = Hmc_memory_cells.store memory 16384 base top resumed_full () in
                        let expected = Hmc_memory_cells.store expected_saved 16384 source source_stop resumed_full () in
                        if expected <> result.New.source.Full.state.X.memory then failwith "caller independently encoded successor";
                        (match Hmc_wasm_program_table.lookup lowered.Lower.blocks source_pc with
                        | Some Hmc_wasm_program_block.Return ->
                        let selection = Hmc_wasm_program_selection.select_reachable program globals lowered 1000 block_count
                          8192 configuration abstract activation (Q.Frame (saved, frames)) source_pc 0 stack_base () in
                        let canonical = Hmc_wasm_program_runtime.config 0 stack_base in
                        let registers = {Hmc_wasm_program_registers.frame = source; heap = H.used heap; heap_limit = 8192;
                          top; stack_limit = top; status = 99; tag = Header.number 123; payload = Header.number 456} in
                        let canonical_state = {X.memory; machine = {E.locals = Hmc_wasm_program_registers.locals registers; stack = S.Empty}} in
                        let canonical_module = Assembly.assemble lowered (G.size blocks) block_count canonical (Hmc_wasm_program_runtime.dispatcher ()) in
                        ghost_ (Hmc_wasm_program_runtime.config_def 0 stack_base;
                          Hmc_wasm_program_registers.local_values registers;
                          Hmc_wasm_program_registers.matches_def (Hmc_wasm_program_registers.locals registers) registers;
                          L.can_set_def (Hmc_wasm_program_registers.locals registers) 11 (S.I32 0); S.same_type_def (S.I32 99) (S.I32 0));
                        let shared_ready = region_above heap top && region_above heap source_stop && top <= H.used heap in
                        if stack_base = 1024 && not shared_ready then failwith "caller shared regions" else
                        let step = if shared_ready then (
                          ghost_ (let _ = Bounds.suffix memory 16384 top () in
                            let _ = Bounds.suffix memory 16384 8192 () in
                            Bounds.covers_def memory 8192; Bounds.covers_def memory top;
                            Hmc_wasm_program_resources.valid_def program globals width stack_base source_stop abstract heap activation (Q.Frame (saved, frames)) registers memory;
                            Hmc_wasm_program_descriptors.valid_def program registers memory runtime 0 table_count);
                          Hmc_wasm_program_source_caller.framed abstract source_stop (capacity + 1) canonical_module registers C.Zero selection.Hmc_wasm_program_selection.selected.Hmc_wasm_program_selection.index
                          lowered canonical.Assembly.locals runtime 0 table_count (Round.labels canonical) program globals heap 8192 (D.S (D.S (D.S (D.S (D.S D.Z)))))
                          activation saved frames signature pc source_pc rest padding source_rest lowered.Lower.restore capacity canonical_state source base top stack_base 16384
                          stored.Check.bytes stored.Check.suffix active.Check.bytes active.Check.suffix 0 4 ()) else
                          Hmc_wasm_program_source_caller.correct canonical_module registers C.Zero selection.Hmc_wasm_program_selection.selected.Hmc_wasm_program_selection.index
                          lowered canonical.Assembly.locals runtime 0 table_count (Round.labels canonical) program globals heap 8192 (D.S (D.S (D.S (D.S (D.S D.Z)))))
                          activation saved frames signature pc source_pc rest padding source_rest lowered.Lower.restore capacity canonical_state source base top stack_base 16384
                          stored.Check.bytes stored.Check.suffix active.Check.bytes active.Check.suffix 0 4 () in
                        if step.Hmc_wasm_program_source_caller.registers <> {registers with Hmc_wasm_program_registers.top = base; status = 0}
                          || step.Hmc_wasm_program_source_caller.body.New.source.Full.state.X.memory <> expected then failwith "caller dispatcher result";
                        (match Calls.run step.Hmc_wasm_program_source_caller.fuel canonical_module
                          (Hmc_wasm_program_dispatch.loop (Hmc_wasm_program_registers.globals registers) memory (C.Succ C.Zero)) with
                        | Calls.Running actual ->
                          if actual <> Hmc_wasm_program_dispatch.loop (Hmc_wasm_program_registers.globals step.Hmc_wasm_program_source_caller.registers) expected (C.Succ C.Zero)
                          then failwith "caller dispatcher execution"
                        | _ -> failwith "caller dispatcher control")
                        | _ -> failwith "caller dispatcher fragment");
                        let target = Assembly.function_ lowered Hmc_wasm_program_block.Return config in
                        let module_ = {Func.functions = Func.Function (target, Func.No_functions); signatures = Func.Signature (Func.Void, Func.No_signatures); table = Func.Element (Some 0, Func.No_elements)} in
                        let call = {Calls.current = {P.globals = wasm_globals; body = {T.code = T.Instruction (I.Call_indirect 0, T.Empty); labels = T.No_labels;
                          state = {X.memory; machine = {E.locals = S.Empty; stack = S.Push (S.I32 0, S.Empty)}}}};
                          result = Func.Void; callers = Calls.Root; capacity = C.Succ C.Zero} in
                        let after = result.New.source.Full.state in
                        (match R.store config.Assembly.stores {GE.globals = wasm_globals; execution = after} with
                        | None -> failwith "caller export"
                        | Some exported ->
                          ghost_ (Func.signature_def module_.Func.signatures 0; Func.element_def module_.Func.table 0; Func.lookup_def module_.Func.functions 0;
                            Round.correct lowered Hmc_wasm_program_block.Return config module_ call 0 0 0 T.Empty S.Empty C.Zero state after exported result.New.fuel ());
                          (match Calls.run (Round.cost config result.New.fuel) module_ call with
                          | Calls.Running actual ->
                            if actual.Calls.current.P.body.T.state.X.memory <> after.X.memory || actual.Calls.callers <> Calls.Root || actual.Calls.capacity <> call.Calls.capacity
                              || WG.get actual.Calls.current.P.globals 1 <> Some (S.I32 base) || WG.get actual.Calls.current.P.globals 2 <> Some (S.I32 0) then failwith "caller indirect result"
                          | _ -> failwith "caller indirect execution");
                          {memory; expected; block = target.Func.code; globals = wasm_globals; depth = Q.depth (Q.Frame (saved, frames)); top = base})
                      | _ -> failwith "caller imported types"))
                  | _ -> failwith "caller materialized frames"))
              | _ -> failwith "caller frame extent"))))
      | _ -> failwith "caller frame headers")
    | _ -> failwith "caller source lookup")
  | _ -> failwith "caller source state"
let rec collect : (stack_base : B.u32) -> (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (configuration : Machine.configuration) @ immutable -> (abstract : Hmc_cfg_semantics.state) @ immutable ->
    int -> {u : unit | Inv.valid program globals (bound ()) configuration abstract} -> fixture list =
  fun stack_base program globals configuration abstract fuel premise ->
    ghost_ (bound_def ());
    match configuration.Machine.state with
    | Q.Done _ -> []
    | Q.Stuck -> failwith "caller source stuck"
    | Q.Running (activation, frames) ->
      if fuel = 0 then failwith "caller source fuel" else
      let fixtures = match Program.lookup program.Program.code activation.F.pc, frames with
        | Some (Program.Keep G.Return), Q.Frame _ -> [fixture stack_base program globals configuration abstract 0 (); fixture stack_base program globals configuration abstract 7 ()]
        | _ -> [] in
      let stack_limit = D.S (D.S (D.S (D.S (D.S D.Z)))) in
      ghost_ (Inv.step program globals 8192 stack_limit configuration abstract ());
      match Machine.step program globals 8192 stack_limit configuration with
      | Machine.Exhausted _ -> failwith "caller source exhaustion"
      | Machine.Advanced next -> fixtures @ collect stack_base program globals next (Abstract.step program abstract) (fuel - 1) ()
let cases stack_base (heap_base : B.u32) =
  if heap_base > 4096 then failwith "caller heap base" else
  let word n = D.Word (Header.number n) in
  let var n = D.Bound (Hmc_wasm_control_fixture.index n) in
  let term = D.Recursive (D.If (D.Primitive (D.Equal_word, var 0, word 0), word 17,
    D.Primitive (D.Add, word 1, D.Apply (var 1, D.Primitive (D.Subtract, var 0, word 1))))) in
  let program = Hmc_wasm_global_fixture.build term in
  let input = Header.number 4 in
  match Hmc_heap_initialize.initialize program heap_base 8192 input () with
  | Hmc_heap_initialize.Heap_exhausted _ -> failwith "caller initialization"
  | Hmc_heap_initialize.Initialized start ->
    ghost_ (bound_def (); Hmc_heap_initialize.correct_def program heap_base 8192 input (Hmc_heap_initialize.Initialized start));
    let fixtures = collect stack_base program start.Hmc_heap_initialize.globals start.Hmc_heap_initialize.configuration (Abstract.initial program input) 1000 () in
    if List.length fixtures <> 8 then failwith "caller return coverage";
    List.iter (fun depth ->
      if List.length (List.filter (fun fixture -> fixture.depth = Hmc_wasm_control_fixture.index depth) fixtures) <> 2 then failwith "caller depth coverage") [1; 2; 3; 4];
    fixtures

let fixtures () =
  let fixtures = cases 8192 1024 in
  let shared = cases 1024 4096 in
  if List.length shared <> 8 then failwith "caller shared-invariant coverage";
  fixtures
