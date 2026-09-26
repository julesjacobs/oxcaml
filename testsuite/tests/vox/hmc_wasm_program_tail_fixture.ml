module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Frame = Hmc_heap_frame
module State = Hmc_heap_state
module Machine = Hmc_heap_machine
module Program = Hmc_tail_ir
module Inv = Hmc_heap_invariant
module Abstract = Hmc_tail_semantics
module Codec = Hmc_pointer_frame_codec
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module T = Wasm_control
module I = Wasm_instruction
module C = Wasm_code
module Capture = Hmc_wasm_cons_capture
module Entry = Hmc_wasm_loaded_call
module Dispatch = Hmc_wasm_call_dispatch
module Plan_entry = Hmc_wasm_call_plan_enter
module Runtime = Hmc_runtime_closures
module Table = Hmc_runtime_descriptor_table
module Slots = Hmc_wasm_descriptor_load
module Separate = Hmc_wasm_selected_call_entry
module Dynamic = Hmc_wasm_dynamic_call_entry
module Plans = Hmc_wasm_call_plan_table
module Select = Hmc_wasm_call_plan_select
module Guarded = Hmc_wasm_closure_guarded
module Write = Hmc_wasm_closure_write
module Structured = Hmc_wasm_structured_block
module Image = Hmc_heap_image
module Above = Hmc_heap_image_suffix
module L = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
module New = Hmc_wasm_program_tail
module Lower = Hmc_wasm_program_lower
module Emit = Hmc_wasm_program_emit
module Block = Hmc_wasm_program_block
module R = Wasm_global_registers
module GE = Wasm_global_execution
module WG = Wasm_globals
module Func = Wasm_functions
module Assembly = Hmc_wasm_program_functions
module Calls = Wasm_calls
module P = Wasm_instance_control
module Round = Hmc_wasm_program_roundtrip
module Check = Hmc_wasm_program_call_fixture
type fixture = {memory : B.bytes; expected : B.bytes; block : T.code; globals : WG.t; cursor : B.u32}
let rec (above @ total) : (heap : Heap.heap) @ immutable -> (boundary : B.u32) ->
    {out : bool | out === Above.above heap boundary} = fun heap boundary ->
  ghost_ (Above.above_def heap boundary);
  match heap with Heap.Empty_heap _ -> true | Heap.Allocate (a, rest) -> boundary <= a.Heap.address && above rest boundary
let rec (region_above @ total) : (heap : Heap.heap) @ immutable -> (boundary : B.u32) ->
    {out : bool | out === Hmc_wasm_heap_suffix.above heap boundary} = fun heap boundary ->
  ghost_ (Hmc_wasm_heap_suffix.above_def heap boundary);
  match heap with
  | Heap.Empty_heap base -> boundary <= base
  | Heap.Allocate (allocation, rest) -> boundary <= allocation.Heap.address && region_above rest boundary
let rec fuel n = if n = 0 then C.Zero else C.Succ (fuel (n - 1))
let rec filler n = if n = 0 then B.End else B.Byte (173, filler (n - 1))
let[@def] (bound @ total) (u : unit) : B.u32 = 8192
module Code = Hmc_wasm_closure_code
module Copy = Hmc_wasm_call_captures
module Memory = Hmc_wasm_dynamic_call_frame
module Words = Hmc_wire_word_sequence
let fixture : (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (configuration : Machine.configuration) @ immutable -> (abstract : Hmc_cfg_semantics.state) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (elapsed : D.index) @ immutable ->
    {u : unit | Inv.valid program globals (bound ()) configuration abstract
      && abstract === Abstract.advance program elapsed (Abstract.initial program input)} -> fixture =
  fun program globals configuration abstract input elapsed premise ->
    ghost_ (bound_def (); Inv.valid_def program globals 8192 configuration abstract);
    match Lower.lower program globals 1000 with
    | None -> failwith "tail program lowering"
    | Some lowered ->
    let _ = ghost_ (Lower.corresponds_def program globals 1000 lowered) in
    let heap = configuration.Machine.heap in
    let table = program.Program.origin.Hmc_cfg_program.origin.Hmc_closure_program.table in
    match configuration.Machine.state with
    | State.Running (activation, State.Halt) ->
      let _ = ghost_ (Hmc_heap_reachable_operands.progress program input elapsed abstract ()) in
      (match Program.lookup program.Program.code activation.Frame.pc with
      | None -> failwith "call instruction lookup"
      | Some instruction ->
      if not (Hmc_heap_operand_shapes.call_instruction instruction) then failwith "call instruction" else
      let fields = Hmc_heap_operand_shapes.call program heap activation State.Halt abstract instruction () in
      (match G.lookup program.Program.origin.Hmc_cfg_program.blocks activation.Frame.pc with
      | None -> failwith "source signature"
      | Some block ->
      let signature = block.G.signature in
      match signature.G.temporaries with
      | G.Empty_temporaries | G.Environment _ -> failwith "source call schema"
      | G.Value (context, ty, schema) ->
      let _ = ghost_ (Hmc_wasm_program_selection.frame_shape program globals 8192 configuration abstract activation State.Halt block ()) in
      let _ = ghost_ (Hmc_frame_capacity.lookup program.Program.origin.Hmc_cfg_program.blocks activation.Frame.pc block ();
        Hmc_pointer_frame_shape.size signature) in
      let padding_count = Hmc_frame_capacity.remaining (Hmc_frame_capacity.capacity program.Program.origin.Hmc_cfg_program.blocks) (Codec.size signature) () in
      let source_padding = Hmc_wasm_frame_padding.cells padding_count in
      ghost_ (Hmc_wasm_frame_padding.length padding_count);
      let source_cells = Codec.encode signature activation source_padding () in
      (match Hmc_u32_index.encode 64 (Heap.length source_cells), Hmc_u32_index.encode 1000 activation.Frame.pc,
        Hmc_u32_index.encode 64 (Codec.locals_size signature.G.locals) with
      | Some source_count, Some old_pc, Some env_count ->
      let address = fields.Hmc_heap_operand_shapes.address in
      let id = fields.Hmc_heap_operand_shapes.id in
      let captures = fields.Hmc_heap_operand_shapes.captured in
          (match Hmc_u32_index.encode 1000 (Heap.length captures), Hmc_u32_index.encode 1000 id with
          | Some count, Some index ->
            if not (Image.encodable 1000 heap) then failwith "call heap encoding" else
            let initial = filler 8192 in
            (match L.drop initial 8192 with
            | None -> failwith "call initial coverage"
            | Some _ ->
              ghost_ (Bounds.covers_def initial 8192);
              let heap_memory = Image.materialize table 1000 initial heap 8192 () in
              match Runtime.lower 1000 table program.Program.origin.Hmc_cfg_program.functions with
              | None -> failwith "runtime descriptor lowering"
              | Some runtime ->
              match Hmc_u32_index.encode 7 (Runtime.size runtime) with
              | None -> failwith "descriptor table fixture size"
              | Some table_count ->
              let memory = Table.store runtime heap_memory 0 table_count 8192 () in
              ghost_ (Table.address_def 0 table_count; S.add32_def 0 (32 * table_count));
              match above heap (32 * table_count) with
              | false -> failwith "descriptor table overlaps heap"
              | true ->
              ghost_ (let _ = Bounds.suffix heap_memory 8192 (32 * table_count) () in
                Above.preserve heap_memory memory heap (32 * table_count) ());
              match Runtime.lookup runtime index with
              | None -> failwith "descriptor lookup"
              | Some descriptor ->
              (match Hmc_closure_ir.lookup table id with
              | None -> failwith "capture entry lookup"
              | Some entry ->
              if count > 64 then failwith "capture fixture count" else
              let minimum : Hmc_wasm_relayout.count = (if entry.Hmc_closure_ir.recursive then 4 else 3) + count in
              if Copy.build entry (minimum - 1) <> None || Copy.build entry minimum = None then failwith "capture capacity boundary";
              (match Copy.build entry lowered.Lower.capacity with
              | None -> failwith "capture plan build"
              | Some fragment ->
              match Some lowered.Lower.calls with
              | None -> failwith "call plan table"
              | Some plans ->
              (match Hmc_cfg_program.lookup program.Program.origin.Hmc_cfg_program.functions id with
              | None -> failwith "callee lookup"
              | Some function_ ->
              (match Hmc_u32_index.encode 1000 function_.Hmc_cfg_program.start with
              | None -> failwith "callee PC encoding"
              | Some pc ->
              let argument = activation.Frame.accumulator in
              let prefix4 = Heap.Cell (V.Nil, Heap.Cell (V.Nil, Heap.Cell (V.Nil, Heap.Cell (V.Nil, Heap.Empty)))) in
              let prefix_cells = if entry.Hmc_closure_ir.recursive then Heap.Cell (V.Nil, prefix4) else prefix4 in
              (match Hmc_u32_index.encode 10 (Heap.length prefix_cells) with
              | None -> failwith "capture prefix count"
              | Some prefix_count ->
              if prefix_count <> (if entry.Hmc_closure_ir.recursive then 5 else 4) then failwith "capture prefix layout" else
              let offset : B.u32 = 16 * prefix_count in
              let target : B.u32 = 256 + offset in
              let stop : B.u32 = target + 16 * count in
              let original_memory = memory in
              let full = Heap.Cell (V.Word (Header.number old_pc), source_cells) in
              ghost_ (Heap.length_def full; Hmc_u32_index.represents_def (Heap.length full) (source_count + 1));
              match Hmc_wasm_reservation.reserve (Heap.length full) (source_count + 1) 256 8192 () with
              | None -> failwith "source frame extent"
              | Some source_stop ->
              let memory = Hmc_memory_cells.store original_memory 8192 256 source_stop full () in
              if not (above heap source_stop) then failwith "source frame overlaps heap" else (
              ghost_ (let _ = Bounds.suffix original_memory 8192 source_stop () in
                Above.preserve original_memory memory heap source_stop ();
                Table.preserve runtime original_memory memory 0 table_count 256 ());
              match L.drop memory 256 with
              | None -> failwith "source bytes"
              | Some source_bytes ->
              match Wasm_word_sequence.decode (Words.words full) source_bytes with
              | None -> failwith "source wire decode"
              | Some source_suffix ->
              ghost_ (Words.recover full source_bytes source_suffix ());
              let capture = {Capture.head_tag = 9; head_payload = 10; tail_tag = 2; tail_payload = 3} in
              let l11 = S.Push (S.I32 8192, S.Push (S.I32 99, S.Empty)) in
              let l10 = S.Push (S.I64 (Header.number 999), l11) in
              let l9 = S.Push (S.I64 (Header.number 999), l10) in
              let l8 = S.Push (S.I32 0, l9) in
              let l7 = S.Push (S.I32 0, l8) in
              let l6 = S.Push (S.I32 0, l7) in
              let l5 = S.Push (S.I32 0, l6) in
              let l4 = S.Push (S.I32 0, l5) in
              let l3 = S.Push (S.I64 (Header.number 999), l4) in
              let l2 = S.Push (S.I64 (Header.number 999), l3) in
              let rest = S.Push (S.I32 256, l2) in
              let locals = S.Push (S.I32 0, rest) in
              let state = {X.memory; machine = {E.locals; stack = S.Empty}} in
              ghost_ (Wasm_locals.get_def locals 5; Wasm_locals.get_def rest 4; Wasm_locals.get_def l2 3;
                Wasm_locals.get_def l3 2; Wasm_locals.get_def l4 1; Wasm_locals.get_def l5 0;
                Wasm_locals.get_def locals 4; Wasm_locals.get_def rest 3; Wasm_locals.get_def l2 2;
                Wasm_locals.get_def l3 1; Wasm_locals.get_def l4 0;
                Wasm_locals.get_def locals 0; Wasm_locals.get_def locals 1; Wasm_locals.get_def rest 0;
                Wasm_locals.get_def locals 2; Wasm_locals.get_def locals 3; Wasm_locals.get_def rest 1; Wasm_locals.get_def rest 2;
                Wasm_locals.get_def l2 0; Wasm_locals.get_def l2 1; Wasm_locals.get_def l3 0;
                Hmc_wasm_call_header_layout.width_def fragment.Copy.recursive;
                Copy.matches_def entry lowered.Lower.capacity fragment; Copy.position_def entry.Hmc_closure_ir.recursive;
                Hmc_wasm_relayout_geometry.size_represents (Heap.length captures) count ();
                Words.size prefix_cells prefix_count offset ());
              let _read = Hmc_wasm_closure_read.correct table heap memory address id captures count () in
              if not (above heap stop) then failwith "callee frame overlaps heap" else
              let slots = {Slots.start = 6; captures = 7; recursive = 8} in
              if not (Slots.distinct slots 4 && Slots.writable slots locals && Separate.separate slots 4 5
                && Separate.separate slots 4 0 && Separate.separate slots 4 1 && Separate.separate slots 4 2 && Separate.separate slots 4 3)
              then failwith "dispatch locals" else
              if not (Capture.distinct capture && Capture.separate capture 1 && Capture.separate capture 0 && Capture.separate capture 5
                && Capture.writable capture locals && Entry.separate capture 0 5 4 && Entry.separate capture 0 5 6
                && Entry.separate capture 0 5 7 && Entry.separate capture 0 5 8) then failwith "capture locals" else
              let _ = ghost_ (Wasm_locals.get_def locals 11; Wasm_locals.get_def rest 10; Wasm_locals.get_def l2 9; Wasm_locals.get_def l3 8; Wasm_locals.get_def l4 7; Wasm_locals.get_def l5 6; Wasm_locals.get_def l6 5; Wasm_locals.get_def l7 4; Wasm_locals.get_def l8 3; Wasm_locals.get_def l9 2; Wasm_locals.get_def l10 1; Wasm_locals.get_def l11 0;
                Hmc_memory_stack.related_def program.Program.origin.Hmc_cfg_program.blocks lowered.Lower.width memory 8192 8192 State.Halt) in
              if not (Entry.separate capture 0 5 11 && Separate.separate slots 4 11) then failwith "stack pointer scratch overlap" else
              (match Program.lookup program.Program.code activation.Frame.pc with
              | Some Program.Tail_call ->
              if Hmc_wasm_program_table.lookup lowered.Lower.blocks old_pc <> Some (Block.Tail_call env_count) then failwith "tail fragment correspondence" else
              let layout = {Emit.structured = {Structured.frame = 1; heap = 0; limit = 0; object_ = 0; scratch = capture};
                top = 11; stack_limit = 11; code = 5; address = 4; descriptor = slots; status = 12; result_tag = 2; result_payload = 3} in
              if not (Wasm_locals.can_set locals 12 (S.I32 0)) then failwith "tail status local" else
              let wasm_globals = {WG.values = S.Push (S.I32 99, S.Push (S.I32 8192, locals)); permissions = WG.Global (true, WG.Global (true, Check.readonly locals))} in
              let config = {Assembly.locals = layout; local_types = Func.Local32 (Func.Local32 (Func.Local64 (Func.Local64 (Func.Local32 (Func.Local32 (Func.Local32 (Func.Local32 (Func.Local32 (Func.Local64 (Func.Local64 (Func.Local32 (Func.Local32 (Func.No_locals))))))))))))); loads = R.Binding (2, 0, R.Binding (3, 1, R.Binding (4, 2, R.Binding (5, 3, R.Binding (6, 4, R.Binding (7, 5, R.Binding (8, 6, R.Binding (9, 7, R.Binding (10, 8, R.Binding (11, 9, R.Binding (12, 10, R.Binding (13, 11, R.Binding (14, 12, R.End)))))))))))));
                stores = R.Binding (0, 12, R.Binding (1, 11, R.End)); table_base = 0; stack_base = 8192} in
              let initial = {GE.globals = wasm_globals; execution = {X.memory; machine = {E.locals = Func.zero_locals config.Assembly.local_types; stack = S.Empty}}} in
              (match R.load config.Assembly.loads initial with
              | None -> failwith "tail import"
              | Some imported ->
              if not (Check.locals_equal imported.GE.execution.X.machine.E.locals locals) then failwith "tail imported locals" else
              let _ = ghost_ (Hmc_wasm_program_status.zero_def ();
                New.separate_def capture 0 5 12; Capture.separate_def capture 12; Separate.separate_def slots 4 12;
                Entry.separate_def capture 0 5 11; Entry.separate_def capture 0 5 4; Entry.separate_def capture 0 5 6; Entry.separate_def capture 0 5 7; Entry.separate_def capture 0 5 8;
                New.separate_def capture 0 5 11; New.separate_def capture 0 5 4;
                New.separate_def capture 0 5 6; New.separate_def capture 0 5 7; New.separate_def capture 0 5 8) in
              let completed = New.correct lowered layout State.Halt globals D.Z (Round.labels config) 8192 8192 11 program heap entry function_ id address captures signature activation context ty schema source_cells source_padding old_pc env_count capture source_bytes source_suffix count plans 5 lowered.Lower.capacity runtime 0 table_count 4 slots state 256 stop 8192 0 1 () in
              let initialized = completed.New.source in
              if Wasm_locals.get initialized.Entry.entry.Dispatch.locals 11 <> Some (S.I32 8192) then failwith "callee changed saved-stack pointer";
              let result = initialized.Entry.entry.Dispatch.call.Plan_entry.entry.Dynamic.frame in
              if Machine.invoke program heap (V.Closure_pointer address) argument <> Some initialized.Entry.entry.Dispatch.call.Plan_entry.entry.Dynamic.entered then failwith "callee source invocation";

              ghost_ (Hmc_frame_call_decode.correct entry function_.Hmc_cfg_program.start pc address argument captures ());
              let callee_cells = Hmc_frame_call_decode.cells entry address argument captures in
              if Codec.decode (Hmc_frame_call_entry.signature entry) function_.Hmc_cfg_program.start callee_cells <>
                Some (Hmc_frame_call_entry.activation entry function_.Hmc_cfg_program.start (V.Closure_pointer address) argument captures, Heap.Empty)
              then failwith "callee frame decoding";
              (match L.drop memory 256, L.drop memory stop with
              | Some before, Some suffix ->
                let cells = Hmc_frame_segments.append (Hmc_wasm_call_header_layout.cells entry.Hmc_closure_ir.recursive pc address argument) captures in
                let captured = Wire.encode_cells cells suffix in
                let expected = Wasm_memory_splice.replace memory 256 before captured () in
                if expected <> result.Memory.memory then failwith "capture copy independent memory";
                let target = Assembly.function_ lowered (Block.Tail_call env_count) config in
                let module_ = {Func.functions = Func.Function (target, Func.No_functions); signatures = Func.Signature (Func.Void, Func.No_signatures); table = Func.Element (Some 0, Func.No_elements)} in
                let call = {Calls.current = {P.globals = wasm_globals; body = {T.code = T.Instruction (I.Call_indirect 0, T.Empty); labels = T.No_labels;
                  state = {X.memory; machine = {E.locals = S.Empty; stack = S.Push (S.I32 0, S.Empty)}}}};
                  result = Func.Void; callers = Calls.Root; capacity = C.Succ C.Zero} in
                (match R.store config.Assembly.stores {GE.globals = wasm_globals; execution = completed.New.state} with
                | None -> failwith "tail export"
                | Some exported ->
                  ghost_ (Func.signature_def module_.Func.signatures 0; Func.element_def module_.Func.table 0; Func.lookup_def module_.Func.functions 0;
                    Round.correct lowered (Block.Tail_call env_count) config module_ call 0 0 0 T.Empty S.Empty C.Zero state completed.New.state exported completed.New.fuel ());
                  (match Calls.run (Round.cost config completed.New.fuel) module_ call with
                  | Calls.Running actual ->
                    if actual.Calls.current.P.body.T.state.X.memory <> expected || actual.Calls.callers <> Calls.Root || actual.Calls.capacity <> call.Calls.capacity
                      || WG.get actual.Calls.current.P.globals 0 <> Some (S.I32 0) || WG.get actual.Calls.current.P.globals 1 <> Some (S.I32 8192) then failwith "tail indirect result"
                  | _ -> failwith "tail indirect execution");
                  (match Hmc_wasm_program_table.lookup lowered.Lower.blocks old_pc, Hmc_u32_index.encode 1000 (G.size program.Program.origin.Hmc_cfg_program.blocks) with
                  | Some (Block.Tail_call selected_count), Some block_count ->
                    if selected_count <> env_count || old_pc >= block_count then failwith "tail dispatcher selection" else
                    let used : Hmc_wasm_relayout.count = (if entry.Hmc_closure_ir.recursive then 4 else 3) + count in
                    if source_count <> lowered.Lower.capacity || used > lowered.Lower.capacity || source_stop > 1024
                      || Heap.used heap < 1024 || not (region_above heap source_stop && region_above heap 1024) then failwith "tail shared regions" else
                    let canonical = Hmc_wasm_program_runtime.config 0 1024 in
                    let registers = {Hmc_wasm_program_registers.frame = 256; heap = Heap.used heap; heap_limit = 8192;
                      top = 1024; stack_limit = 1024; status = 99; tag = Header.number 123; payload = Header.number 456} in
                    let module_ = Assembly.assemble lowered (G.size program.Program.origin.Hmc_cfg_program.blocks) block_count canonical (Hmc_wasm_program_runtime.dispatcher ()) in
                    ghost_ (Assembly.source_order globals program.Program.origin.Hmc_cfg_program.blocks program.Program.code lowered.Lower.blocks lowered.Lower.capacity 1000 block_count ();
                      Assembly.dispatch_target lowered (G.size program.Program.origin.Hmc_cfg_program.blocks) block_count canonical (Hmc_wasm_program_runtime.dispatcher ()) old_pc (Block.Tail_call env_count) ();
                      Assembly.assemble_def lowered (G.size program.Program.origin.Hmc_cfg_program.blocks) block_count canonical (Hmc_wasm_program_runtime.dispatcher ());
                      Hmc_wasm_program_dispatch.void_signature_def (); Func.signature_def module_.Func.signatures 0);
                    ghost_ (let _ = Bounds.suffix memory 8192 1024 () in
                      Bounds.covers_def memory 1024;
                      Hmc_memory_stack.related_def program.Program.origin.Hmc_cfg_program.blocks lowered.Lower.width memory 1024 1024 State.Halt;
                      Hmc_wasm_program_resources.valid_def program globals lowered.Lower.width 1024 source_stop abstract heap activation State.Halt registers memory;
                      Hmc_wasm_program_descriptors.valid_def program registers memory runtime 0 table_count;
                      Hmc_wasm_program_frame.valid_def signature activation registers memory source_stop old_pc source_cells source_padding source_bytes source_suffix (source_count + 1));
                    let padded = Hmc_wasm_program_source_tail.framed lowered module_ State.Halt globals D.Z 1024 program heap entry function_ id address captures
                      signature activation context ty schema source_cells source_padding old_pc env_count source_bytes source_suffix count runtime 0 table_count
                      registers memory stop C.Zero (Assembly.reverse_index block_count old_pc) used source_stop (source_count + 1) abstract () in
                    let step = padded.Hmc_wasm_program_source_tail.step in
                    if Heap.length padded.Hmc_wasm_program_source_tail.frame.Hmc_wasm_frame_extend.cells <> Heap.length source_cells
                      || Codec.decode (Hmc_frame_call_entry.signature entry) step.Hmc_wasm_program_source_tail.callee.Dynamic.entered.Frame.pc
                        padded.Hmc_wasm_program_source_tail.frame.Hmc_wasm_frame_extend.cells <>
                        Some (step.Hmc_wasm_program_source_tail.callee.Dynamic.entered, padded.Hmc_wasm_program_source_tail.frame.Hmc_wasm_frame_extend.padding)
                    then failwith "tail full-capacity decoding";
                    let expected_registers = {registers with Hmc_wasm_program_registers.status = 0} in
                    if step.Hmc_wasm_program_source_tail.registers <> expected_registers
                      || step.Hmc_wasm_program_source_tail.callee.Dynamic.frame.Memory.memory <> expected then failwith "tail dispatcher source result";
                    let start = Hmc_wasm_program_dispatch.loop (Hmc_wasm_program_registers.globals registers) memory (C.Succ C.Zero) in
                    let final = Hmc_wasm_program_dispatch.loop (Hmc_wasm_program_registers.globals expected_registers) expected (C.Succ C.Zero) in
                    if Calls.run step.Hmc_wasm_program_source_tail.fuel module_ start <> Calls.Running final then failwith "tail dispatcher execution"
                  | _ -> failwith "tail dispatcher block");
                  {memory; expected; block = target.Func.code; globals = wasm_globals; cursor = 8192})
              | _ -> failwith "capture copy bounds"))
              | _ -> failwith "source tail call expected"))))))))

          | _ -> failwith "call index encoding")
      | _ -> failwith "source encoding")))
    | _ -> failwith "call source state"
let rec collect : (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (configuration : Machine.configuration) @ immutable -> (abstract : Hmc_cfg_semantics.state) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (elapsed : D.index) @ immutable ->
    int -> {u : unit | Inv.valid program globals (bound ()) configuration abstract
      && abstract === Abstract.advance program elapsed (Abstract.initial program input)} -> fixture list =
  fun program globals configuration abstract input elapsed fuel premise ->
    ghost_ (bound_def ());
    match configuration.Machine.state with
    | State.Done _ -> []
    | State.Stuck -> failwith "reached source stuck"
    | State.Running (activation, _) ->
      if fuel = 0 then failwith "reached source fuel" else
      let fixtures = match Program.lookup program.Program.code activation.Frame.pc with
        | Some Program.Tail_call ->
          [fixture program globals configuration abstract input elapsed ()]
        | _ -> [] in
      ghost_ (Hmc_heap_reachable_operands.advance_next program input elapsed; Inv.step program globals 8192 (D.S (D.S D.Z)) configuration abstract ());
      match Machine.step program globals 8192 (D.S (D.S D.Z)) configuration with
      | Machine.Exhausted _ -> failwith "reached source exhausted"
      | Machine.Advanced next -> fixtures @ collect program globals next (Abstract.step program abstract) input (D.S elapsed) (fuel - 1) ()
let cases source (initial_base : B.u32) =
  if initial_base > 1031 then failwith "call heap base" else
  let program = Hmc_wasm_global_fixture.build source in
  let input = Header.number 4 in
  match Hmc_heap_initialize.initialize program initial_base 8192 input () with
  | Hmc_heap_initialize.Heap_exhausted _ -> failwith "reached initialization exhausted"
  | Hmc_heap_initialize.Initialized start ->
    ghost_ (bound_def (); Hmc_heap_initialize.correct_def program initial_base 8192 input (Hmc_heap_initialize.Initialized start));
    ghost_ (Abstract.advance_def program D.Z (Abstract.initial program input));
    collect program start.Hmc_heap_initialize.globals start.Hmc_heap_initialize.configuration (Abstract.initial program input) input D.Z 200 ()
let fixtures () =
  let word n = D.Word (Header.number n) in
  let countdown = D.Recursive (D.If (D.Primitive (D.Equal_word, D.Bound D.Z, word 0), word 17,
    D.Apply (D.Bound (D.S D.Z), D.Primitive (D.Subtract, D.Bound D.Z, word 1)))) in
  let fixtures = cases countdown 1024 @ cases countdown 1031 in
  if List.length fixtures <> 8 then failwith "tail countdown coverage";
  fixtures
