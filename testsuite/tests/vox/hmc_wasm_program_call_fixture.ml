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
module Image = Hmc_heap_image
module Above = Hmc_heap_image_suffix
module L = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
module Call = Hmc_wasm_ordinary_call
module Save = Hmc_wasm_call_save
module Pad = Hmc_wasm_frame_padding
module Index = Hmc_u32_index
module Cap = Hmc_frame_capacity
module Stack = Hmc_memory_stack
module Capacity = Hmc_memory_stack_capacity
let[@def] (difference @ total) (n : B.u32) (used : B.u32) : B.u32 = if used <= n then n - used else 0
let rec (padding_index @ total) : (capacity : D.index) @ immutable -> (used : B.u32) -> (n : B.u32) ->
    {u : unit | Index.represents capacity n && used <= n} ->
    {out : D.index | Index.represents out (difference n used)} @ immutable = fun capacity used n premise ->
  ghost_ (Index.represents_def capacity n; difference_def n used);
  match capacity with
  | D.Z -> ghost_ (Index.represents_def D.Z (n - used)); D.Z
  | D.S rest -> if used = 0 then capacity else (ghost_ (difference_def (n - 1) (used - 1)); padding_index rest (used - 1) (n - 1) ())
module New = Hmc_wasm_program_call
module Lower = Hmc_wasm_program_lower
module Emit = Hmc_wasm_program_emit
module Block = Hmc_wasm_program_block
module Structured = Hmc_wasm_structured_block
module R = Wasm_global_registers
module GE = Wasm_global_execution
module WG = Wasm_globals
module Func = Wasm_functions
module Assembly = Hmc_wasm_program_functions
module Calls = Wasm_calls
module P = Wasm_instance_control
module Round = Hmc_wasm_program_roundtrip
let (value_equal @ total) : (a : S.value) @ immutable -> (b : S.value) @ immutable -> {out : bool | out === (a === b)} = fun a b ->
  match a, b with
  | S.I32 x, S.I32 y -> x = y
  | S.I64 x, S.I64 y -> x.Hmc_word64.lo = y.Hmc_word64.lo && x.Hmc_word64.hi = y.Hmc_word64.hi
  | _ -> false
let rec (locals_equal @ total) : (a : S.stack) @ immutable -> (b : S.stack) @ immutable -> {out : bool | out === (a === b)} = fun a b ->
  match a, b with
  | S.Empty, S.Empty -> true
  | S.Push (x, xs), S.Push (y, ys) -> value_equal x y && locals_equal xs ys
  | _ -> false
let rec readonly = function S.Empty -> WG.Empty | S.Push (_, rest) -> WG.Global (false, readonly rest)
type fixture = {memory : B.bytes; expected : B.bytes; block : T.code; globals : WG.t; cursor : B.u32; status : int}
let rec (above @ total) : (heap : Heap.heap) @ immutable -> (boundary : B.u32) ->
    {out : bool | out === Above.above heap boundary} = fun heap boundary ->
  ghost_ (Above.above_def heap boundary);
  match heap with Heap.Empty_heap _ -> true | Heap.Allocate (a, rest) -> boundary <= a.Heap.address && above rest boundary
let rec fuel n = if n = 0 then C.Zero else C.Succ (fuel (n - 1))
let rec filler n = if n = 0 then B.End else B.Byte (173, filler (n - 1))
let rec (region_above @ total) : (heap : Heap.heap) @ immutable -> (boundary : B.u32) ->
    {out : bool | out === Hmc_wasm_heap_suffix.above heap boundary} = fun heap boundary ->
  ghost_ (Hmc_wasm_heap_suffix.above_def heap boundary);
  match heap with Heap.Empty_heap base -> boundary <= base | Heap.Allocate (a, rest) -> boundary <= a.Heap.address && region_above rest boundary
let[@def] (bound @ total) (u : unit) : B.u32 = 8192
module Code = Hmc_wasm_closure_code
module Copy = Hmc_wasm_call_captures
module Memory = Hmc_wasm_dynamic_call_frame
module Words = Hmc_wire_word_sequence
let fixture : bool -> (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (configuration : Machine.configuration) @ immutable -> (abstract : Hmc_cfg_semantics.state) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (elapsed : D.index) @ immutable ->
    {u : unit | Inv.valid program globals (bound ()) configuration abstract
      && abstract === Abstract.advance program elapsed (Abstract.initial program input)} -> fixture =
  fun available program globals configuration abstract input elapsed premise ->
    ghost_ (bound_def (); Inv.valid_def program globals 8192 configuration abstract);
    match Lower.lower program globals 1000 with
  | None -> failwith "ordinary program lowering"
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
      let _ = ghost_ (Cap.lookup program.Program.origin.Hmc_cfg_program.blocks activation.Frame.pc block (); Hmc_pointer_frame_shape.size signature) in
      let source_padding = if Heap.used heap > 4096 then Pad.cells (Cap.remaining (Cap.capacity program.Program.origin.Hmc_cfg_program.blocks) (Codec.size signature) ()) else Heap.Empty in
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
              let blocks = program.Program.origin.Hmc_cfg_program.blocks in
              (match Program.lookup program.Program.code activation.Frame.pc with
              | Some (Program.Keep (G.Call next)) ->
              (match G.lookup blocks next, Index.encode 1000 (Cap.capacity blocks),
                Index.encode 1000 (D.add (Codec.locals_size context) (Codec.temporaries_size schema)),
                Save.build signature next lowered.Lower.capacity 1000 with
              | Some target_block, Some stored_capacity, Some saved_count, Some save_fragment ->
              if stored_capacity < 2 + saved_count then failwith "saved frame capacity" else
              let padding_count : Hmc_wasm_relayout.count = stored_capacity - 2 - saved_count in
              let padding_length = padding_index (Cap.capacity blocks) (2 + saved_count) stored_capacity () in
              let _ = ghost_ (difference_def stored_capacity (2 + saved_count)) in
              let padding = Pad.build (3 + saved_count) padding_count padding_length () in
              let width : B.u32 = 16 * (1 + stored_capacity) in
              if width > 4096 || stop > 4096 || source_stop > 4096 then failwith "call regions" else
              let stack_limit : B.u32 = if available then 4096 + width else 4096 in
              if Heap.used heap > 4096 && not (above heap stack_limit) then failwith "call stack overlaps heap" else
              let stack_capacity = if available then D.S D.Z else D.Z in
              let l12 = S.Push (S.I32 stack_limit, S.Push (S.I32 99, S.Empty)) in
              let l11 = S.Push (S.I32 4096, l12) in
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
              let _ = ghost_ (
                Wasm_locals.get_def locals 11; Wasm_locals.get_def rest 10; Wasm_locals.get_def l2 9; Wasm_locals.get_def l3 8; Wasm_locals.get_def l4 7; Wasm_locals.get_def l5 6; Wasm_locals.get_def l6 5; Wasm_locals.get_def l7 4; Wasm_locals.get_def l8 3; Wasm_locals.get_def l9 2; Wasm_locals.get_def l10 1; Wasm_locals.get_def l11 0;
                Wasm_locals.get_def locals 12; Wasm_locals.get_def rest 11; Wasm_locals.get_def l2 10; Wasm_locals.get_def l3 9; Wasm_locals.get_def l4 8; Wasm_locals.get_def l5 7; Wasm_locals.get_def l6 6; Wasm_locals.get_def l7 5; Wasm_locals.get_def l8 4; Wasm_locals.get_def l9 3; Wasm_locals.get_def l10 2; Wasm_locals.get_def l11 1; Wasm_locals.get_def l12 0;
                Stack.related_def blocks width memory 4096 4096 State.Halt;
                Capacity.region_def width stack_capacity 4096 stack_limit; Stack.previous_def width stack_limit;
                Capacity.region_def width D.Z 4096 4096; Stack.zero_def ();
                Hmc_memory_saved_frame.slots_def blocks; Index.represents_def (D.S (Cap.capacity blocks)) (1 + stored_capacity);
                Program.valid_def program; Program.lookup_related blocks program.Program.code program.Program.sites activation.Frame.pc ();
                Program.select_def program.Program.sites activation.Frame.pc block.G.instruction;
                Hmc_cfg_program.valid_def program.Program.origin;
                Hmc_cfg_extension.lookup_valid (Hmc_monomorphic.manifest program.Program.origin.Hmc_cfg_program.origin.Hmc_closure_program.origin.Hmc_monomorphic.definitions)
                  table blocks activation.Frame.pc block ();
                G.block_valid_def (Hmc_monomorphic.manifest program.Program.origin.Hmc_cfg_program.origin.Hmc_closure_program.origin.Hmc_monomorphic.definitions)
                  table blocks block;
                (match ty with D.Function (_, result) -> G.accepts_def blocks next context schema (Some result) | _ -> ())) in
              if not (Entry.separate capture 0 5 11 && Separate.separate slots 4 11) then failwith "stack pointer scratch overlap" else
              (match Hmc_wasm_reservation.reserve (Hmc_memory_saved_frame.slots blocks) (1 + stored_capacity) 0 width () with
              | None -> failwith "stack frame extent"
              | Some _ ->
              let call_fragment = Block.Call {Block.save = save_fragment; padding; padding_length; saved = saved_count; environment = env_count} in
              if stored_capacity <> lowered.Lower.capacity || width <> lowered.Lower.width || Hmc_wasm_program_table.lookup lowered.Lower.blocks old_pc <> Some call_fragment
                then failwith "ordinary fragment differs from program table" else
              let layout = {Emit.structured = {Structured.frame = 1; heap = 0; limit = 0; object_ = 0; scratch = capture};
                top = 11; stack_limit = 12; code = 5; address = 4; descriptor = slots; status = 13; result_tag = 2; result_payload = 3} in
              if not (Wasm_locals.can_set locals 13 (S.I32 3)) then failwith "ordinary status layout" else
              let _ = ghost_ (New.failure_def (); Index.unique (Cap.capacity blocks) stored_capacity lowered.Lower.capacity ()) in
              let wasm_globals = {WG.values = S.Push (S.I32 99, S.Push (S.I32 4096, locals));
                permissions = WG.Global (true, WG.Global (true, readonly locals))} in
              let config = {Assembly.locals = layout; local_types = Func.Local32 (Func.Local32 (Func.Local64 (Func.Local64 (Func.Local32 (Func.Local32 (Func.Local32 (Func.Local32 (Func.Local32 (Func.Local64 (Func.Local64 (Func.Local32 (Func.Local32 (Func.Local32 (Func.No_locals))))))))))))));
                loads = R.Binding (2, 0, R.Binding (3, 1, R.Binding (4, 2, R.Binding (5, 3, R.Binding (6, 4, R.Binding (7, 5, R.Binding (8, 6, R.Binding (9, 7, R.Binding (10, 8, R.Binding (11, 9, R.Binding (12, 10, R.Binding (13, 11, R.Binding (14, 12, R.Binding (15, 13, R.End)))))))))))))); stores = R.Binding (0, 13, R.Binding (1, 11, R.End)); table_base = 0; stack_base = 4096} in
              let initial = {GE.globals = wasm_globals; execution = {X.memory; machine = {E.locals = Func.zero_locals config.Assembly.local_types; stack = S.Empty}}} in
              (match R.load config.Assembly.loads initial with
              | None -> failwith "ordinary import"
              | Some imported ->
              if not (locals_equal imported.GE.execution.X.machine.E.locals locals) then failwith "ordinary imported locals" else
              let result = New.correct lowered layout State.Halt 4096 width stack_capacity stack_limit 12 (Round.labels config) blocks target_block stored_capacity table heap runtime 0 table_count source_count source_stop signature activation source_cells source_padding context ty schema next env_count saved_count old_pc save_fragment lowered.Lower.capacity padding padding_count padding_length state 256 4096 8192 source_bytes source_suffix 1 11 program entry function_ id address captures capture count plans 5 lowered.Lower.capacity 4 slots stop 0 globals () in
              let called = result.New.source in
              let control = Emit.emit lowered call_fragment layout 0 4096 in
              let cursor : B.u32 = if available then 4096 + width else 4096 in
              let expected = match called with
                | New.Stack_exhausted -> if available then failwith "call exhausted with room" else memory
                | New.Called out ->
                  if not available then failwith "call exceeded stack capacity";
                  (match Machine.step program globals 8192 stack_capacity {Machine.heap; state = State.Running (activation, State.Halt)} with
                  | Machine.Advanced {Machine.state = State.Running (callee, State.Frame (saved, State.Halt)); _} ->
                    if not (Codec.shape target_block.G.signature saved && Codec.shape (Hmc_frame_call_entry.signature entry) callee) then failwith "source call shapes" else
                    let saved_cells = Codec.encode target_block.G.signature saved (Pad.cells padding_length) () in
                    let callee_cells = Codec.encode (Hmc_frame_call_entry.signature entry) callee Heap.Empty () in
                    let saved_full = Heap.Cell (V.Word (Header.number save_fragment.Save.pc), saved_cells) in
                    let callee_full = Heap.Cell (V.Word (Header.number pc), callee_cells) in
                    (match L.drop memory 4096, L.drop memory cursor with
                    | Some before, Some suffix ->
                      let saved_memory = Wasm_memory_splice.replace memory 4096 before (Wire.encode_cells saved_full suffix) () in
                      (match L.drop saved_memory 256, L.drop saved_memory stop with
                      | Some before, Some suffix -> Wasm_memory_splice.replace saved_memory 256 before (Wire.encode_cells callee_full suffix) ()
                      | _ -> failwith "callee expected extent")
                    | _ -> failwith "saved expected extent")
                  | _ -> failwith "source ordinary call") in
              (match T.run result.New.fuel {T.code = control; labels = Round.labels config; state} with
              | T.Running returned ->
                let final = returned.T.state in
                if returned.T.code <> T.Empty || returned.T.labels <> Round.labels config || final <> result.New.state || Wasm_locals.get final.X.machine.E.locals 13 <> Some (S.I32 (if available then 0 else 3)) || final.X.memory <> expected || Wasm_locals.get final.X.machine.E.locals 11 <> Some (S.I32 cursor) then failwith "ordinary call execution"
              | _ -> failwith "ordinary call control");
              (match Hmc_wasm_program_table.lookup lowered.Lower.blocks old_pc, Index.encode 1000 (G.size blocks) with
              | Some (Block.Call selected), Some block_count ->
                let selection = Hmc_wasm_program_selection.select_reachable program globals lowered 1000 block_count
                  8192 configuration abstract activation State.Halt old_pc 0 4096 () in
                let _ = ghost_ (Program.valid_def program;
                  Block.corresponds_def globals signature (Program.Keep (G.Call next)) lowered.Lower.capacity 1000 (Block.Call selected);
                  Index.unique (Codec.locals_size signature.G.locals) env_count selected.Block.environment ();
                  Index.unique (D.add (Codec.locals_size context) (Codec.temporaries_size schema)) saved_count selected.Block.saved ();
                  Block.difference_def lowered.Lower.capacity (2 + saved_count)) in
                let canonical = Hmc_wasm_program_runtime.config 0 4096 in
                let canonical_capture = canonical.Assembly.locals.Emit.structured.Structured.scratch in
                let canonical_slots = canonical.Assembly.locals.Emit.descriptor in
                let registers = {Hmc_wasm_program_registers.frame = 256; heap = Heap.used heap; heap_limit = 8192;
                  top = 4096; stack_limit; status = 99; tag = Header.number 123; payload = Header.number 456} in
                let canonical_state = {X.memory; machine = {E.locals = Hmc_wasm_program_registers.locals registers; stack = S.Empty}} in
                let canonical_module = Assembly.assemble lowered (G.size blocks) block_count canonical (Hmc_wasm_program_runtime.dispatcher ()) in
                ghost_ (Hmc_wasm_program_runtime.config_def 0 4096;
                  Hmc_wasm_program_registers.local_values registers;
                  Hmc_wasm_program_registers.matches_def (Hmc_wasm_program_registers.locals registers) registers;
                  Wasm_locals.can_set_def (Hmc_wasm_program_registers.locals registers) 11 (S.I32 3); S.same_type_def (S.I32 99) (S.I32 3);
                  Capture.distinct_def canonical_capture; Capture.writable_def canonical_capture (Hmc_wasm_program_registers.locals registers);
                  Capture.word_slot_def (Hmc_wasm_program_registers.locals registers) 14; Capture.word_slot_def (Hmc_wasm_program_registers.locals registers) 15;
                  Capture.word_slot_def (Hmc_wasm_program_registers.locals registers) 16; Capture.word_slot_def (Hmc_wasm_program_registers.locals registers) 17;
                  Slots.distinct_def canonical_slots 7; Slots.writable_def canonical_slots (Hmc_wasm_program_registers.locals registers);
                  Slots.limb_slot_def (Hmc_wasm_program_registers.locals registers) 8; Slots.limb_slot_def (Hmc_wasm_program_registers.locals registers) 9;
                  Slots.limb_slot_def (Hmc_wasm_program_registers.locals registers) 10;
                  Capture.separate_def canonical_capture 0; Hmc_wasm_loaded_call.separate_def canonical_capture 3 6 0;
                  Capture.separate_def canonical_capture 3; Hmc_wasm_loaded_call.separate_def canonical_capture 3 6 3;
                  Capture.separate_def canonical_capture 6; Hmc_wasm_loaded_call.separate_def canonical_capture 3 6 6;
                  Capture.separate_def canonical_capture 4; Hmc_wasm_loaded_call.separate_def canonical_capture 3 6 4;
                  Capture.separate_def canonical_capture 7; Hmc_wasm_loaded_call.separate_def canonical_capture 3 6 7;
                  Capture.separate_def canonical_capture 8; Hmc_wasm_loaded_call.separate_def canonical_capture 3 6 8;
                  Capture.separate_def canonical_capture 9; Hmc_wasm_loaded_call.separate_def canonical_capture 3 6 9;
                  Capture.separate_def canonical_capture 10; Hmc_wasm_loaded_call.separate_def canonical_capture 3 6 10;
                  Hmc_wasm_selected_call_entry.separate_def canonical_slots 7 4;
                  Hmc_wasm_selected_call_entry.separate_def canonical_slots 7 6;
                  Hmc_wasm_selected_call_entry.separate_def canonical_slots 7 3;
                  Hmc_wasm_selected_call_entry.separate_def canonical_slots 7 0;
                  Hmc_wasm_selected_call_entry.separate_def canonical_slots 7 16;
                  Hmc_wasm_selected_call_entry.separate_def canonical_slots 7 17);
                let step = if Heap.used heap > 4096 then (
                  let used : Hmc_wasm_relayout.count = (if entry.Hmc_closure_ir.recursive then 4 else 3) + count in
                  if source_count <> lowered.Lower.capacity || used > source_count || not (region_above heap source_stop && region_above heap stack_limit)
                    || stack_limit > Heap.used heap then failwith "call shared regions" else
                  let _ = ghost_ (let _ = Bounds.suffix memory 8192 stack_limit () in
                    Bounds.covers_def memory stack_limit;
                    Hmc_wasm_program_resources.valid_def program globals width 4096 source_stop abstract heap activation State.Halt registers memory;
                    Hmc_wasm_program_descriptors.valid_def program registers memory runtime 0 table_count) in
                  let framed = Hmc_wasm_program_source_call.framed abstract (source_count + 1) used canonical_module registers C.Zero selection.Hmc_wasm_program_selection.selected.Hmc_wasm_program_selection.index
                  lowered canonical.Assembly.locals State.Halt 4096 width stack_capacity stack_limit 5 (Round.labels canonical) blocks target_block stored_capacity table heap runtime 0 table_count
                  source_count source_stop signature activation source_cells source_padding context ty schema next env_count saved_count old_pc selected.Block.save lowered.Lower.capacity
                  selected.Block.padding padding_count selected.Block.padding_length canonical_state 256 4096 8192 source_bytes source_suffix 0 4 program entry function_ id address captures
                  canonical_capture count plans 6 lowered.Lower.capacity 7 canonical_slots stop 3 globals () in
                  if Heap.length framed.Hmc_wasm_program_source_call.cells <> Heap.length source_cells then failwith "call full frame capacity";
                  framed.Hmc_wasm_program_source_call.step) else
                  Hmc_wasm_program_source_call.correct canonical_module registers C.Zero selection.Hmc_wasm_program_selection.selected.Hmc_wasm_program_selection.index
                  lowered canonical.Assembly.locals State.Halt 4096 width stack_capacity stack_limit 5 (Round.labels canonical) blocks target_block stored_capacity table heap runtime 0 table_count
                  source_count source_stop signature activation source_cells source_padding context ty schema next env_count saved_count old_pc selected.Block.save lowered.Lower.capacity
                  selected.Block.padding padding_count selected.Block.padding_length canonical_state 256 4096 8192 source_bytes source_suffix 0 4 program entry function_ id address captures
                  canonical_capture count plans 6 lowered.Lower.capacity 7 canonical_slots stop 3 globals () in
                if step.Hmc_wasm_program_source_call.registers <> {registers with Hmc_wasm_program_registers.top = cursor; status = (if available then 0 else 3)}
                  || step.Hmc_wasm_program_source_call.body.New.state.X.memory <> expected then failwith "call dispatcher result";
                (match Calls.run step.Hmc_wasm_program_source_call.fuel canonical_module
                  (Hmc_wasm_program_dispatch.loop (Hmc_wasm_program_registers.globals registers) memory (C.Succ C.Zero)) with
                | Calls.Running actual ->
                  if not available || actual <> Hmc_wasm_program_dispatch.loop (Hmc_wasm_program_registers.globals step.Hmc_wasm_program_source_call.registers) expected (C.Succ C.Zero)
                  then failwith "call dispatcher successor"
                | Calls.Finished actual ->
                  if available || actual.GE.globals <> Hmc_wasm_program_registers.globals step.Hmc_wasm_program_source_call.registers || actual.GE.execution.X.memory <> memory
                    || actual.GE.execution.X.machine <> {E.locals = S.Empty; stack = S.Push (S.I32 3, S.Empty)} then failwith "call dispatcher exhaustion"
                | _ -> failwith "call dispatcher control")
              | _ -> failwith "call dispatcher fragment");
              let target = Assembly.function_ lowered call_fragment config in
              let module_ = {Func.functions = Func.Function (target, Func.No_functions); signatures = Func.Signature (Func.Void, Func.No_signatures); table = Func.Element (Some 0, Func.No_elements)} in
              let call = {Calls.current = {P.globals = wasm_globals; body = {T.code = T.Instruction (I.Call_indirect 0, T.Empty); labels = T.No_labels;
                state = {X.memory; machine = {E.locals = S.Empty; stack = S.Push (S.I32 0, S.Empty)}}}};
                result = Func.Void; callers = Calls.Root; capacity = C.Succ C.Zero} in
              (match R.store config.Assembly.stores {GE.globals = wasm_globals; execution = result.New.state} with
              | None -> failwith "ordinary export"
              | Some exported ->
                ghost_ (Func.signature_def module_.Func.signatures 0; Func.element_def module_.Func.table 0; Func.lookup_def module_.Func.functions 0;
                  Round.correct lowered call_fragment config module_ call 0 0 0 T.Empty S.Empty C.Zero state result.New.state exported result.New.fuel ());
                (match Calls.run (Round.cost config result.New.fuel) module_ call with
                | Calls.Running actual ->
                  if actual.Calls.current.P.body.T.state.X.memory <> expected || actual.Calls.callers <> Calls.Root || actual.Calls.capacity <> call.Calls.capacity
                    || WG.get actual.Calls.current.P.globals 0 <> Some (S.I32 (if available then 0 else 3))
                    || WG.get actual.Calls.current.P.globals 1 <> Some (S.I32 cursor) then failwith "ordinary indirect result"
                | _ -> failwith "ordinary indirect execution");
                {memory; expected; block = target.Func.code; globals = wasm_globals; cursor; status = (if available then 0 else 3)})))
              | _ -> failwith "call save plan")
              | _ -> failwith "ordinary call expected"))))))))

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
        | Some (Program.Keep (G.Call _)) ->
          [fixture true program globals configuration abstract input elapsed (); fixture false program globals configuration abstract input elapsed ()]
        | _ -> [] in
      ghost_ (Hmc_heap_reachable_operands.advance_next program input elapsed; Inv.step program globals 8192 (D.S (D.S D.Z)) configuration abstract ());
      match Machine.step program globals 8192 (D.S (D.S D.Z)) configuration with
      | Machine.Exhausted _ -> failwith "reached source exhausted"
      | Machine.Advanced next -> fixtures @ collect program globals next (Abstract.step program abstract) input (D.S elapsed) (fuel - 1) ()
let cases source (initial_base : B.u32) =
  if initial_base > 6151 then failwith "call heap base" else
  let program = Hmc_wasm_global_fixture.build source in
  let input = Header.number 42 in
  match Hmc_heap_initialize.initialize program initial_base 8192 input () with
  | Hmc_heap_initialize.Heap_exhausted _ -> failwith "reached initialization exhausted"
  | Hmc_heap_initialize.Initialized start ->
    ghost_ (bound_def (); Hmc_heap_initialize.correct_def program initial_base 8192 input (Hmc_heap_initialize.Initialized start));
    ghost_ (Abstract.advance_def program D.Z (Abstract.initial program input));
    collect program start.Hmc_heap_initialize.globals start.Hmc_heap_initialize.configuration (Abstract.initial program input) input D.Z 200 ()
let fixtures () =
  let captured = D.Lambda (D.Let (D.Apply (D.Lambda (D.Bound (D.S D.Z)), D.Word (Header.number 7)), D.Bound D.Z)) in
  let recursive = D.Lambda (D.Let (D.Apply (D.Recursive (D.Bound D.Z), D.Word (Header.number 7)), D.Bound D.Z)) in
  let fixtures = cases captured 1024 @ cases captured 1031 @ cases recursive 1024 @ cases recursive 1031 in
  let heap_above_stack = cases captured 6144 @ cases captured 6151 @ cases recursive 6144 @ cases recursive 6151 in
  if List.length heap_above_stack <> 8 then failwith "call heap-above-stack coverage";
  if List.length fixtures <> 8 then failwith "call capture coverage";
  fixtures
