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
module Capture = Hmc_wasm_list_capture
module Full = Hmc_wasm_list_full
module Source = Hmc_wasm_list_conditional
module Finished = Hmc_wasm_list_finish_invariant
module Entry = Hmc_wasm_cons_entry
module Guarded = Hmc_wasm_cons_guarded
module Block = Hmc_wasm_structured_block
module Image = Hmc_heap_image
module Above = Hmc_heap_image_suffix
module L = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
module New = Hmc_wasm_program_list
module Emit = Hmc_wasm_program_emit
module Lower = Hmc_wasm_program_lower
module Capacity = Hmc_frame_capacity
module Pad = Hmc_wasm_frame_padding
module Assembly = Hmc_wasm_program_functions
module Round = Hmc_wasm_program_roundtrip
module Registers = Wasm_global_registers
module WG = Wasm_globals
module GE = Wasm_global_execution
module Func = Wasm_functions
module Calls = Wasm_calls
module P = Wasm_instance_control
type fixture = {memory : B.bytes; expected : B.bytes; block : T.code; globals : WG.t; empty : bool}
let rec (above @ total) : (heap : Heap.heap) @ immutable -> (boundary : B.u32) ->
    {out : bool | out === Above.above heap boundary} = fun heap boundary ->
  ghost_ (Above.above_def heap boundary);
  match heap with Heap.Empty_heap _ -> true | Heap.Allocate (a, rest) -> boundary <= a.Heap.address && above rest boundary
let rec filler n = if n = 0 then B.End else B.Byte (173, filler (n - 1))
let[@def] (bound @ total) (u : unit) : B.u32 = 8192
let rec padding n = if n = 0 then Heap.Empty else Heap.Cell (V.Nil, padding (n - 1))
let fixture : (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (configuration : Machine.configuration) @ immutable -> (abstract : Hmc_cfg_semantics.state) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (elapsed : D.index) @ immutable ->
    (base : B.u32) -> {u : unit | Inv.valid program globals (bound ()) configuration abstract
      && abstract === Abstract.advance program elapsed (Abstract.initial program input)} -> fixture =
  fun program globals configuration abstract input elapsed base premise ->
  ghost_ (bound_def ());
  if base > 7 then failwith "reached list base" else
  match Lower.lower program globals 1000 with
  | None -> failwith "list program lowering"
  | Some lowered_program ->
  let heap = configuration.Machine.heap in
  let table = program.Program.origin.Hmc_cfg_program.origin.Hmc_closure_program.table in
  ghost_ (Inv.valid_def program globals 8192 configuration abstract);
  match configuration.Machine.state with
  | State.Running (activation, frames) ->
    (match Program.lookup program.Program.code activation.Frame.pc, G.lookup program.Program.origin.Hmc_cfg_program.blocks activation.Frame.pc, activation.Frame.accumulator with
    | Some (Program.Keep (G.List_branch (empty, next))), Some block, value ->
      let signature = block.G.signature in
      let _ = ghost_ (Hmc_heap_reachable_operands.progress program input elapsed abstract ()) in
      let fields = Hmc_heap_operand_shapes.list program heap activation frames abstract empty next () in
      (match signature.G.accumulator with
      | Some (D.List_type element) ->
      let address = fields.Hmc_heap_operand_shapes.address in
      let head = fields.Hmc_heap_operand_shapes.head in
      let tail = fields.Hmc_heap_operand_shapes.tail in
      let _ = ghost_ (Hmc_wasm_program_selection.frame_shape program globals 8192 configuration abstract activation frames block ()) in
      let _ = ghost_ (Lower.corresponds_def program globals 1000 lowered_program;
        Capacity.lookup program.Program.origin.Hmc_cfg_program.blocks activation.Frame.pc block ();
        Hmc_pointer_frame_shape.size signature) in
      let length = Capacity.remaining (Capacity.capacity program.Program.origin.Hmc_cfg_program.blocks) (Codec.size signature) () in
      let padding = Pad.cells length in
      ghost_ (Pad.length length);
      let cells = Codec.encode signature activation padding () in
      (match Hmc_u32_index.encode 1000 (Heap.length cells), Hmc_u32_index.encode 1000 activation.Frame.pc, Hmc_u32_index.encode 1000 empty with
      | Some capacity, Some old_pc, Some empty_pc ->
        let frame_stop : B.u32 = base + 16 + 16 * capacity in
        let cursor = Heap.used heap in
        if frame_stop > 1024 || cursor > 8000 || frame_stop > cursor then failwith "reached list memory layout" else
        if not (above heap frame_stop && Image.encodable 1000 heap) then failwith "reached list heap encoding" else
        let full = Heap.Cell (V.Word (Header.number old_pc), cells) in
        let suffix = filler 8192 in
        let bytes = Wire.encode_cells full suffix in
        let initial = Hmc_wasm_frame_fixture.prefix base bytes in
        (match L.drop initial 8192, L.drop initial frame_stop with
        | Some _, Some _ ->
          ghost_ (Bounds.covers_def initial 8192; Bounds.covers_def initial frame_stop);
          let memory = Hmc_heap_image_prefix.materialize table 1000 initial heap 8192 frame_stop () in
          ghost_ (Heap.length_def full; Hmc_u32_index.represents_def (D.S (Heap.length cells)) (capacity + 1));
          let transported = Hmc_wasm_frame_transport.correct initial memory base frame_stop (capacity + 1) full bytes suffix () in
          let slots = {Capture.head_tag = 3; head_payload = 4; tail_tag = 5; tail_payload = 6} in
          let scratch = S.Push (S.I64 (Header.number 999), S.Push (S.I64 (Header.number 999), S.Push (S.I64 (Header.number 999), S.Push (S.I64 (Header.number 999), S.Push (S.I32 99, S.Empty))))) in
          let locals = S.Push (S.I32 base, S.Push (S.I32 999, S.Push (S.I32 cursor, scratch))) in
          if not (Capture.distinct slots && Capture.separate slots 0 && Capture.separate slots 1 && Capture.separate slots 2 && Capture.writable slots locals) then failwith "reached list scratch layout" else
          if capacity <> lowered_program.Lower.capacity then failwith "list frame capacity" else
          let _ = ghost_ (Program.valid_def program;
            Hmc_wasm_program_table.lookup_correct globals program.Program.origin.Hmc_cfg_program.blocks program.Program.code program.Program.sites
              lowered_program.Lower.blocks lowered_program.Lower.capacity 1000 activation.Frame.pc old_pc ()) in
          (match Hmc_wasm_program_table.lookup lowered_program.Lower.blocks old_pc with
          | Some (Hmc_wasm_program_block.Structured (Block.List_branch list as lowered)) ->
            ghost_ (Hmc_wasm_program_block.corresponds_def globals signature (Program.Keep (G.List_branch (empty, next))) capacity 1000 (Hmc_wasm_program_block.Structured lowered);
              Block.corresponds_def globals signature (G.List_branch (empty, next)) capacity 1000 lowered);
            let runtime = {Emit.structured = {Block.frame = 0; heap = 2; limit = 7; object_ = 1;
              scratch = {Hmc_wasm_cons_capture.head_tag = 3; head_payload = 4; tail_tag = 5; tail_payload = 6}};
              top = 0; stack_limit = 0; code = 0; address = 0; descriptor = {Hmc_wasm_descriptor_load.start = 0; captures = 0; recursive = 0};
              status = 7; result_tag = 3; result_payload = 4} in
              let config = {Assembly.locals = runtime;
                local_types = Func.Local32 (Func.Local32 (Func.Local32 (Func.Local64 (Func.Local64 (Func.Local64 (Func.Local64 (Func.Local32 Func.No_locals)))))));
                loads = Registers.Binding (0, 0, Registers.Binding (1, 1, Registers.Binding (2, 2, Registers.Binding (3, 3,
                  Registers.Binding (4, 4, Registers.Binding (5, 5, Registers.Binding (6, 6, Registers.Binding (7, 7, Registers.End))))))));
                stores = Registers.Binding (2, 2, Registers.Binding (7, 7, Registers.End)); table_base = 0; stack_base = 0} in
              let wasm_globals = {WG.values = locals; permissions = WG.Global (false, WG.Global (false, WG.Global (true,
                WG.Global (false, WG.Global (false, WG.Global (false, WG.Global (false, WG.Global (true, WG.Empty))))))))} in
              let initial = {GE.globals = wasm_globals; execution = {X.memory; machine = {E.locals = Func.zero_locals config.Assembly.local_types; stack = S.Empty}}} in
              (match Registers.load config.Assembly.loads initial with
              | None -> failwith "Cons register import"
              | Some imported ->
              let state = imported.GE.execution in
              let locals = state.X.machine.E.locals in
              (match Wasm_locals.get locals 0, Wasm_locals.get locals 1, Wasm_locals.get locals 2 with
              | Some (S.I32 actual_base), Some (S.I32 actual_object), Some (S.I32 actual_cursor) ->
              if actual_base <> base || actual_object <> 999 || actual_cursor <> cursor || not (Capture.writable slots locals) then failwith "Cons imported registers" else
              if not (Wasm_locals.can_set locals 7 (S.I32 2) && Wasm_locals.can_set locals 1 (S.I32 address) && Capture.separate slots 7) then failwith "list scratch registers" else
              let _ = ghost_ (New.failure_def (); Capture.separate_def slots 7) in
              let entry = New.correct lowered_program runtime 0 0 (Round.labels config) program globals 8192 (D.S (D.S D.Z)) frames empty signature element activation next head tail list capacity 1000 old_pc cells padding
                table heap address 1 2 frame_stop state 0 base slots transported.Hmc_wasm_frame_transport.bytes transported.Hmc_wasm_frame_transport.tail () in
              let fragment_ = Hmc_wasm_program_block.Structured lowered in
              let target = Assembly.function_ lowered_program fragment_ config in
              let module_ = {Func.functions = Func.Function (target, Func.No_functions); signatures = Func.Signature (Func.Void, Func.No_signatures);
                table = Func.Element (Some 0, Func.No_elements)} in
              let call = {Calls.current = {P.globals = wasm_globals; body = {T.code = T.Instruction (I.Call_indirect 0, T.Empty); labels = T.No_labels;
                state = {X.memory; machine = {E.locals = S.Empty; stack = S.Push (S.I32 0, S.Empty)}}}};
                result = Func.Void; callers = Calls.Root; capacity = C.Succ C.Zero} in
              (match Registers.store config.Assembly.stores {GE.globals = wasm_globals; execution = entry.New.state} with
              | None -> failwith "Cons register export"
              | Some exported ->
                ghost_ (Func.signature_def module_.Func.signatures 0; Func.element_def module_.Func.table 0; Func.lookup_def module_.Func.functions 0;
                  Round.correct lowered_program fragment_ config module_ call 0 0 0 T.Empty S.Empty C.Zero state entry.New.state exported entry.New.fuel ());
                (match Calls.run (Round.cost config entry.New.fuel) module_ call with
                | Calls.Running actual ->
                  if actual.Calls.current.P.body.T.state.X.memory <> entry.New.state.X.memory
                    || WG.get actual.Calls.current.P.globals 2 <> Some (S.I32 cursor)
                    || WG.get actual.Calls.current.P.globals 7 <> Some (S.I32 0)
                    || actual.Calls.capacity <> call.Calls.capacity || actual.Calls.callers <> Calls.Root then failwith "Cons indirect return"
                | _ -> failwith "Cons indirect execution");
                (match Hmc_u32_index.encode 1000 (G.size program.Program.origin.Hmc_cfg_program.blocks) with
                | None -> failwith "list dispatcher block count"
                | Some count ->
                  if old_pc >= count then failwith "list dispatcher PC range" else
                  let runtime = Hmc_wasm_program_runtime.config 0 512 in
                  let registers = {Hmc_wasm_program_registers.frame = base; heap = cursor;
                    heap_limit = 8192; top = 512; stack_limit = 1024; status = 99;
                    tag = Header.number 123; payload = Header.number 456} in
                  let module_ = Assembly.assemble lowered_program (G.size program.Program.origin.Hmc_cfg_program.blocks) count runtime (Hmc_wasm_program_runtime.dispatcher ()) in
                  ghost_ (Assembly.source_order globals program.Program.origin.Hmc_cfg_program.blocks program.Program.code lowered_program.Lower.blocks lowered_program.Lower.capacity 1000 count ();
                    Assembly.dispatch_target lowered_program (G.size program.Program.origin.Hmc_cfg_program.blocks) count runtime (Hmc_wasm_program_runtime.dispatcher ()) old_pc fragment_ ();
                    Assembly.assemble_def lowered_program (G.size program.Program.origin.Hmc_cfg_program.blocks) count runtime (Hmc_wasm_program_runtime.dispatcher ());
                    Hmc_wasm_program_dispatch.void_signature_def (); Func.signature_def module_.Func.signatures 0;
                    Hmc_wasm_program_source_list.fragment_def list);
                  let step = Hmc_wasm_program_source_list.correct lowered_program module_ program globals heap signature element activation frames
                    (D.S (D.S D.Z)) empty next head tail address list capacity 1000 old_pc cells padding registers memory
                    transported.Hmc_wasm_frame_transport.bytes transported.Hmc_wasm_frame_transport.tail frame_stop 0 512 C.Zero
                    (Assembly.reverse_index count old_pc) () in
                  if step.Hmc_wasm_program_source_list.source.Finished.memory <> entry.New.state.X.memory then failwith "list dispatcher source memory";
                  let before = Hmc_wasm_program_dispatch.loop (Hmc_wasm_program_registers.globals registers) memory (C.Succ C.Zero) in
                  let expected = Hmc_wasm_program_dispatch.loop (Hmc_wasm_program_registers.globals step.Hmc_wasm_program_source_list.registers) entry.New.state.X.memory (C.Succ C.Zero) in
                  if Calls.run step.Hmc_wasm_program_source_list.fuel module_ before <> Calls.Running expected then failwith "list dispatcher execution");
                {memory; expected = entry.New.state.X.memory; block = target.Func.code; globals = wasm_globals;
                  empty = Hmc_wasm_list_probe.is_nil activation.Frame.accumulator})
              | _ -> failwith "Cons imported types"))

          | _ -> failwith "list lowering")
        | _ -> failwith "reached list initial coverage")
      | _ -> failwith "reached list frame encoding")
      | _ -> failwith "reached list schema")
    | _ -> failwith "reached list lookup")
  | _ -> failwith "reached list state"
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
      let fixtures = match Program.lookup program.Program.code activation.Frame.pc, activation.Frame.accumulator with
        | Some (Program.Keep (G.List_branch _)), _ ->
          [fixture program globals configuration abstract input elapsed 0 ();
           fixture program globals configuration abstract input elapsed 7 ()]
        | _ -> [] in
      ghost_ (Hmc_heap_reachable_operands.advance_next program input elapsed; Inv.step program globals 8192 (D.S (D.S D.Z)) configuration abstract ());
      match Machine.step program globals 8192 (D.S (D.S D.Z)) configuration with
      | Machine.Exhausted _ -> failwith "reached source exhausted"
      | Machine.Advanced next -> fixtures @ collect program globals next (Abstract.step program abstract) input (D.S elapsed) (fuel - 1) ()
let cases source =
  let program = Hmc_wasm_global_fixture.build source in
  let input = Header.number 42 in
  match Hmc_heap_initialize.initialize program 1024 8192 input () with
  | Hmc_heap_initialize.Heap_exhausted _ -> failwith "reached initialization exhausted"
  | Hmc_heap_initialize.Initialized start ->
    ghost_ (bound_def (); Hmc_heap_initialize.correct_def program 1024 8192 input (Hmc_heap_initialize.Initialized start));
    ghost_ (Abstract.advance_def program D.Z (Abstract.initial program input));
    collect program start.Hmc_heap_initialize.globals start.Hmc_heap_initialize.configuration (Abstract.initial program input) input D.Z 200 ()
let fixtures () =
  let words = D.Lambda (D.CaseList (D.Cons (D.Bound D.Z, D.Cons (D.Word (Header.number 2), D.Nil)), D.Word (Header.number 0), D.Bound D.Z)) in
  let closures = D.Lambda (D.CaseList (D.Cons (D.Lambda (D.Bound (D.S D.Z)), D.Cons (D.Lambda (D.Bound D.Z), D.Nil)), D.Word (Header.number 0), D.Word (Header.number 1))) in
  let empty_words = D.Lambda (D.CaseList (D.Nil, D.Word (Header.number 0), D.Bound D.Z)) in
  let empty_closures = D.Lambda (D.CaseList (D.Nil, D.Word (Header.number 0), D.Apply (D.Bound D.Z, D.Word (Header.number 1)))) in
  let fixtures = cases words @ cases closures @ cases empty_words @ cases empty_closures in
  if List.length fixtures <> 8 then failwith "reached list coverage";
  fixtures
