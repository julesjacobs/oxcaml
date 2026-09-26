module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module P = Hmc_tail_ir
module Cfg = Hmc_cfg_program
module Heap = Hmc_heap_objects
module Frame = Hmc_heap_frame
module State = Hmc_heap_state
module Machine = Hmc_heap_machine
module Inv = Hmc_heap_invariant
module Abstract = Hmc_tail_semantics
module Codec = Hmc_pointer_frame_codec
module Wire = Hmc_heap_wire
module V = Hmc_tagged_cell
module Header = Hmc_wasm_header_update
module Index = Hmc_u32_index
module Capacity = Hmc_frame_capacity
module Pad = Hmc_wasm_frame_padding
module Lower = Hmc_wasm_program_lower
module Table = Hmc_wasm_program_table
module Block = Hmc_wasm_program_block
module Structured = Hmc_wasm_structured_block
module Straight = Hmc_wasm_block_lower
module Jump = Hmc_wasm_program_jump
module Branch = Hmc_wasm_program_branch
module Local = Hmc_wasm_program_local
module Literal = Hmc_wasm_program_literal
module Simple = Hmc_wasm_simple_lower
module Primitive = Hmc_wasm_program_primitive
module Save = Hmc_wasm_program_save_environment
module Saved = Hmc_wasm_program_saved_environment
module Assembly = Hmc_wasm_program_functions
module Round = Hmc_wasm_program_roundtrip
module R = Wasm_global_registers
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module GE = Wasm_global_execution
module WG = Wasm_globals
module F = Wasm_functions
module T = Wasm_control
module Calls = Wasm_calls
module Count = Wasm_code
module L = Wasm_locals
module Config = Hmc_wasm_program_global_fixture
type fixture = {image : Config.fixture; kind : int}
let finish : (lowered : Lower.program) @ immutable -> (fragment : Block.fragment) @ immutable -> (config : Assembly.config) @ immutable ->
    (globals : WG.t) @ immutable -> (memory : B.bytes) @ immutable -> (imported : X.state) @ immutable ->
    (after : X.state) @ immutable -> (fuel : Count.count) @ immutable -> (expected : B.bytes) @ immutable -> (base : B.u32) -> int ->
    {u : unit | imported.X.machine.E.stack === S.Empty && after.X.machine.E.stack === S.Empty
      && GE.run (R.load_code config.Assembly.loads) {GE.globals; execution = {X.memory;
        machine = {E.locals = F.zero_locals config.Assembly.local_types; stack = S.Empty}}} === GE.Done {GE.globals; execution = imported}
      && T.run fuel {T.code = Hmc_wasm_program_emit.emit lowered fragment config.Assembly.locals config.Assembly.table_base config.Assembly.stack_base;
        labels = Round.labels config; state = imported} === T.Running {T.code = T.Empty; labels = Round.labels config; state = after}} -> fixture =
  fun lowered fragment config globals memory imported after fuel expected base kind premise ->
    let target = Assembly.function_ lowered fragment config in
    let module_ = {F.functions = F.Function (target, F.No_functions); signatures = F.Signature (F.Void, F.No_signatures);
      table = F.Element (Some 0, F.No_elements)} in
    let call = {Calls.current = {Wasm_instance_control.globals; body = {T.code = T.Instruction (Wasm_instruction.Call_indirect 0, T.Empty);
      labels = T.No_labels; state = {X.memory; machine = {E.locals = S.Empty; stack = S.Push (S.I32 0, S.Empty)}}}};
      result = F.Void; callers = Calls.Root; capacity = Count.Succ Count.Zero} in
    match R.store config.Assembly.stores {GE.globals; execution = after} with
    | None -> failwith "frame register export"
    | Some exported ->
      ghost_ (F.signature_def module_.F.signatures 0; F.element_def module_.F.table 0; F.lookup_def module_.F.functions 0;
        Round.correct lowered fragment config module_ call 0 0 0 T.Empty S.Empty Count.Zero imported after exported fuel ());
      (match Calls.run (Round.cost config fuel) module_ call with
      | Calls.Running returned ->
        if returned.Calls.current.Wasm_instance_control.body.T.state.X.memory <> expected
          || WG.get returned.Calls.current.Wasm_instance_control.globals 1 <> Some (S.I32 0)
          || returned.Calls.capacity <> call.Calls.capacity || returned.Calls.callers <> Calls.Root then
          failwith "generated frame block disagrees with source"
      | _ -> failwith "frame block failed to return");
      {image = {Config.memory; expected; base; block = target.F.code}; kind}
let audit_source_step (module_ : F.module_ @ immutable) (before : Hmc_wasm_program_registers.registers @ immutable)
    (memory : B.bytes @ immutable) (after : Hmc_wasm_program_registers.registers @ immutable) (fuel : Count.count @ immutable)
    (actual : B.bytes @ immutable) (expected : B.bytes @ immutable) =
  if actual <> expected then failwith "source dispatcher memory";
  match Calls.run fuel module_ (Hmc_wasm_program_dispatch.loop (Hmc_wasm_program_registers.globals before) memory (Count.Succ Count.Zero)) with
  | Calls.Running state ->
    let expected = Hmc_wasm_program_dispatch.loop (Hmc_wasm_program_registers.globals after) expected (Count.Succ Count.Zero) in
    if state <> expected then failwith "source dispatcher successor"
  | _ -> failwith "source dispatcher stopped"
let[@def] (bound @ total) (u : unit) : B.u32 = 1048576
let rec collect : (program : P.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (configuration : Machine.configuration) @ immutable -> (base : B.u32) ->
    (input : Hmc_word64.t) @ immutable -> (elapsed : D.index) @ immutable ->
    (abstract : Hmc_cfg_semantics.state) @ immutable -> int ->
    {u : unit | Inv.valid program globals (bound ()) configuration abstract
      && abstract === Abstract.advance program elapsed (Abstract.initial program input)} -> fixture list =
  fun program globals configuration base input elapsed abstract steps premise ->
  ghost_ (bound_def ());
  if base > 7 || steps = 0 then failwith "frame fixture bounds" else
  match configuration.Machine.state with
  | State.Done _ -> []
  | State.Stuck -> failwith "frame source stuck"
  | State.Running (activation, frames) ->
    let rest () =
      ghost_ (Inv.step program globals 1048576 (D.S (D.S (D.S D.Z))) configuration abstract ();
        Hmc_heap_reachable_operands.advance_next program input elapsed);
      match Machine.step program globals 1048576 (D.S (D.S (D.S D.Z))) configuration with
      | Machine.Exhausted _ -> failwith "frame source exhaustion"
      | Machine.Advanced next -> collect program globals next base input (D.S elapsed) (Abstract.step program abstract) (steps - 1) () in
    match P.lookup program.P.code activation.Frame.pc with
    | Some (P.Keep ((G.Load ((G.Local _ | G.Truth | G.False | G.Word _ | G.Nil), _, _, _) | G.Jump _ | G.Branch _ | G.Primitive _ | G.Save_environment _ | G.Save_value _ | G.Bind _ | G.Restore _) as instruction)) ->
      (match G.lookup program.P.origin.Cfg.blocks activation.Frame.pc, Lower.lower program globals 1024, Index.encode 1024 activation.Frame.pc with
      | Some block, Some lowered, Some old_pc ->
        let signature = block.G.signature in
        let _ = ghost_ (Hmc_wasm_program_selection.frame_shape program globals 1048576 configuration abstract activation frames block ()) in
        let _ = ghost_ (Lower.corresponds_def program globals 1024 lowered; P.valid_def program;
          Table.lookup_correct globals program.P.origin.Cfg.blocks program.P.code program.P.sites lowered.Lower.blocks lowered.Lower.capacity 1024 activation.Frame.pc old_pc ();
          Capacity.lookup program.P.origin.Cfg.blocks activation.Frame.pc block (); Hmc_pointer_frame_shape.size signature) in
        let length = Capacity.remaining (Capacity.capacity program.P.origin.Cfg.blocks) (Codec.size signature) () in
        let padding = Pad.cells length in
        ghost_ (Pad.length length);
        let cells = Codec.encode signature activation padding () in
        let full = Heap.Cell (V.Word (Header.number old_pc), cells) in
        let tail = B.Byte (42, B.End) in
        let before_frame = Wire.encode_cells full tail in
        let memory = Hmc_wasm_frame_fixture.prefix base before_frame in
        let config = Config.frame_config () in
        let wasm_globals = {WG.values = S.Push (S.I32 base, S.Push (S.I32 99, S.Empty)); permissions = WG.Global (false, WG.Global (true, WG.Empty))} in
        let initial = {GE.globals = wasm_globals; execution = {X.memory; machine = {E.locals = F.zero_locals config.Assembly.local_types; stack = S.Empty}}} in
        (match R.load config.Assembly.loads initial with
        | None -> failwith "frame register import"
        | Some imported ->
          let state = imported.GE.execution in
          (match L.get state.X.machine.E.locals 0, L.get state.X.machine.E.locals 1 with
          | Some (S.I32 actual_base), Some (S.I32 status) ->
            if actual_base <> base then failwith "frame imported base" else
            let _ = ghost_ (Config.frame_config_def (); Heap.length_def full;
              Jump.failure_def (); Branch.failure_def (); Local.failure_def (); Literal.failure_def (); Primitive.failure_def (); Save.failure_def (); Saved.failure_def ();
              L.can_set_def state.X.machine.E.locals 1 (S.I32 2); S.same_type_def (S.I32 status) (S.I32 2)) in
            (match Table.lookup lowered.Lower.blocks old_pc with
            | Some (Block.Structured (Structured.Straight straight) as fragment_) ->
              ghost_ (Block.corresponds_def globals signature (P.Keep instruction) lowered.Lower.capacity 1024 fragment_;
                Structured.corresponds_def globals signature instruction lowered.Lower.capacity 1024 (Structured.Straight straight);
                Straight.corresponds_def globals signature instruction lowered.Lower.capacity 1024 straight;
                Hmc_heap_simple.step_def instruction (State.Running (activation, frames)));
              let runtime = Hmc_wasm_program_runtime.config 0 512 in
              let registers = {Hmc_wasm_program_registers.frame = base; heap = Heap.used configuration.Machine.heap;
                heap_limit = 1048576; top = 512; stack_limit = 1024; status = 99;
                tag = Header.number 123; payload = Header.number 456} in
              (match Index.encode 1024 (G.size program.P.origin.Cfg.blocks) with
                | None -> failwith "dispatcher block count" | Some count ->
                if old_pc >= count then failwith "dispatcher PC range" else
                let module_ = Assembly.assemble lowered (G.size program.P.origin.Cfg.blocks) count runtime (Hmc_wasm_program_runtime.dispatcher ()) in
                ghost_ (Assembly.source_order globals program.P.origin.Cfg.blocks program.P.code lowered.Lower.blocks lowered.Lower.capacity 1024 count ();
                  Assembly.dispatch_target lowered (G.size program.P.origin.Cfg.blocks) count runtime (Hmc_wasm_program_runtime.dispatcher ()) old_pc fragment_ ();
                  Assembly.assemble_def lowered (G.size program.P.origin.Cfg.blocks) count runtime (Hmc_wasm_program_runtime.dispatcher ());
                  Hmc_wasm_program_dispatch.void_signature_def (); F.signature_def module_.F.signatures 0);
              let fixture = match instruction, straight with
                | G.Jump next, Straight.Simple (Simple.Jump pc as simple) ->
                  ghost_ (Simple.corresponds_def instruction simple);
                  let result = Jump.correct lowered config.Assembly.locals 0 0 (Round.labels config) program globals configuration.Machine.heap 1048576 (D.S (D.S (D.S D.Z)))
                    signature signature activation frames next pc old_pc cells padding state 0 base before_frame tail () in
                  let next_activation = {activation with Frame.pc = next} in
                  if not (Codec.shape signature next_activation) then failwith "jump next shape" else
                  let next_cells = Codec.encode signature next_activation padding () in
                  let expected = Hmc_wasm_frame_fixture.prefix base (Wire.encode_cells (Heap.Cell (V.Word (Header.number pc), next_cells)) tail) in
                  ghost_ (Hmc_wasm_program_source_jump.fragment_def pc);
                  let step = Hmc_wasm_program_source_jump.correct lowered module_ program globals configuration.Machine.heap
                    signature signature activation frames (D.S (D.S (D.S D.Z))) next pc old_pc cells padding
                    registers memory before_frame tail 0 512 Count.Zero (Assembly.reverse_index count old_pc) () in
                  audit_source_step module_ registers memory step.Hmc_wasm_program_source_jump.registers step.Hmc_wasm_program_source_jump.fuel
                    step.Hmc_wasm_program_source_jump.source.Hmc_wasm_jump_invariant.memory expected;
                  finish lowered fragment_ config wasm_globals memory state result.Jump.state result.Jump.fuel expected base 7 ()
                | G.Branch (yes, no), Straight.Simple (Simple.Branch (yes_pc, no_pc) as simple) ->
                  ghost_ (Simple.corresponds_def instruction simple);
                  let _ = ghost_ (Inv.valid_def program globals 1048576 configuration abstract;
                    Hmc_heap_reachable_operands.progress program input elapsed abstract ()) in
                  let _ = ghost_ (Hmc_wasm_program_source_branch.fragment_def yes_pc no_pc) in
                  let ready = Hmc_wasm_program_branch_ready.prepare program globals configuration.Machine.heap activation frames abstract block
                    lowered.Lower.capacity 1024 yes no yes_pc no_pc () in
                  let condition = ready.Hmc_wasm_program_branch_ready.condition in
                  let next = ready.Hmc_wasm_program_branch_ready.next in
                  let pc = ready.Hmc_wasm_program_branch_ready.pc in
                  let next_signature = ready.Hmc_wasm_program_branch_ready.block.G.signature in
                  let result = Branch.correct lowered config.Assembly.locals 0 0 (Round.labels config) program globals configuration.Machine.heap 1048576 (D.S (D.S (D.S D.Z)))
                    signature next_signature activation frames yes no yes_pc no_pc condition next pc old_pc cells padding state 0 base before_frame tail () in
                  let next_activation = {activation with Frame.pc = next} in
                  if not (Codec.shape next_signature next_activation) then failwith "branch next shape" else
                  let next_cells = Codec.encode next_signature next_activation padding () in
                  let expected = Hmc_wasm_frame_fixture.prefix base (Wire.encode_cells (Heap.Cell (V.Word (Header.number pc), next_cells)) tail) in
                  ghost_ (Hmc_wasm_program_source_branch.fragment_def yes_pc no_pc);
                  let step = Hmc_wasm_program_source_branch.correct lowered module_ program globals configuration.Machine.heap
                    signature next_signature activation frames (D.S (D.S (D.S D.Z))) yes no yes_pc no_pc condition next pc old_pc cells padding
                    registers memory before_frame tail 0 512 Count.Zero (Assembly.reverse_index count old_pc) () in
                  audit_source_step module_ registers memory step.Hmc_wasm_program_source_branch.registers step.Hmc_wasm_program_source_branch.fuel
                    step.Hmc_wasm_program_source_branch.source.Hmc_wasm_branch_invariant.memory expected;
                  finish lowered fragment_ config wasm_globals memory state result.Branch.state result.Branch.fuel expected base (if condition then 8 else 9) ()
                | G.Load (G.Local index, ty, derivation, next), Straight.Simple (Simple.Local (number, pc) as simple) ->
                  let next_signature = {signature with G.accumulator = Some ty} in
                  ghost_ (Simple.corresponds_def instruction simple);
                  if base + Simple.slot_tag number > 4294967280 then failwith "local address overflow" else
                  (match Hmc_heap_simple.lookup activation.Frame.env index with
                  | None -> failwith "missing source local"
                  | Some value ->
                    let result = Local.correct lowered config.Assembly.locals 0 0 (Round.labels config) program globals configuration.Machine.heap 1048576 (D.S (D.S (D.S D.Z)))
                      signature next_signature activation frames index number ty derivation next value pc old_pc cells padding state 0 base before_frame tail () in
                    let next_activation = {activation with Frame.pc = next; accumulator = value} in
                    if not (Codec.shape next_signature next_activation) then failwith "local next shape" else
                    let next_cells = Codec.encode next_signature next_activation padding () in
                    let expected = Hmc_wasm_frame_fixture.prefix base (Wire.encode_cells (Heap.Cell (V.Word (Header.number pc), next_cells)) tail) in
                  ghost_ (Hmc_wasm_program_source_local.fragment_def number pc);
                  let step = Hmc_wasm_program_source_local.correct lowered module_ program globals configuration.Machine.heap
                    signature next_signature activation frames (D.S (D.S (D.S D.Z))) index number ty derivation next value pc old_pc cells padding
                    registers memory before_frame tail 0 512 Count.Zero (Assembly.reverse_index count old_pc) () in
                  audit_source_step module_ registers memory step.Hmc_wasm_program_source_local.registers step.Hmc_wasm_program_source_local.fuel
                    step.Hmc_wasm_program_source_local.source.Hmc_wasm_local_invariant.memory expected;
                    finish lowered fragment_ config wasm_globals memory state result.Local.state result.Local.fuel expected base 6 ())
                | G.Load (atom, ty, derivation, next), Straight.Simple (Simple.Literal (value, pc) as simple) ->
                  let next_signature = {signature with G.accumulator = Some ty} in
                  ghost_ (Simple.corresponds_def instruction simple);
                  let result = Literal.correct lowered config.Assembly.locals 0 0 (Round.labels config) program globals configuration.Machine.heap 1048576 (D.S (D.S (D.S D.Z)))
                    signature next_signature activation frames atom ty derivation next value pc old_pc cells padding state 0 base before_frame tail () in
                  let next_activation = {activation with Frame.pc = next; accumulator = value} in
                  if not (Codec.shape next_signature next_activation) then failwith "literal next shape" else
                  let next_cells = Codec.encode next_signature next_activation padding () in
                  let expected = Hmc_wasm_frame_fixture.prefix base (Wire.encode_cells (Heap.Cell (V.Word (Header.number pc), next_cells)) tail) in
                  ghost_ (Hmc_wasm_program_source_literal.fragment_def value pc);
                    let step = Hmc_wasm_program_source_literal.correct lowered module_ program globals configuration.Machine.heap
                      signature next_signature activation frames (D.S (D.S (D.S D.Z))) atom ty derivation next value pc old_pc cells padding
                      registers memory before_frame tail 0 512 Wasm_code.Zero (Assembly.reverse_index count old_pc) () in
                  audit_source_step module_ registers memory step.Hmc_wasm_program_source_literal.registers
                    step.Hmc_wasm_program_source_literal.fuel step.Hmc_wasm_program_source_literal.source.Hmc_wasm_literal_invariant.memory expected;
                    (match Index.encode 1024 (Heap.length full) with
                    | None -> failwith "literal frame cell count"
                    | Some count ->
                      if base + 16 * count > 4294967295 then failwith "literal frame extent" else
                      let after = step.Hmc_wasm_program_source_literal.source in
                      let next_full = Heap.Cell (V.Word (Header.number pc), after.Hmc_wasm_literal_invariant.cells) in
                      ghost_ (Heap.length_def next_full;
                        Hmc_wasm_frame_preservation.correct memory after.Hmc_wasm_literal_invariant.memory base (base + 16 * count)
                          before_frame after.Hmc_wasm_literal_invariant.bytes full next_full tail count ());
                      if Hmc_linear_bytes.drop after.Hmc_wasm_literal_invariant.memory (base + 16 * count) <> Some tail
                        || V.length memory <> V.length after.Hmc_wasm_literal_invariant.memory then failwith "literal frame exterior");

                  let kind = match atom with G.Truth -> 10 | G.False -> 11 | G.Nil -> 12 | _ -> 5 in
                  finish lowered fragment_ config wasm_globals memory state result.Literal.state result.Literal.fuel expected base kind ()
                | G.Primitive (op, next), Straight.Primitive fragment ->
                  let _ = ghost_ (Inv.valid_def program globals 1048576 configuration abstract;
                    Hmc_heap_reachable_operands.progress program input elapsed abstract ()) in
                  let operands = Hmc_heap_operand_shapes.primitive program configuration.Machine.heap activation frames abstract op next () in
                  let left = operands.Hmc_heap_operand_shapes.left in
                  let right = operands.Hmc_heap_operand_shapes.right in
                  (match signature.G.temporaries,
                    Hmc_frame_primitive_model.successor signature op, Hmc_heap_simple.step instruction (State.Running (activation, frames)) with
                  | G.Value (context, D.Word64, schema), Some next_signature, State.Running (next_activation, _) ->
                    let result = Primitive.correct lowered config.Assembly.locals 0 0 (Round.labels config) program globals configuration.Machine.heap 1048576 (D.S (D.S (D.S D.Z)))
                      signature next_signature activation next_activation frames op next context schema fragment lowered.Lower.capacity 1024 old_pc left right cells padding state 0 base before_frame tail () in
                    if not (Codec.shape next_signature next_activation) then failwith "primitive next shape" else
                    let next_cells = Codec.encode next_signature next_activation result.Primitive.source.Hmc_wasm_primitive_invariant.padding () in
                    let expected = Hmc_wasm_frame_fixture.prefix base (Wire.encode_cells (Heap.Cell (V.Word (Header.number fragment.Hmc_wasm_primitive_lower.pc), next_cells)) tail) in
                    ghost_ (Hmc_wasm_program_source_primitive.fragment_def fragment);
                    let step = Hmc_wasm_program_source_primitive.correct lowered module_ program globals configuration.Machine.heap
                      signature next_signature activation next_activation frames (D.S (D.S (D.S D.Z))) op next context schema
                      fragment lowered.Lower.capacity 1024 old_pc left right cells padding registers memory before_frame tail 0 512 Count.Zero (Assembly.reverse_index count old_pc) () in
                    audit_source_step module_ registers memory step.Hmc_wasm_program_source_primitive.registers step.Hmc_wasm_program_source_primitive.fuel
                      step.Hmc_wasm_program_source_primitive.source.Hmc_wasm_primitive_invariant.memory expected;
                    finish lowered fragment_ config wasm_globals memory state result.Primitive.state result.Primitive.fuel expected base 0 ()
                  | _ -> failwith "primitive reached state")
                | G.Save_environment next, Straight.Relayout fragment ->
                  (match Hmc_frame_relayout_model.successor signature instruction, Hmc_heap_simple.step instruction (State.Running (activation, frames)) with
                  | Some next_signature, State.Running (next_activation, _) ->
                    let result = Save.correct lowered config.Assembly.locals 0 0 (Round.labels config) program globals configuration.Machine.heap 1048576 (D.S (D.S (D.S D.Z)))
                      signature next_signature activation next_activation frames next fragment lowered.Lower.capacity 1024 old_pc cells padding state 0 base before_frame tail () in
                    if not (Codec.shape next_signature next_activation) then failwith "save next shape" else
                    let next_cells = Codec.encode next_signature next_activation result.Save.source.Hmc_wasm_relayout_save_invariant.padding () in
                    let expected = Hmc_wasm_frame_fixture.prefix base (Wire.encode_cells (Heap.Cell (V.Word (Header.number fragment.Hmc_wasm_relayout.pc), next_cells)) tail) in
                    ghost_ (Hmc_wasm_program_source_save_environment.fragment_def fragment);
                    let step = Hmc_wasm_program_source_save_environment.correct lowered module_ program globals configuration.Machine.heap
                      signature next_signature activation next_activation frames (D.S (D.S (D.S D.Z))) next
                      fragment lowered.Lower.capacity 1024 old_pc cells padding registers memory before_frame tail 0 512 Count.Zero (Assembly.reverse_index count old_pc) () in
                    audit_source_step module_ registers memory step.Hmc_wasm_program_source_save_environment.registers step.Hmc_wasm_program_source_save_environment.fuel
                      step.Hmc_wasm_program_source_save_environment.source.Hmc_wasm_relayout_save_invariant.memory expected;
                    finish lowered fragment_ config wasm_globals memory state result.Save.state result.Save.fuel expected base 1 ()
                  | _ -> failwith "save reached state")
                | (G.Save_value next | G.Bind next | G.Restore next), Straight.Relayout fragment ->
                  (match signature.G.temporaries, Hmc_frame_relayout_model.successor signature instruction, Hmc_heap_simple.step instruction (State.Running (activation, frames)) with
                  | G.Environment (context, schema), Some next_signature, State.Running (next_activation, _) ->
                    let result = Saved.correct lowered config.Assembly.locals 0 0 (Round.labels config) program globals configuration.Machine.heap 1048576 (D.S (D.S (D.S D.Z)))
                      signature next_signature activation next_activation frames instruction next context schema fragment lowered.Lower.capacity 1024 old_pc cells padding state 0 base before_frame tail () in
                    if not (Codec.shape next_signature next_activation) then failwith "saved next shape" else
                    let next_cells = Codec.encode next_signature next_activation result.Saved.source.Hmc_wasm_relayout_saved_invariant.padding () in
                    let expected = Hmc_wasm_frame_fixture.prefix base (Wire.encode_cells (Heap.Cell (V.Word (Header.number fragment.Hmc_wasm_relayout.pc), next_cells)) tail) in
                    let kind = match instruction with G.Save_value _ -> 2 | G.Bind _ -> 3 | _ -> 4 in
                    ghost_ (Hmc_wasm_program_source_saved_environment.fragment_def fragment);
                    let step = Hmc_wasm_program_source_saved_environment.correct lowered module_ program globals configuration.Machine.heap
                      signature next_signature activation next_activation frames (D.S (D.S (D.S D.Z))) instruction next context schema
                      fragment lowered.Lower.capacity 1024 old_pc cells padding registers memory before_frame tail 0 512 Count.Zero (Assembly.reverse_index count old_pc) () in
                    audit_source_step module_ registers memory step.Hmc_wasm_program_source_saved_environment.registers step.Hmc_wasm_program_source_saved_environment.fuel
                      step.Hmc_wasm_program_source_saved_environment.source.Hmc_wasm_relayout_saved_invariant.memory expected;
                    finish lowered fragment_ config wasm_globals memory state result.Saved.state result.Saved.fuel expected base kind ()
                  | _ -> failwith "saved reached state")
                | _ -> failwith "compiler fragment mismatch" in
              fixture :: rest ())
            | _ -> failwith "missing generated frame block")
          | _ -> failwith "frame register types"))
      | _ -> failwith "source frame lookup")
    | _ -> rest ()
let cases term =
  let program = Config.build term in
  match Hmc_heap_initialize.initialize program 64 1048576 (Header.number 42) () with
  | Hmc_heap_initialize.Heap_exhausted _ -> failwith "frame initialization"
  | Hmc_heap_initialize.Initialized start ->
    let input = Header.number 42 in
    ghost_ (bound_def (); Hmc_heap_initialize.correct_def program 64 1048576 input (Hmc_heap_initialize.Initialized start);
      Abstract.advance_def program D.Z (Abstract.initial program input));
    collect program start.Hmc_heap_initialize.globals start.Hmc_heap_initialize.configuration 0 input D.Z (Abstract.initial program input) 500 ()
    @ collect program start.Hmc_heap_initialize.globals start.Hmc_heap_initialize.configuration 7 input D.Z (Abstract.initial program input) 500 ()
let fixtures () =
  let max = D.Word {Hmc_word64.lo = 4294967295; hi = 4294967295} in
  let word n = D.Word (Header.number n) in
  let term op left right =
    let result = match op with D.Equal_word | D.Unsigned_less -> D.If (D.Bound D.Z, word 7, word 9) | _ -> D.Bound D.Z in
    D.Lambda (D.Let (D.Primitive (op, left, right), result)) in
  let fixtures = cases (term D.Add max (word 1)) @ cases (term D.Subtract (word 0) (word 1))
    @ cases (term D.Equal_word max max) @ cases (term D.Unsigned_less max (word 0))
    @ cases (D.Lambda (D.If (D.Truth, word 1, word 2)))
    @ cases (D.Lambda (D.If (D.False, word 1, word 2)))
    @ cases (D.Lambda (D.CaseList (D.Nil, word 0, D.Bound D.Z))) in
  List.iter (fun kind -> if not (List.exists (fun f -> f.kind = kind) fixtures) then failwith "missing frame instruction coverage") [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12];
  fixtures
