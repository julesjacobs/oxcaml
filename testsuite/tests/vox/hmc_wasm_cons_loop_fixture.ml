module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Frame = Hmc_heap_frame
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
module Conditional = Hmc_wasm_cons_conditional
module Lift = Wasm_control_lift
module Fuel = Wasm_control_compose
type fixture = {memory : B.bytes; expected : B.bytes; prefix : I.t list; code : C.t; cursor : B.u32; trapped : bool}
let rec filler n = if n = 0 then B.End else B.Byte (173, filler (n - 1))
module Guarded = Hmc_wasm_cons_guarded
module Program = Hmc_tail_ir
module Block = Hmc_wasm_structured_block
module Table = Hmc_wasm_structured_table
module Prepared = Hmc_wasm_dispatch_code
module Select = Hmc_wasm_structured_select
module Entry = Hmc_wasm_cons_entry
module Loop = Hmc_wasm_structured_loop
let rec cons_pc = function
  | Program.Empty -> failwith "missing Cons instruction"
  | Program.Add (Program.Keep (G.Cons _), rest) -> Program.size rest
  | Program.Add (_, rest) -> cons_pc rest
let fixture (base : B.u32) enough =
  if base > 7 then failwith "operand fixture base" else
  let source = D.Lambda (D.CaseList (D.Cons (D.Bound D.Z, D.Nil), D.Word (Header.number 0), D.Bound D.Z)) in
  let program = Hmc_wasm_global_fixture.build source in
  let pc = cons_pc program.Program.code in
  match Program.lookup program.Program.code pc, Hmc_u32_index.encode 100 pc with
  | Some (Program.Keep (G.Cons next)), Some old_pc -> (
  let context1 = D.Binding (D.Forall (D.Z, D.Word64), D.Empty_context) in
  let context2 = D.Binding (D.Forall (D.Z, D.Word64), context1) in
  let signature = {G.locals = context2; temporaries = G.Value (context1, D.Word64, G.Empty_temporaries); accumulator = Some (D.List_type D.Word64)} in
  let head = V.Word (Header.number 42) in
  let activation = {Frame.pc = pc; current = V.Closure_pointer 64; accumulator = V.Nil;
    env = Heap.Cell (V.Word (Header.number 9), Heap.Cell (V.Word (Header.number 10), Heap.Empty));
    temporaries = Frame.Value (head, Heap.Cell (V.Word (Header.number 11), Heap.Empty), Frame.Empty)} in
  if not (Codec.shape signature activation) then failwith "operand frame shape" else
  let padding = Heap.Cell (V.Nil, Heap.Cell (V.Nil, Heap.Empty)) in
  let cells = Codec.encode signature activation padding () in
  let suffix = filler 128 in
  let full = Heap.Cell (V.Word (Header.number old_pc), cells) in
  let bytes = Wire.encode_cells full suffix in
  let memory = Hmc_wasm_frame_fixture.prefix base bytes in
  let heap_base : B.u32 = base + 192 in
  let limit : B.u32 = heap_base + (if enough then 32 else 31) in
  let slots = {Capture.head_tag = 3; head_payload = 4; tail_tag = 5; tail_payload = 6} in
  let scratch = S.Push (S.I64 (Header.number 999), S.Push (S.I64 (Header.number 999), S.Push (S.I64 (Header.number 999), S.Push (S.I64 (Header.number 999), S.Empty)))) in
  let locals = S.Push (S.I32 base, S.Push (S.I32 heap_base, S.Push (S.I32 limit, scratch))) in
  if not (Capture.distinct slots && Capture.separate slots 0 && Capture.separate slots 1 && Capture.separate slots 2 && Capture.writable slots locals) then failwith "operand scratch layout" else
  let state = {X.memory; machine = {E.locals; stack = S.Empty}} in
  ghost_ (Heap.length_def full; Wasm_locals.get_def locals 0;
    Wasm_locals.get_def locals 1; Wasm_locals.get_def (S.Push (S.I32 heap_base, S.Push (S.I32 limit, scratch))) 0;
    Wasm_locals.get_def locals 2; Wasm_locals.get_def (S.Push (S.I32 heap_base, S.Push (S.I32 limit, scratch))) 1; Wasm_locals.get_def (S.Push (S.I32 limit, scratch)) 0;
    Codec.locals_size_def context2; Codec.locals_size_def context1; Codec.locals_size_def D.Empty_context;
    Hmc_u32_index.represents_def (D.S (D.S D.Z)) 2; Hmc_u32_index.represents_def (D.S D.Z) 1; Hmc_u32_index.represents_def D.Z 0);
  match Hmc_linear_bytes.drop memory limit with
  | None -> failwith "operand heap coverage"
  | Some _ ->
    let heap = Heap.Empty_heap heap_base in
    ghost_ (Hmc_linear_bounds.covers_def memory limit; Heap.valid_def Hmc_closure_ir.Empty heap; Heap.used_def heap;
      Hmc_heap_image.related_def memory heap; Hmc_wasm_cons_fixture.word_object heap (Header.number 42));
    match Hmc_u32_index.encode 8 (Heap.length cells) with
    | None -> failwith "Cons frame capacity"
    | Some capacity -> if capacity <> 8 then failwith "Cons frame size" else
    let next_signature = {G.locals = context1; temporaries = G.Empty_temporaries; accumulator = Some (D.List_type D.Word64)} in
    let next_activation = {activation with Frame.pc = next; accumulator = V.Cons_pointer heap_base;
      env = Heap.Cell (V.Word (Header.number 11), Heap.Empty); temporaries = Frame.Empty} in
    let frame_stop : B.u32 = base + 144 in
    if Block.lower Hmc_heap_machine.Empty_globals {signature with G.accumulator = Some (D.List_type D.Boolean)} (G.Cons next) 8 100 <> None then failwith "Cons mismatched element accepted";
    if Block.lower Hmc_heap_machine.Empty_globals {signature with G.accumulator = None} (G.Cons next) 8 100 <> None then failwith "Cons missing tail accepted";
    if Block.lower Hmc_heap_machine.Empty_globals signature (G.Cons next) 4 100 <> None then failwith "Cons insufficient capacity accepted";
    match Block.lower Hmc_heap_machine.Empty_globals signature (G.Cons next) 8 100 with
    | None | Some (Block.Straight _) | Some (Block.List_branch _) | Some (Block.Closure _) -> failwith "Cons structured lowering"
    | Some (Block.Cons fragment as lowered) ->
      ghost_ (Block.corresponds_def Hmc_heap_machine.Empty_globals signature (G.Cons next) 8 100 lowered;
        Hmc_frame_value_pop.successor_def signature (D.List_type D.Word64);
        Hmc_frame_value_pop.transition_def activation next (V.Cons_pointer heap_base);
        Hmc_heap_image_suffix.above_def heap frame_stop);
      let locals = {Block.frame = 0; heap = 1; limit = 2; object_ = 2; scratch = slots} in
      let table = Table.Add (102, lowered, Table.Add (101, lowered, Table.Add (old_pc, lowered, Table.Empty))) in
      if Prepared.prepare table locals 4294967294 <> None then failwith "dispatch depth overflow accepted";
      match Prepared.prepare table locals 2 with
      | None -> failwith "structured dispatch preparation"
      | Some prepared ->
      let exit_label = T.Label ({T.restart = None; continuation = T.Empty; saved = S.Empty}, T.No_labels) in
      let outer = Loop.inner prepared 0 exit_label in
      let selected = Select.selection prepared old_pc 0 outer state in
      let depth = Prepared.exit_depth table old_pc 2 in
      ghost_ (Table.lookup_def table old_pc;
        Table.lookup_def (Table.Add (101, lowered, Table.Add (old_pc, lowered, Table.Empty))) old_pc;
        Table.lookup_def (Table.Add (old_pc, lowered, Table.Empty)) old_pc;
        Hmc_wasm_frame_pc.correct memory base bytes old_pc cells suffix ();
        Prepared.lookup_correct table prepared locals 2 old_pc ();
        Select.selected_code prepared old_pc 0 outer state;
        Loop.inner_def prepared 0 exit_label; Loop.labels_def prepared 0 exit_label;
        Wasm_control_branch_target.valid_def 2 outer; Wasm_control_branch_target.valid_def 1 (Loop.labels prepared 0 exit_label);
        Wasm_control_branch_target.valid_def 0 exit_label;
        Hmc_wasm_dispatch_exit.valid table prepared locals 2 old_pc lowered outer state ());
      let entry = Entry.correct selected.T.labels depth 2 program Hmc_heap_machine.Empty_globals D.Z signature next_signature activation next_activation Hmc_heap_state.Halt
        (D.List_type D.Word64) D.Word64 next context1 G.Empty_temporaries fragment 8 100 old_pc head V.Nil
        heap_base 1 2 cells padding Hmc_closure_ir.Empty heap limit slots frame_stop state 0 base bytes suffix () in
      let result = entry.Entry.result in
      let cost = Entry.cost fragment 0 1 2 slots heap_base limit in
      ghost_ (Entry.locals_def 0 1 2 slots;
        Hmc_wasm_structured_execute.correct prepared old_pc 0 base outer state cost (T.Running entry.Entry.target) ();
        (match result.Guarded.allocation with
        | Hmc_heap_allocate.Exhausted -> ()
        | Hmc_heap_allocate.Allocated _ -> Loop.correct prepared old_pc 0 base cost exit_label state result.Guarded.state ()));
      (match result.Guarded.allocation with
      | Hmc_heap_allocate.Exhausted -> ()
      | Hmc_heap_allocate.Allocated _ ->
        (match T.run (Loop.cost prepared old_pc 0 cost) (Loop.configuration prepared 0 exit_label state) with
        | T.Running actual -> if actual <> Loop.configuration prepared 0 exit_label result.Guarded.state then failwith "Cons loop iteration state"
        | _ -> failwith "Cons loop iteration stopped"));
      let code = T.Block (Loop.emit prepared 0, T.Empty) in
      (match T.run (Hmc_wasm_allocation_exit_fixture.fuel 10000) {T.code; labels = T.No_labels; state} with
      | T.Finished actual -> if enough || actual <> result.Guarded.state then failwith "Cons loop exhaustion state"
      | T.Trap -> if not enough then failwith "Cons loop exhaustion trapped"
      | _ -> failwith "Cons loop did not reach its boundary");
      let prefix = [I.I32_const base; I.Local_set 0; I.I32_const heap_base; I.Local_set 1; I.I32_const limit; I.Local_set 2;
        I.I64_const (Header.number 999); I.Local_set 3; I.I64_const (Header.number 999); I.Local_set 4;
        I.I64_const (Header.number 999); I.Local_set 5; I.I64_const (Header.number 999); I.Local_set 6] in
      {memory; expected = result.Guarded.state.X.memory; prefix; code = T.flatten code C.Empty; cursor = (if enough then heap_base + 32 else heap_base); trapped = enough})
  | _ -> failwith "Cons source lookup"
let fixtures () = [fixture 0 true; fixture 7 true; fixture 0 false; fixture 7 false]
