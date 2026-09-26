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
module Entry = Hmc_wasm_call_dispatch
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
module Block = Hmc_wasm_structured_block
module Image = Hmc_heap_image
module Above = Hmc_heap_image_suffix
module L = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
type fixture = {memory : B.bytes; expected : B.bytes; prefix : I.t list; code : C.t; cursor : B.u32}
let rec (above @ total) : (heap : Heap.heap) @ immutable -> (boundary : B.u32) ->
    {out : bool | out === Above.above heap boundary} = fun heap boundary ->
  ghost_ (Above.above_def heap boundary);
  match heap with Heap.Empty_heap _ -> true | Heap.Allocate (a, rest) -> boundary <= a.Heap.address && above rest boundary
let rec fuel n = if n = 0 then C.Zero else C.Succ (fuel (n - 1))
let rec filler n = if n = 0 then B.End else B.Byte (173, filler (n - 1))
let[@def] (bound @ total) (u : unit) : B.u32 = 8192
module Code = Hmc_wasm_closure_code
module Copy = Hmc_wasm_call_captures
module Memory = Hmc_wasm_dynamic_call_frame
module Words = Hmc_wire_word_sequence
let fixture : (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (configuration : Machine.configuration) @ immutable -> (abstract : Hmc_cfg_semantics.state) @ immutable ->
    {u : unit | Inv.valid program globals (bound ()) configuration abstract} -> fixture =
  fun program globals configuration abstract premise ->
    ghost_ (bound_def (); Inv.valid_def program globals 8192 configuration abstract);
    let heap = configuration.Machine.heap in
    let table = program.Program.origin.Hmc_cfg_program.origin.Hmc_closure_program.table in
    match configuration.Machine.state with
    | State.Running (activation, _) ->
      (match activation.Frame.temporaries with
      | Frame.Value (V.Closure_pointer address, _, _) ->
        (match Hmc_heap_preservation.lookup_object heap address with
        | Some (Heap.Closure (id, captures)) ->
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
              if not (Codec.environment entry.Hmc_closure_ir.captured captures) then failwith "callee capture shape" else
              if not (Hm_elaboration_check.index_equal (Codec.locals_size entry.Hmc_closure_ir.captured) (Heap.length captures)) then failwith "capture context length" else
              if count > 64 then failwith "capture fixture count" else
              let minimum : Hmc_wasm_relayout.count = (if entry.Hmc_closure_ir.recursive then 4 else 3) + count in
              if Copy.build entry (minimum - 1) <> None || Copy.build entry minimum = None then failwith "capture capacity boundary";
              (match Copy.build entry 80 with
              | None -> failwith "capture plan build"
              | Some fragment ->
              match Plans.build table 80 1000 with
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
              let l8 = S.Push (S.I32 0, S.Empty) in
              let l7 = S.Push (S.I32 0, l8) in
              let l6 = S.Push (S.I32 0, l7) in
              let l5 = S.Push (S.I32 index, l6) in
              let l4 = S.Push (S.I32 0, l5) in
              let l3 = S.Push (S.I64 (V.payload argument), l4) in
              let l2 = S.Push (S.I64 (V.tag argument), l3) in
              let rest = S.Push (S.I32 256, l2) in
              let locals = S.Push (S.I32 address, rest) in
              let state = {X.memory; machine = {E.locals; stack = S.Empty}} in
              ghost_ (Wasm_locals.get_def locals 5; Wasm_locals.get_def rest 4; Wasm_locals.get_def l2 3;
                Wasm_locals.get_def l3 2; Wasm_locals.get_def l4 1; Wasm_locals.get_def l5 0;
                Wasm_locals.get_def locals 4; Wasm_locals.get_def rest 3; Wasm_locals.get_def l2 2;
                Wasm_locals.get_def l3 1; Wasm_locals.get_def l4 0;
                Wasm_locals.get_def locals 0; Wasm_locals.get_def locals 1; Wasm_locals.get_def rest 0;
                Wasm_locals.get_def locals 2; Wasm_locals.get_def locals 3; Wasm_locals.get_def rest 1; Wasm_locals.get_def rest 2;
                Wasm_locals.get_def l2 0; Wasm_locals.get_def l2 1; Wasm_locals.get_def l3 0;
                Hmc_wasm_call_header_layout.width_def fragment.Copy.recursive;
                Copy.matches_def entry 80 fragment; Copy.position_def entry.Hmc_closure_ir.recursive;
                Hmc_wasm_relayout_geometry.size_represents (Heap.length captures) count ();
                Words.size prefix_cells prefix_count offset ());
              let _read = Hmc_wasm_closure_read.correct table heap memory address id captures count () in
              if not (above heap stop) then failwith "callee frame overlaps heap" else
              let slots = {Slots.start = 6; captures = 7; recursive = 8} in
              if not (Slots.distinct slots 4 && Slots.writable slots locals && Separate.separate slots 4 5
                && Separate.separate slots 4 0 && Separate.separate slots 4 1 && Separate.separate slots 4 2 && Separate.separate slots 4 3)
              then failwith "dispatch locals" else
              let initialized = Entry.correct program heap entry function_ id address captures argument count plans index 5 T.No_labels 80 runtime 0 table_count 4 slots descriptor state 256 stop 8192 0 1 2 3 () in
              let result = initialized.Entry.call.Plan_entry.entry.Dynamic.frame in
              if Machine.invoke program heap (V.Closure_pointer address) argument <> Some initialized.Entry.call.Plan_entry.entry.Dynamic.entered then failwith "callee source invocation";

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
                let control = Entry.emit plans 0 5 4 slots 0 1 2 3 in
                if T.run (fuel 10000) {T.code = control; labels = T.No_labels; state} <> T.Finished {X.memory = expected; machine = {E.locals = initialized.Entry.locals; stack = S.Empty}}
                then failwith "selected callee control";
                (match Wasm_locals.set locals 5 (S.I32 4294967295) with
                | None -> failwith "invalid callee local"
                | Some invalid ->
                  if T.run (fuel 10000) {T.code = control; labels = T.No_labels; state = {state with X.machine = {state.X.machine with E.locals = invalid}}} <> T.Trap
                  then failwith "invalid callee accepted");
                {memory; expected; prefix = [I.I32_const address; I.Local_set 0; I.I32_const 256; I.Local_set 1; I.I64_const (V.tag argument); I.Local_set 2; I.I64_const (V.payload argument); I.Local_set 3; I.I32_const 0; I.Local_set 4; I.I32_const index; I.Local_set 5]; code = T.flatten control C.Empty; cursor = 256}
              | _ -> failwith "capture copy bounds")))))))

          | _ -> failwith "call index encoding")
        | _ -> failwith "call closure object")
      | _ -> failwith "call operand")
    | _ -> failwith "call source state"
let rec collect : (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (configuration : Machine.configuration) @ immutable -> (abstract : Hmc_cfg_semantics.state) @ immutable ->
    int -> {u : unit | Inv.valid program globals (bound ()) configuration abstract} -> fixture list =
  fun program globals configuration abstract fuel premise ->
    ghost_ (bound_def ());
    match configuration.Machine.state with
    | State.Done _ -> []
    | State.Stuck -> failwith "reached source stuck"
    | State.Running (activation, _) ->
      if fuel = 0 then failwith "reached source fuel" else
      let fixtures = match Program.lookup program.Program.code activation.Frame.pc with
        | Some (Program.Keep (G.Call _)) | Some Program.Tail_call ->
          [fixture program globals configuration abstract ()]
        | _ -> [] in
      ghost_ (Inv.step program globals 8192 (D.S (D.S D.Z)) configuration abstract ());
      match Machine.step program globals 8192 (D.S (D.S D.Z)) configuration with
      | Machine.Exhausted _ -> failwith "reached source exhausted"
      | Machine.Advanced next -> fixtures @ collect program globals next (Abstract.step program abstract) (fuel - 1) ()
let cases source (initial_base : B.u32) =
  if initial_base > 1031 then failwith "call heap base" else
  let program = Hmc_wasm_global_fixture.build source in
  let input = Header.number 42 in
  match Hmc_heap_initialize.initialize program initial_base 8192 input () with
  | Hmc_heap_initialize.Heap_exhausted _ -> failwith "reached initialization exhausted"
  | Hmc_heap_initialize.Initialized start ->
    ghost_ (bound_def (); Hmc_heap_initialize.correct_def program initial_base 8192 input (Hmc_heap_initialize.Initialized start));
    collect program start.Hmc_heap_initialize.globals start.Hmc_heap_initialize.configuration (Abstract.initial program input) 200 ()
let fixtures () =
  let captured = D.Lambda (D.Apply (D.Lambda (D.Bound (D.S D.Z)), D.Word (Header.number 7))) in
  let recursive = D.Lambda (D.Apply (D.Recursive (D.Bound D.Z), D.Word (Header.number 7))) in
  let fixtures = cases captured 1024 @ cases captured 1031 @ cases recursive 1024 @ cases recursive 1031 in
  if List.length fixtures <> 4 then failwith "call capture coverage";
  fixtures
