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
module Entry = Hmc_wasm_cons_entry
module Guarded = Hmc_wasm_cons_guarded
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
let rec filler n = if n = 0 then B.End else B.Byte (173, filler (n - 1))
let[@def] (bound @ total) (u : unit) : B.u32 = 8192
let fixture : (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (configuration : Machine.configuration) @ immutable -> (abstract : Hmc_cfg_semantics.state) @ immutable ->
    (base : B.u32) -> bool -> {u : unit | Inv.valid program globals (bound ()) configuration abstract} -> fixture =
  fun program globals configuration abstract base enough premise ->
  ghost_ (bound_def ());
  if base > 7 then failwith "reached Cons base" else
  let heap = configuration.Machine.heap in
  let table = program.Program.origin.Hmc_cfg_program.origin.Hmc_closure_program.table in
  ghost_ (Inv.valid_def program globals 8192 configuration abstract;
    Inv.request_valid program heap configuration.Machine.state abstract ());
  match configuration.Machine.state with
  | State.Running (activation, frames) ->
    (match Program.lookup program.Program.code activation.Frame.pc, G.lookup program.Program.origin.Hmc_cfg_program.blocks activation.Frame.pc, activation.Frame.temporaries with
    | Some (Program.Keep (G.Cons next)), Some block, Frame.Value (head, _, _) ->
      let signature = block.G.signature in
      (match signature.G.temporaries with
      | G.Value (context, head_type, schema) ->
      if not (Codec.shape signature activation) then failwith "reached frame shape" else
      let padding = Heap.Cell (V.Nil, Heap.Cell (V.Nil, Heap.Empty)) in
      let cells = Codec.encode signature activation padding () in
      (match Hmc_u32_index.encode 1000 (Heap.length cells), Hmc_u32_index.encode 1000 activation.Frame.pc,
          Hmc_u32_index.encode 1000 (Codec.locals_size signature.G.locals) with
      | Some capacity, Some old_pc, Some env_count ->
        let frame_stop : B.u32 = base + 16 + 16 * capacity in
        let cursor = Heap.used heap in
        if frame_stop > 1024 || cursor > 8000 || frame_stop > cursor then failwith "reached memory layout" else
        if not (above heap frame_stop && Image.encodable 1000 heap) then failwith "reached heap encoding" else
        let limit : B.u32 = cursor + (if enough then 32 else 31) in
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
          (match L.drop memory limit with
          | None -> failwith "reached memory coverage"
          | Some _ ->
            let slots = {Capture.head_tag = 3; head_payload = 4; tail_tag = 5; tail_payload = 6} in
            let scratch = S.Push (S.I64 (Header.number 999), S.Push (S.I64 (Header.number 999), S.Push (S.I64 (Header.number 999), S.Push (S.I64 (Header.number 999), S.Empty)))) in
            let locals = S.Push (S.I32 base, S.Push (S.I32 cursor, S.Push (S.I32 limit, scratch))) in
            if not (Capture.distinct slots && Capture.separate slots 0 && Capture.separate slots 1 && Capture.separate slots 2 && Capture.writable slots locals) then failwith "reached scratch layout" else
            let state = {X.memory; machine = {E.locals; stack = S.Empty}} in
            let outer = T.Label ({T.restart = None; continuation = T.Empty; saved = S.Empty}, T.No_labels) in
            (match Block.lower globals signature (G.Cons next) capacity 1000,
                Hmc_frame_value_pop.successor signature (D.List_type head_type),
                Hmc_frame_value_pop.transition activation next (V.Cons_pointer cursor) with
            | Some (Block.Cons fragment as lowered), Some next_signature, Some next_activation ->
              ghost_ (Block.corresponds_def globals signature (G.Cons next) capacity 1000 lowered;
                Hmc_wasm_value_pop.matches_def signature next capacity 1000 fragment;
                Hmc_wasm_relayout_geometry.size_represents (Codec.locals_size signature.G.locals) env_count ();
                Bounds.covers_def memory limit;
                Hmc_heap_step.request_def program configuration.Machine.state;
                Hmc_heap_allocating.request_def (G.Cons next) activation;
                Wasm_locals.get_def locals 0; Wasm_locals.get_def locals 1; Wasm_locals.get_def locals 2;
                Wasm_locals.get_def (S.Push (S.I32 cursor, S.Push (S.I32 limit, scratch))) 0;
                Wasm_locals.get_def (S.Push (S.I32 cursor, S.Push (S.I32 limit, scratch))) 1;
                Wasm_locals.get_def (S.Push (S.I32 limit, scratch)) 0;
                Wasm_control_branch_continue.labels_def T.Empty outer;
                Wasm_control_branch_target.valid_def 1 (Wasm_control_branch_continue.labels T.Empty outer);
                Wasm_control_branch_target.valid_def 0 outer);
              let entry = Entry.correct outer 1 env_count program globals (D.S (D.S D.Z)) signature next_signature activation next_activation frames
                (D.List_type head_type) head_type next context schema fragment capacity 1000 old_pc head activation.Frame.accumulator
                cursor 1 2 cells padding table heap limit slots frame_stop state 0 base transported.Hmc_wasm_frame_transport.bytes transported.Hmc_wasm_frame_transport.tail () in
              let code = T.Block (Block.emit lowered (Entry.locals 0 1 2 slots) 1, T.Empty) in
              (match T.run (Hmc_wasm_allocation_exit_fixture.fuel 10000) {T.code; labels = T.No_labels; state} with
              | T.Finished actual -> if actual <> entry.Entry.result.Guarded.state then failwith "reached Cons target state"
              | _ -> failwith "reached Cons did not finish");
              let prefix = [I.I32_const base; I.Local_set 0; I.I32_const cursor; I.Local_set 1; I.I32_const limit; I.Local_set 2;
                I.I64_const (Header.number 999); I.Local_set 3; I.I64_const (Header.number 999); I.Local_set 4;
                I.I64_const (Header.number 999); I.Local_set 5; I.I64_const (Header.number 999); I.Local_set 6] in
              {memory; expected = entry.Entry.result.Guarded.state.X.memory; prefix; code = T.flatten code C.Empty; cursor = (if enough then cursor + 32 else cursor)}
            | _ -> failwith "reached Cons lowering"))
        | _ -> failwith "reached initial coverage")
      | _ -> failwith "reached frame encoding")
      | _ -> failwith "reached Cons schema")
    | _ -> failwith "reached Cons lookup")
  | _ -> failwith "reached Cons state"
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
        | Some (Program.Keep (G.Cons _)) ->
          [fixture program globals configuration abstract 0 true ();
           fixture program globals configuration abstract 7 true ();
           fixture program globals configuration abstract 0 false ();
           fixture program globals configuration abstract 7 false ()]
        | _ -> [] in
      ghost_ (Inv.step program globals 8192 (D.S (D.S D.Z)) configuration abstract ());
      match Machine.step program globals 8192 (D.S (D.S D.Z)) configuration with
      | Machine.Exhausted _ -> failwith "reached source exhausted"
      | Machine.Advanced next -> fixtures @ collect program globals next (Abstract.step program abstract) (fuel - 1) ()
let cases source =
  let program = Hmc_wasm_global_fixture.build source in
  let input = Header.number 42 in
  match Hmc_heap_initialize.initialize program 1024 8192 input () with
  | Hmc_heap_initialize.Heap_exhausted _ -> failwith "reached initialization exhausted"
  | Hmc_heap_initialize.Initialized start ->
    ghost_ (bound_def (); Hmc_heap_initialize.correct_def program 1024 8192 input (Hmc_heap_initialize.Initialized start));
    collect program start.Hmc_heap_initialize.globals start.Hmc_heap_initialize.configuration (Abstract.initial program input) 200 ()
let fixtures () =
  let words = D.Lambda (D.CaseList (D.Cons (D.Bound D.Z, D.Cons (D.Word (Header.number 2), D.Nil)), D.Word (Header.number 0), D.Bound D.Z)) in
  let closures = D.Lambda (D.CaseList (D.Cons (D.Lambda (D.Bound (D.S D.Z)), D.Cons (D.Lambda (D.Bound D.Z), D.Nil)), D.Word (Header.number 0), D.Word (Header.number 1))) in
  let fixtures = cases words @ cases closures in
  if List.length fixtures <> 16 then failwith "reached Cons coverage";
  fixtures
