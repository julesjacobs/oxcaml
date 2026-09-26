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
module Source = Hmc_wasm_list_full_entry
module Finished = Hmc_wasm_list_finish_invariant
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
let rec padding n = if n = 0 then Heap.Empty else Heap.Cell (V.Nil, padding (n - 1))
let fixture : (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (configuration : Machine.configuration) @ immutable -> (abstract : Hmc_cfg_semantics.state) @ immutable ->
    (base : B.u32) -> {u : unit | Inv.valid program globals (bound ()) configuration abstract} -> fixture =
  fun program globals configuration abstract base premise ->
  ghost_ (bound_def ());
  if base > 7 then failwith "reached list base" else
  let heap = configuration.Machine.heap in
  let table = program.Program.origin.Hmc_cfg_program.origin.Hmc_closure_program.table in
  ghost_ (Inv.valid_def program globals 8192 configuration abstract);
  match configuration.Machine.state with
  | State.Running (activation, frames) ->
    (match Program.lookup program.Program.code activation.Frame.pc, G.lookup program.Program.origin.Hmc_cfg_program.blocks activation.Frame.pc, activation.Frame.accumulator with
    | Some (Program.Keep (G.List_branch (empty, next))), Some block, V.Cons_pointer address ->
      let signature = block.G.signature in
      (match signature.G.accumulator, Hmc_heap_preservation.lookup_object heap address with
      | Some (D.List_type element), Some (Heap.Cons (head, tail)) ->
      if not (Codec.shape signature activation) then failwith "reached list frame shape" else
      let padding = padding 16 in
      let cells = Codec.encode signature activation padding () in
      (match Hmc_u32_index.encode 1000 (Heap.length cells), Hmc_u32_index.encode 1000 activation.Frame.pc with
      | Some capacity, Some old_pc ->
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
          let scratch = S.Push (S.I64 (Header.number 999), S.Push (S.I64 (Header.number 999), S.Push (S.I64 (Header.number 999), S.Push (S.I64 (Header.number 999), S.Empty)))) in
          let locals = S.Push (S.I32 base, S.Push (S.I32 999, S.Push (S.I32 cursor, scratch))) in
          if not (Capture.distinct slots && Capture.separate slots 0 && Capture.separate slots 1 && Capture.separate slots 2 && Capture.writable slots locals) then failwith "reached list scratch layout" else
          let state = {X.memory; machine = {E.locals; stack = S.Empty}} in
          (match Hmc_wasm_list_relayout.build signature next capacity 1000 with
          | Some fragment ->
            ghost_ (Wasm_locals.get_def locals 0; Wasm_locals.get_def locals 1; Wasm_locals.get_def locals 2;
              Wasm_locals.get_def (S.Push (S.I32 999, S.Push (S.I32 cursor, scratch))) 0;
              Wasm_locals.get_def (S.Push (S.I32 999, S.Push (S.I32 cursor, scratch))) 1;
              Wasm_locals.get_def (S.Push (S.I32 cursor, scratch)) 0);
            ghost_ (Wasm_locals.can_set_def locals 1 (S.I32 address); S.same_type_def (S.I32 999) (S.I32 address));
            let result = Source.correct program globals 8192 (D.S (D.S D.Z)) frames empty signature element activation next head tail fragment capacity 1000 old_pc cells padding
              table heap address 1 2 frame_stop state 0 base slots transported.Hmc_wasm_frame_transport.bytes transported.Hmc_wasm_frame_transport.tail () in
            let code = Source.emit fragment 0 1 slots in
            (match X.run code state with
            | X.Done actual -> if actual.X.memory <> result.Full.frame.Finished.memory || actual.X.machine.E.locals <> result.Full.locals then failwith "reached list target state"
            | _ -> failwith "reached list did not finish");
            let prefix = [I.I32_const base; I.Local_set 0; I.I32_const 999; I.Local_set 1; I.I32_const cursor; I.Local_set 2;
              I.I64_const (Header.number 999); I.Local_set 3; I.I64_const (Header.number 999); I.Local_set 4;
              I.I64_const (Header.number 999); I.Local_set 5; I.I64_const (Header.number 999); I.Local_set 6] in
            {memory; expected = result.Full.frame.Finished.memory; prefix; code; cursor}
          | _ -> failwith "reached list lowering")
        | _ -> failwith "reached list initial coverage")
      | _ -> failwith "reached list frame encoding")
      | _ -> failwith "reached list schema")
    | _ -> failwith "reached list lookup")
  | _ -> failwith "reached list state"
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
      let fixtures = match Program.lookup program.Program.code activation.Frame.pc, activation.Frame.accumulator with
        | Some (Program.Keep (G.List_branch _)), V.Cons_pointer _ ->
          [fixture program globals configuration abstract 0 ();
           fixture program globals configuration abstract 7 ()]
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
  if List.length fixtures <> 4 then failwith "reached list coverage";
  fixtures
