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
let rec filler n = if n = 0 then B.End else B.Byte (173, filler (n - 1))
let[@def] (bound @ total) (u : unit) : B.u32 = 8192
module Code = Hmc_wasm_closure_code
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
              let memory = Image.materialize table 1000 initial heap 8192 () in
              let rest = S.Push (S.I32 999, S.Empty) in
              let locals = S.Push (S.I32 address, rest) in
              let state = {X.memory; machine = {E.locals; stack = S.Empty}} in
              ghost_ (Wasm_locals.get_def locals 0; Wasm_locals.get_def locals 1; Wasm_locals.get_def rest 0);
              (match Hmc_closure_ir.lookup table id, Hmc_cfg_program.lookup program.Program.origin.Hmc_cfg_program.functions id with
              | Some entry, Some function_ ->
                if not (Codec.environment entry.Hmc_closure_ir.captured captures) then failwith "call capture shape" else
                let frame = Hmc_frame_call_entry.correct program heap address id captures entry function_ activation.Frame.accumulator Heap.Empty () in
                if Machine.invoke program heap (V.Closure_pointer address) activation.Frame.accumulator <> Some frame.Hmc_frame_call_entry.entered
                  || Codec.decode (Hmc_frame_call_entry.signature entry) function_.Hmc_cfg_program.start frame.Hmc_frame_call_entry.cells <> Some (frame.Hmc_frame_call_entry.entered, Heap.Empty)
                then failwith "call entry frame"
              | _ -> failwith "call function lookup");
              let result = Code.correct table heap state address id captures count 0 1 () in
              ghost_ (Hmc_u32_index.unique id index result.Code.code ());
              if result.Code.code <> index then failwith "call code index";
              (match X.run (Code.emit 0 1) state with
              | X.Done after ->
                if after.X.memory <> memory || after.X.machine.E.locals <> result.Code.locals
                  || after.X.machine.E.stack <> S.Empty then failwith "call closure code read"
              | _ -> failwith "call closure code trap");
              {memory; expected = memory; prefix = [I.I32_const address; I.Local_set 0; I.I32_const 999; I.Local_set 1];
                code = Code.emit 0 1; cursor = index})
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
  if List.length fixtures <> 4 then failwith "call code coverage";
  fixtures
