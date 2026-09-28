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
let fixture : (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (configuration : Machine.configuration) @ immutable -> (abstract : Hmc_cfg_semantics.state) @ immutable ->
    (base : B.u32) -> bool -> {u : unit | Inv.valid program globals (bound ()) configuration abstract} -> fixture =
  fun program globals configuration abstract base enough premise ->
  ghost_ (bound_def ());
  if base > 7 then failwith "reached closure base" else
  let heap = configuration.Machine.heap in
  let table = program.Program.origin.Hmc_cfg_program.origin.Hmc_closure_program.table in
  ghost_ (Inv.valid_def program globals 8192 configuration abstract;
    Inv.request_valid program heap configuration.Machine.state abstract ());
  match configuration.Machine.state with
  | State.Running (activation, frames) ->
    (match Program.lookup program.Program.code activation.Frame.pc, G.lookup program.Program.origin.Hmc_cfg_program.blocks activation.Frame.pc with
    | Some (Program.Keep (G.Load (G.Closure id, type_, typing, next))), Some block ->
      let signature = block.G.signature in
      let next_signature = {signature with G.accumulator = Some type_} in
      if not (Codec.shape signature activation) then failwith "reached closure frame shape" else
      let padding = Heap.Cell (V.Nil, Heap.Cell (V.Nil, Heap.Empty)) in
      let cells = Codec.encode signature activation padding () in
      (match Hmc_u32_index.encode 1000 (Heap.length cells), Hmc_u32_index.encode 1000 activation.Frame.pc,
          Hmc_u32_index.encode 1000 next with
      | Some capacity, Some old_pc, Some pc ->
        (match Block.lower globals signature (G.Load (G.Closure id, type_, typing, next)) capacity 1000 with
        | None -> failwith "reached closure lowering"
        | Some (Block.Closure lowered as block_code) ->
        let fragment = lowered.Hmc_wasm_closure_lower.object_ in
        ghost_ (Block.corresponds_def globals signature (G.Load (G.Closure id, type_, typing, next)) capacity 1000 block_code;
          Hmc_wasm_closure_lower.matches_def signature id next capacity 1000 lowered);
        let frame_stop : B.u32 = base + 16 + 16 * capacity in
        let cursor = Heap.used heap in
        if frame_stop > 1024 || cursor > 7000 || frame_stop > cursor || fragment.Write.bytes < 16 || fragment.Write.bytes > 1024 then failwith "reached closure memory layout" else
        if not (above heap frame_stop && Image.encodable 1000 heap) then failwith "reached closure heap encoding" else
        let limit : B.u32 = cursor + fragment.Write.bytes - (if enough then 0 else 1) in
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
          | None -> failwith "reached closure memory coverage"
          | Some _ ->
            let locals2 = S.Push (S.I32 limit, S.Empty) in
            let locals1 = S.Push (S.I32 cursor, locals2) in
            let locals = S.Push (S.I32 base, locals1) in
            let state = {X.memory; machine = {E.locals; stack = S.Empty}} in
            ghost_ (Bounds.covers_def memory limit;
              Hmc_heap_step.request_def program configuration.Machine.state;
              Hmc_heap_allocating.request_def (G.Load (G.Closure id, type_, typing, next)) activation;
              Wasm_locals.get_def locals 0; Wasm_locals.get_def locals 1; Wasm_locals.get_def locals 2;
              Wasm_locals.get_def locals1 0; Wasm_locals.get_def locals1 1; Wasm_locals.get_def locals2 0);
            let result = Guarded.correct program globals (D.S (D.S D.Z)) table heap signature next_signature activation frames id type_ typing next cells padding old_pc pc capacity 1000 fragment state base frame_stop cursor limit 0 1 2
              transported.Hmc_wasm_frame_transport.bytes transported.Hmc_wasm_frame_transport.tail () in
            let expected_source = Machine.step program globals limit (D.S (D.S D.Z)) configuration in
            if expected_source <> result.Guarded.source then failwith "reached closure source result";
            let outer = T.Label ({T.restart = None; continuation = T.Empty; saved = S.Empty}, T.No_labels) in
            let branch_labels = Wasm_control_branch_continue.labels T.Empty outer in
            (match T.branch 1 branch_labels state with
            | T.Running exhausted ->
              let continued = Hmc_wasm_closure_block_entry.correct outer 1 exhausted program globals (D.S (D.S D.Z)) table heap signature next_signature activation frames id type_ typing next cells padding old_pc pc capacity 1000 fragment state base frame_stop cursor limit 0 1 2
                transported.Hmc_wasm_frame_transport.bytes transported.Hmc_wasm_frame_transport.tail () in
              if continued.Guarded.state <> result.Guarded.state then failwith "reached closure continuation state"
            | _ -> failwith "reached closure exhaustion target");
            let slots = {Block.frame = 0; heap = 1; limit = 2; object_ = 0; scratch = {Capture.head_tag = 0; head_payload = 0; tail_tag = 0; tail_payload = 0}} in
            let code = T.Block (Block.emit block_code slots 1, T.Empty) in
            (match T.run (Hmc_wasm_allocation_exit_fixture.fuel 10000) {T.code; labels = T.No_labels; state} with
            | T.Finished actual -> if actual <> result.Guarded.state then failwith "reached closure target state"
            | _ -> failwith "reached closure did not finish");
            let expected = match expected_source with
              | Machine.Exhausted Machine.Heap -> memory
              | Machine.Advanced after ->
                (match after.Machine.state with
                | State.Running (next_activation, _) ->
                  if not (Codec.shape next_signature next_activation) then failwith "reached closure successor shape" else
                  let next_cells = Codec.encode next_signature next_activation padding () in
                  let next_bytes = Wire.encode_cells (Heap.Cell (V.Word (Header.number pc), next_cells)) suffix in
                  let next_initial = Hmc_wasm_frame_fixture.prefix base next_bytes in
                  if not (Image.encodable 1000 after.Machine.heap) then failwith "reached closure successor heap encoding" else
                  (match L.drop next_initial 8192 with
                  | None -> failwith "reached closure successor coverage"
                  | Some _ ->
                    ghost_ (Bounds.covers_def next_initial 8192);
                    Hmc_heap_image.materialize table 1000 next_initial after.Machine.heap 8192 ())
                | _ -> failwith "reached closure successor")
              | _ -> failwith "reached closure unexpected exhaustion" in
            if expected <> result.Guarded.state.X.memory then failwith "reached closure independently encoded memory";
            {memory; expected; prefix = [I.I32_const base; I.Local_set 0; I.I32_const cursor; I.Local_set 1; I.I32_const limit; I.Local_set 2];
              code = T.flatten code C.Empty; cursor = (if enough then cursor + fragment.Write.bytes else cursor)})
        | _ -> failwith "reached closure initial coverage")
        | _ -> failwith "reached closure lowering kind")
      | _ -> failwith "reached closure frame encoding")
    | _ -> failwith "reached closure lookup")
  | _ -> failwith "reached closure state"
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
        | Some (Program.Keep (G.Load (G.Closure _, _, _, _))) ->
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
  let signature = {G.locals = D.Empty_context; temporaries = G.Empty_temporaries; accumulator = None} in
  let build = Hmc_wasm_closure_lower.build in
  if build signature D.Z D.Z 1 0 <> None
    || build signature (D.S D.Z) D.Z 2 0 <> None
    || build signature D.Z (D.S D.Z) 2 0 <> None then failwith "invalid closure lowering accepted";
  if build signature D.Z D.Z 2 0 = None then failwith "empty closure lowering rejected";
  let source = D.Lambda (D.CaseList (D.Cons (D.Lambda (D.Bound (D.S D.Z)), D.Cons (D.Lambda (D.Bound D.Z), D.Nil)), D.Word (Header.number 0), D.Word (Header.number 1))) in
  let fixtures = cases source in
  if List.length fixtures <> 8 then failwith "reached closure coverage";
  fixtures
