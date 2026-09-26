module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Table = Hmc_wasm_table_lower
module Dispatch = Hmc_wasm_dispatch_select
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module T = Wasm_control
let rec fuel n = if n = 0 then C.Zero else C.Succ (fuel (n - 1))
type fixture = {memory : B.bytes; base : B.u32; code : T.code; expected : (B.u32 * W.t) list option}
let fixture (base : B.u32) (pc : W.limb) =
  if base > 7 || pc > 3 then failwith "fixture bounds" else
  let index = Hmc_wasm_control_fixture.index in
  let signature = {G.locals = D.Empty_context; temporaries = G.Empty_temporaries; accumulator = None} in
  let source = G.Add ({G.signature; instruction = G.Jump (index 12)},
    G.Add ({G.signature; instruction = G.Jump (index 11)}, G.Add ({G.signature; instruction = G.Jump (index 10)}, G.Empty))) in
  if Table.lower Hmc_heap_machine.Empty_globals source 3 1 <> None then failwith "label bound accepted";
  if Table.lower Hmc_heap_machine.Empty_globals (G.Add ({G.signature; instruction = G.Return}, G.Empty)) 3 12 <> None then failwith "unsupported instruction accepted";
  match Table.lower Hmc_heap_machine.Empty_globals source 3 12 with
  | None -> failwith "table lowering"
  | Some table ->
    let cells = Heap.Cell (V.Closure_pointer 64, Heap.Cell (V.Word (Header.number 42), Heap.Cell (V.Nil, Heap.Empty))) in
    let tail = B.Byte (42, B.End) in
    let body = Wire.encode_cells cells tail in
    let frame = V.encode (V.Word (Header.number pc)) body in
    let memory = Hmc_wasm_frame_fixture.prefix base frame in
    let locals = S.Push (S.I32 base, S.Push (S.I64 (Header.number 37), S.Push (S.I64 (Header.number 99), S.Empty))) in
    let state = {X.memory; machine = {E.locals; stack = S.Empty}} in
    let code = Dispatch.emit table 0 in
    let logical_pc = index pc in
    ghost_ (Wasm_locals.get_def locals 0; Wasm_cell.cell memory base frame (V.Word (Header.number pc)) body ();
      Wasm_cell.payload_def memory base; V.payload_def (V.Word (Header.number pc)); Hmc_wasm_pc_update.offset_def ();
      Dispatch.correct table pc 0 base T.No_labels state ();
      Dispatch.selected_code table pc 0 T.No_labels state;
      Table.lookup_correct Hmc_heap_machine.Empty_globals source table 3 12 logical_pc pc ());
    (match T.run (Dispatch.cost table pc 0) {T.code; labels = T.No_labels; state} with
    | T.Running selected -> if selected <> Dispatch.selection table pc 0 T.No_labels state then failwith "selection mismatch"
    | _ -> failwith "selection did not reach block");
    if not (Wasm_nesting.structured code) then failwith "dispatcher structure" else
    ghost_ (Wasm_control_codec.roundtrip code ());
    if pc = 3 then (
      (match Table.lookup table pc with
      | Some _ -> failwith "unknown PC found"
      | None -> ghost_ (Dispatch.reject table pc 0 base T.No_labels state ()));

      if T.run (fuel 1000) {T.code; labels = T.No_labels; state} <> T.Trap then failwith "unknown PC did not trap";
      {memory; base; code; expected = None})
    else
      let expected_cells = Heap.Cell (V.Word (Header.number (10 + pc)), cells) in
      let after_frame = Wire.encode_cells expected_cells tail in
      let after = Wasm_memory_splice.replace memory base frame after_frame () in
      (match T.run (fuel 1000) {T.code; labels = T.No_labels; state} with
      | T.Finished actual -> if actual.X.memory <> after || actual.X.machine <> state.X.machine then failwith "dispatcher result"
      | _ -> failwith "dispatcher execution");
      {memory; base; code; expected = Some (Hmc_wasm_range_fixture.fields 0 expected_cells)}
let fixtures () = [fixture 0 0; fixture 7 0; fixture 0 1; fixture 7 1; fixture 0 2; fixture 7 2; fixture 0 3; fixture 7 3]
