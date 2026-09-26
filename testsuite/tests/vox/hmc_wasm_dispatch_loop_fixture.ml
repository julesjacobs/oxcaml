module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Table = Hmc_wasm_table_lower
module Block = Hmc_wasm_block_lower
module Loop = Hmc_wasm_dispatch_loop
module Trace = Hmc_wasm_dispatch_trace
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module M = Wasm_memory
module T = Wasm_control
let[@def] (zero @ total) (unit : unit) : B.u32 = 0
let rec build : int -> (table : Table.table) @ immutable -> (base : B.u32) -> (state : X.state) @ immutable ->
    {trace : Trace.trace | Trace.valid trace table (zero ()) base state} @ immutable = fun count table base state ->
  ghost_ (zero_def ());
  if count = 0 then (ghost_ (Trace.valid_def Trace.Stop table 0 base state); Trace.Stop) else
  match state.X.machine.E.stack, Wasm_locals.get state.X.machine.E.locals 0, M.load state.X.memory base (Hmc_wasm_pc_update.offset ()) M.W64 with
  | S.Empty, Some (S.I32 address), Some (S.I64 word) ->
    if address <> base || word.W.hi <> 0 then failwith "trace input" else
    (match Table.lookup table word.W.lo with
    | None -> failwith "trace PC"
    | Some fragment -> (match X.run (Block.emit fragment 0) state with
      | X.Done after -> (match after.X.machine.E.stack with
        | S.Empty ->
          let rest = build (count - 1) table base after in
          let trace = Trace.More (word.W.lo, fragment, after, rest) in
          ghost_ (Header.number_def word.W.lo; Trace.valid_def trace table 0 base state); trace
        | _ -> failwith "trace stack")
      | _ -> failwith "trace block"))
  | _ -> failwith "trace header"
type fixture = {memory : B.bytes; base : B.u32; code : T.code; final_pc : W.limb}
let fixture (base : B.u32) (pc : W.limb) =
  if base > 7 || pc > 1 then failwith "fixture bounds" else
  let index = Hmc_wasm_control_fixture.index in
  let signature = {G.locals = D.Empty_context; temporaries = G.Empty_temporaries; accumulator = None} in
  let source = G.Add ({G.signature; instruction = G.Jump (index 3)},
    G.Add ({G.signature; instruction = G.Jump (index 2)}, G.Add ({G.signature; instruction = G.Jump (index 1)}, G.Empty))) in
  match Table.lower Hmc_heap_machine.Empty_globals source 3 3 with
  | None -> failwith "loop table"
  | Some table ->
    let cells = Heap.Cell (V.Closure_pointer 64, Heap.Cell (V.Word (Header.number 42), Heap.Cell (V.Nil, Heap.Empty))) in
    let tail = B.Byte (42, B.End) in
    let frame = Wire.encode_cells (Heap.Cell (V.Word (Header.number pc), cells)) tail in
    let memory = Hmc_wasm_frame_fixture.prefix base frame in
    let locals = S.Push (S.I32 base, S.Push (S.I64 (Header.number 37), S.Push (S.I64 (Header.number 99), S.Empty))) in
    let state = {X.memory; machine = {E.locals; stack = S.Empty}} in
    let trace = build (3 - pc) table base state in
    let last = Trace.last trace state in
    let code = Loop.emit table 0 in
    ghost_ (zero_def (); Loop.enter table 0 T.No_labels state (); Trace.correct trace table 0 base T.No_labels state ());
    (match T.run (Trace.cost trace table 0) (Loop.configuration table 0 T.No_labels state) with
    | T.Running actual -> if actual <> Loop.configuration table 0 T.No_labels last then failwith "loop trace"
    | _ -> failwith "loop prefix terminated");
    let after_frame = Wire.encode_cells (Heap.Cell (V.Word (Header.number 3), cells)) tail in
    let after = Wasm_memory_splice.replace memory base frame after_frame () in
    if last.X.memory <> after || last.X.machine <> state.X.machine then failwith "loop result";
    if T.run (Hmc_wasm_dispatch_fixture.fuel 5000) {T.code; labels = T.No_labels; state} <> T.Trap then failwith "loop unknown PC";
    if not (Wasm_nesting.structured code) then failwith "loop structure" else
    ghost_ (Wasm_control_codec.roundtrip code ());
    {memory; base; code; final_pc = 3}
let fixtures () = [fixture 0 0; fixture 7 0; fixture 0 1; fixture 7 1]
