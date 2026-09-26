module B = Wasm_u32
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Store = Hmc_wasm_list_store
module D = Hm_declarative
module G = Hmc_cfg_ir
module Frame = Hmc_heap_frame
module Codec = Hmc_pointer_frame_codec
module Seg = Hmc_frame_segments
module Index = Hmc_u32_index
module Model = Hmc_frame_list_branch
module Lower = Hmc_wasm_relayout
module Capture = Hmc_wasm_list_capture
module Finish = Hmc_wasm_list_finish
module Write = Wasm_frame_write
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module I = Wasm_instruction
module C = Wasm_code
type fixture = {memory : B.bytes; expected : B.bytes; code : C.t}
let fixture (base : B.u32) closure =
  if base > 7 then failwith "list store base" else
  let context1 = D.Binding (D.Forall (D.Z, D.Word64), D.Empty_context) in
  let context2 = D.Binding (D.Forall (D.Z, D.Word64), context1) in
  let element = if closure then D.Function (D.Word64, D.Word64) else D.Word64 in
  let signature = {G.locals = context2; temporaries = G.Environment (context1, G.Empty_temporaries); accumulator = Some (D.List_type element)} in
  let current = V.Closure_pointer 128 in let accumulator = V.Cons_pointer 256 in
  let head = if closure then V.Closure_pointer 512 else V.Word {Hmc_word64.lo = 42; hi = 37} in
  let tail = if closure then V.Nil else V.Cons_pointer 768 in
  let word n = V.Word (Header.number n) in
  let env = Heap.Cell (word 10, Heap.Cell (word 20, Heap.Empty)) in
  let old = Heap.Cell (word 90, Heap.Empty) in
  let old_padding = Heap.Cell (word 1, Heap.Cell (word 2, Heap.Cell (word 3, Heap.Cell (word 4, Heap.Cell (word 5, Heap.Cell (word 6, Heap.Empty)))))) in
  let activation = {Frame.pc = Hmc_wasm_control_fixture.index 7; current; accumulator; env; temporaries = Frame.Environment (old, Frame.Empty)} in
  let next = Hmc_wasm_control_fixture.index 11 in
  if not (Codec.shape signature activation) then failwith "list frame shape" else
  let source_cells = Codec.encode signature activation old_padding () in
  ghost_ (Codec.decode_def signature activation.Frame.pc source_cells);
  match source_cells with
  | Heap.Cell (_, Heap.Cell (_, Heap.Cell (old_head, Heap.Cell (old_tail, remaining)))) ->
  let body = Heap.Cell (old_head, Heap.Cell (old_tail, remaining)) in
  (match Seg.take (Codec.locals_size signature.G.locals) body, Seg.drop (Codec.locals_size signature.G.locals) body with
  | Some env, Some old_start -> (match Seg.take (Codec.temporaries_size signature.G.temporaries) old_start with
  | Some old ->
  let values = Seg.append (Seg.append env env) (Seg.append old Heap.Empty) in
  (match Seg.drop (Heap.length values) remaining, Index.encode 11 (Codec.locals_size signature.G.locals), Index.encode 11 (Codec.temporaries_size signature.G.temporaries),
      Hmc_wasm_list_relayout.build signature next 11 11 with
  | Some padding, Some env_count, Some old_count, Some fragment ->
  ghost_ (Hmc_wasm_list_relayout.matches_def signature next 11 11 fragment; Index.unique next fragment.Lower.pc 11 ());
  let rest = Seg.append env (Seg.append env (Seg.append old padding)) in
  let suffix = B.Byte (173, B.Byte (29, B.End)) in
  let before_frame = Wire.encode_cells (Store.cells (V.Word (Header.number 7)) current accumulator old_head old_tail remaining) suffix in
  let after_frame = Wire.encode_cells (Store.cells (V.Word (Header.number 11)) current accumulator head tail rest) suffix in
  let memory = Hmc_wasm_frame_fixture.prefix base before_frame in
  let expected = Wasm_memory_splice.replace memory base before_frame after_frame () in
  let slots = {Capture.head_tag = 1; head_payload = 2; tail_tag = 3; tail_payload = 4} in
  let locals = S.Push (S.I32 base, S.Push (S.I64 (V.tag head), S.Push (S.I64 (V.payload head),
    S.Push (S.I64 (V.tag tail), S.Push (S.I64 (V.payload tail), S.Empty))))) in
  ghost_ (Wasm_locals.get_def locals 0; Wasm_locals.get_def locals 1; Wasm_locals.get_def locals 2;
    Wasm_locals.get_def locals 3; Wasm_locals.get_def locals 4);
  ghost_ (Wasm_locals.get_def (S.Push (S.I64 (V.tag head), S.Push (S.I64 (V.payload head), S.Push (S.I64 (V.tag tail), S.Push (S.I64 (V.payload tail), S.Empty))))) 0;
    Wasm_locals.get_def (S.Push (S.I64 (V.tag head), S.Push (S.I64 (V.payload head), S.Push (S.I64 (V.tag tail), S.Push (S.I64 (V.payload tail), S.Empty))))) 1;
    Wasm_locals.get_def (S.Push (S.I64 (V.tag head), S.Push (S.I64 (V.payload head), S.Push (S.I64 (V.tag tail), S.Push (S.I64 (V.payload tail), S.Empty))))) 2;
    Wasm_locals.get_def (S.Push (S.I64 (V.tag head), S.Push (S.I64 (V.payload head), S.Push (S.I64 (V.tag tail), S.Push (S.I64 (V.payload tail), S.Empty))))) 3;
    Wasm_locals.get_def (S.Push (S.I64 (V.payload head), S.Push (S.I64 (V.tag tail), S.Push (S.I64 (V.payload tail), S.Empty)))) 0;
    Wasm_locals.get_def (S.Push (S.I64 (V.payload head), S.Push (S.I64 (V.tag tail), S.Push (S.I64 (V.payload tail), S.Empty)))) 1;
    Wasm_locals.get_def (S.Push (S.I64 (V.payload head), S.Push (S.I64 (V.tag tail), S.Push (S.I64 (V.payload tail), S.Empty)))) 2;
    Wasm_locals.get_def (S.Push (S.I64 (V.tag tail), S.Push (S.I64 (V.payload tail), S.Empty))) 0;
    Wasm_locals.get_def (S.Push (S.I64 (V.tag tail), S.Push (S.I64 (V.payload tail), S.Empty))) 1;
    Wasm_locals.get_def (S.Push (S.I64 (V.payload tail), S.Empty)) 0);
  let state = {X.memory; machine = {E.locals; stack = S.Empty}} in
  ghost_ (Hmc_wasm_list_finish_step.correct signature element activation next head tail fragment 11 11 env_count old_count 7
    old_head old_tail remaining env old_start old old_padding padding state expected 0 base slots before_frame after_frame suffix ());
  (match Index.encode 11 (Heap.length source_cells) with
  | Some capacity when capacity = 11 ->
    ghost_ (Store.cells_def (V.Word (Header.number 7)) current accumulator old_head old_tail remaining;
      Heap.length_def (Heap.Cell (V.Word (Header.number 7), source_cells)));
    let result = Hmc_wasm_list_finish_invariant.correct signature element activation next head tail fragment 11 11 7 source_cells old_padding
      state 0 base slots before_frame suffix () in
    if result.Hmc_wasm_list_finish_invariant.memory <> expected then failwith "list frame invariant memory"
  | _ -> failwith "list frame capacity");
  let stores = Finish.emit fragment 0 slots in
  (match X.run stores state with
  | X.Done out -> if out.X.memory <> expected || out.X.machine <> state.X.machine then failwith "list store representation"
  | _ -> failwith "list store execution");
  let prefix = C.Next (I.I32_const base, C.Next (I.Local_set 0,
    C.Next (I.I64_const (V.tag head), C.Next (I.Local_set 1,
    C.Next (I.I64_const (V.payload head), C.Next (I.Local_set 2,
    C.Next (I.I64_const (V.tag tail), C.Next (I.Local_set 3,
    C.Next (I.I64_const (V.payload tail), C.Next (I.Local_set 4, C.Empty)))))))))) in
  {memory; expected; code = E.append prefix (E.append stores (C.Next (I.I32_const 0, C.Empty)))}
  | _ -> failwith "list frame bounds")
  | _ -> failwith "list temporary slice")
  | _ -> failwith "list environment slice")
  | _ -> failwith "list frame header"

let fixtures () = [fixture 0 false; fixture 7 false; fixture 0 true; fixture 7 true]
