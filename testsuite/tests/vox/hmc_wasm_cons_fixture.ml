module B = Wasm_u32
module W = Hmc_word64
module V = Hmc_tagged_cell
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module T = Wasm_control
module I = Wasm_instruction
module C = Wasm_code
module Header = Hmc_wasm_header_update
module Heap = Hmc_heap_objects
module R = Hmc_closure_semantics
module Shape = Hmc_frame_shape
module Write = Hmc_wasm_cons_write
module Advance = Hmc_wasm_allocation_advance
module Select = Hmc_wasm_allocation_select
type fixture = {memory : B.bytes; expected : B.bytes; base : B.u32; limit : B.u32; head : V.value; tail : V.value; cursor : B.u32; code : C.t}
let (word_object @ total) (heap : Heap.heap @ immutable) (word : W.t @ immutable) :
    {u : unit | Heap.object_valid Hmc_closure_ir.Empty (Heap.view heap) (Heap.Cons (V.Word word, V.Nil))} @ ghost = ghost_ (
  Heap.object_valid_def Hmc_closure_ir.Empty (Heap.view heap) (Heap.Cons (V.Word word, V.Nil));
  Heap.decode_object_def (Heap.view heap) (Heap.Cons (V.Word word, V.Nil));
  Heap.decode_value_def (Heap.view heap) (V.Word word); Heap.decode_value_def (Heap.view heap) V.Nil;
  Shape.value_def Hmc_closure_ir.Empty (R.V.Cons (R.V.Word word, R.V.Nil));
  Shape.valid_def Hmc_closure_ir.Empty (R.V.Cons (R.V.Word word, R.V.Nil));
  Shape.valid_def Hmc_closure_ir.Empty (R.V.Word word); Shape.valid_def Hmc_closure_ir.Empty R.V.Nil;
  Shape.first_class_def (R.V.Cons (R.V.Word word, R.V.Nil)); Shape.first_class_def (R.V.Word word); Shape.first_class_def R.V.Nil)
let fixture (base : B.u32) head tail enough =
  if base > 7 then failwith "cons fixture base" else
  let suffix = B.Byte (17, B.Byte (42, B.Byte (255, B.End))) in
  let old = Wasm_word_sequence.encode (Wasm_four_words.layout (Header.number 99) {W.lo = 4294967295; hi = 4294967295}
    (Header.number 88) (Header.number 77)) suffix in
  let memory = Hmc_wasm_frame_fixture.prefix base old in
  let limit : B.u32 = if enough then base + 32 else base + 31 in
  let local5 = S.Push (S.I64 (V.payload tail), S.Empty) in
  let local4 = S.Push (S.I64 (V.tag tail), local5) in
  let local3 = S.Push (S.I64 (V.payload head), local4) in
  let local2 = S.Push (S.I64 (V.tag head), local3) in
  let local1 = S.Push (S.I32 limit, local2) in
  let locals = S.Push (S.I32 base, local1) in
  let state = {X.memory; machine = {E.locals; stack = S.Empty}} in
  match Hmc_linear_bytes.drop memory limit with
  | None -> failwith "cons memory coverage"
  | Some _ ->
    ghost_ (Hmc_linear_bounds.covers_def memory limit;
      Wasm_locals.get_def locals 0;
      Wasm_locals.get_def locals 1;
      Wasm_locals.get_def local1 0;
      Wasm_locals.get_def locals 2;
      Wasm_locals.get_def local1 1;
      Wasm_locals.get_def local2 0;
      Wasm_locals.get_def locals 3;
      Wasm_locals.get_def local1 2;
      Wasm_locals.get_def local2 1;
      Wasm_locals.get_def local3 0;
      Wasm_locals.get_def locals 4;
      Wasm_locals.get_def local1 3;
      Wasm_locals.get_def local2 2;
      Wasm_locals.get_def local3 1;
      Wasm_locals.get_def local4 0;
      Wasm_locals.get_def locals 5;
      Wasm_locals.get_def local1 4;
      Wasm_locals.get_def local2 3;
      Wasm_locals.get_def local3 2;
      Wasm_locals.get_def local4 1;
      Wasm_locals.get_def local5 0);
    let expected = if enough then (
      let written = Hmc_wasm_cons_memory.correct head tail state base limit 0 2 3 4 5 () in
      let middle = {state with X.memory = written.Hmc_wasm_cons_memory.memory} in
      let _updated = Advance.correct 32 0 base limit middle () in
      written.Hmc_wasm_cons_memory.memory) else memory in
    (match head, tail with
    | V.Word word, V.Nil ->
      let heap = Heap.Empty_heap base in
      ghost_ (Heap.valid_def Hmc_closure_ir.Empty heap; Heap.used_def heap;
        Hmc_heap_image.related_def memory heap; word_object heap word);
      let source = Hmc_wasm_cons_conditional.correct Hmc_closure_ir.Empty heap head tail state base limit 0 1 2 3 4 5 () in
      if source.Hmc_wasm_cons_conditional.state.X.memory <> expected then failwith "source allocation memory"
    | _ -> ());
    let code = Hmc_wasm_cons_conditional.emit 0 1 2 3 4 5 in
    let cursor : B.u32 = if enough then base + 32 else base in
    (match T.run (Hmc_wasm_allocation_fixture.fuel 128) {T.code; labels = T.No_labels; state} with
    | T.Finished actual ->
      if actual.X.memory <> expected || actual.X.machine.E.stack <> S.Empty
        || Wasm_locals.get actual.X.machine.E.locals 0 <> Some (S.I32 cursor) then failwith "cons allocation conditional"
    | _ -> failwith "cons allocation did not finish");
    {memory; expected; base; limit; head; tail; cursor; code = T.flatten code C.Empty}
let fixtures () =
  let cases = [V.Word (Header.number 42), V.Nil; V.Boolean false, V.Cons_pointer 64;
    V.Closure_pointer 128, V.Cons_pointer 256; V.Word {W.lo = 4294967295; hi = 4294967295}, V.Nil] in
  List.concat_map (fun (head, tail) -> [fixture 0 head tail true; fixture 7 head tail true; fixture 0 head tail false; fixture 7 head tail false]) cases
