module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Simple = Hmc_heap_simple
module Index = Hmc_u32_index
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module R = Hmc_wasm_moves_fixture
let word (lo : W.limb) (hi : W.limb) : W.t = {W.lo; hi}
let fixture (base : B.u32) operation left right =
  if base > 7 then failwith "fixture base" else
  let current = V.Closure_pointer 64 in
  let body = Heap.Cell (V.Word left, Heap.Cell (V.Nil, Heap.Empty)) in
  let cells = Heap.Cell (current, Heap.Cell (V.Word right, body)) in
  let value = Simple.primitive operation left right in
  let after_cells = Heap.Cell (current, Heap.Cell (value, body)) in
  let full = Heap.Cell (V.Word (Header.number 7), cells) in
  let after_full = Heap.Cell (V.Word (Header.number 7), after_cells) in
  let tail = B.Byte (42, B.End) in
  let before_frame = Wire.encode_cells full tail in
  let after_frame = Wire.encode_cells after_full tail in
  let memory = Hmc_wasm_frame_fixture.prefix base before_frame in
  let after = Wasm_memory_splice.replace memory base before_frame after_frame () in
  let locals = S.Push (S.I32 base, S.Push (S.I64 (Header.number 37), S.Push (S.I64 (Header.number 99), S.Empty))) in
  let state = {X.memory; machine = {E.locals; stack = S.Push (S.I32 91, S.Empty)}} in
  ghost_ (
    Heap.length_def full; Heap.length_def cells; Heap.length_def (Heap.Cell (V.Word right, body));
    Heap.length_def after_full; Heap.length_def after_cells; Heap.length_def (Heap.Cell (value, body));
    Hmc_wasm_relayout_finish.closure 7 cells before_frame tail ();
    Hmc_wasm_relayout_finish.closure 7 after_cells after_frame tail ();
    Index.represents_def D.Z 0; Index.represents_def (D.S D.Z) 1;
    Index.represents_def (D.S (D.S D.Z)) 2; Index.represents_def (D.S (D.S (D.S D.Z))) 3;
    Simple.lookup_def full (D.S (D.S D.Z)); Simple.lookup_def cells (D.S D.Z); Simple.lookup_def (Heap.Cell (V.Word right, body)) D.Z;
    Simple.lookup_def full (D.S (D.S (D.S D.Z))); Simple.lookup_def cells (D.S (D.S D.Z));
    Simple.lookup_def (Heap.Cell (V.Word right, body)) (D.S D.Z); Simple.lookup_def body D.Z;
    Hmc_wasm_cells_read.correct memory base before_frame (Heap.length full) full tail (D.S (D.S D.Z)) 2 32 40 (V.Word right) ();
    Hmc_wasm_cells_read.correct memory base before_frame (Heap.length full) full tail (D.S (D.S (D.S D.Z))) 3 48 56 (V.Word left) ();
    V.payload_def (V.Word left); V.payload_def (V.Word right); Hmc_wasm_primitive_payload.offset_def (); Wasm_locals.get_def locals 0;
    Hmc_wasm_primitive_update.correct operation 56 left right 7 current body (Heap.length body) state after 0 base before_frame after_frame tail ());
  let code : {code : Wasm_code.t | X.run code state === X.Done {X.memory = after; machine = state.X.machine}} @ immutable =
    Hmc_wasm_primitive_write.emit operation 56 0 in
  (match X.run code state with
  | X.Done actual -> if actual.X.memory <> after || actual.X.machine <> state.X.machine then failwith "primitive frame update"
  | _ -> failwith "primitive execution");
  {R.memory; base; first = Header.number 37; second = Header.number 99; code; expected = Hmc_wasm_range_fixture.fields 0 after_full}
let fixtures () =
  let max = word 4294967295 4294967295 in
  let zero = word 0 0 in let one = word 1 0 in
  let cases = [D.Add, max, one; D.Add, word 4294967295 0, one; D.Add, word 0 2147483648, word 0 2147483648;
    D.Subtract, zero, one; D.Subtract, word 0 1, one; D.Subtract, word 37 99, word 37 99;
    D.Equal_word, max, max; D.Equal_word, word 0 1, zero;
    D.Unsigned_less, zero, max; D.Unsigned_less, max, zero; D.Unsigned_less, word 4294967295 0, word 0 1;
    D.Unsigned_less, max, max] in
  List.concat_map (fun (operation, left, right) -> [fixture 0 operation left right; fixture 7 operation left right]) cases
