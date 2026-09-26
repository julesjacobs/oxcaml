module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module Copy = Wasm_parallel_copy
module Seg = Hmc_frame_segments
module R = Hmc_wasm_moves_fixture
let word (lo : W.limb) : W.t = {W.lo; hi = 0}
let fixture (base : B.u32) =
  if base > 4294967248 then failwith "fixture address" else
  let old = V.Word (word 42) in let value = V.Boolean true in
  let rest = Heap.Cell (value, Heap.Empty) in
  let before_cells = Heap.Cell (old, rest) in let after_cells = Heap.Cell (value, rest) in
  let tail = B.Byte (91, B.End) in
  let before_frame = Wire.encode_cells before_cells tail in
  let after_frame = Wire.encode_cells after_cells tail in
  let memory = Hmc_wasm_frame_fixture.prefix base before_frame in
  let after = Wasm_memory_splice.replace memory base before_frame after_frame () in
  ghost_ (Hmc_u32_index.represents_def (D.S D.Z) 1; Hmc_u32_index.represents_def D.Z 0;
    Heap.length_def Heap.Empty; Seg.append_def Heap.Empty before_cells; Seg.append_def Heap.Empty after_cells;
    Hmc_heap_simple.lookup_def before_cells (D.S D.Z); Hmc_heap_simple.lookup_def rest D.Z;
    Hmc_wasm_cells_read.correct memory base before_frame (Heap.length before_cells) before_cells tail (D.S D.Z) 1 16 24 value ();
    Copy.apply_def Copy.End memory base);
  let _middle = Hmc_wasm_cell_update.correct Heap.Empty rest old value 0 memory after base 0 8 before_frame after_frame tail memory 16 24 Copy.End () in
  let locals = S.Push (S.I32 base, S.Push (S.I64 (word 37), S.Push (S.I64 (word 99), S.Empty))) in
  let state = {X.memory; machine = {E.locals; stack = S.Push (S.I32 91, S.Empty)}} in
  let plan = Copy.Copy (16, 0, Copy.Copy (24, 8, Copy.End)) in
  ghost_ (Wasm_locals.get_def locals 0; Copy.correct plan 0 state base after ());
  let code = Copy.emit plan 0 in
  (match X.run code state with X.Done result ->
    if result.X.memory <> after || result.X.machine <> state.X.machine then failwith "cell-copy representation"
  | _ -> failwith "cell-copy execution");
  {R.memory; base; first = word 37; second = word 99; code; expected = [0, word 0; 8, word 1; 16, word 0; 24, word 1]}
let fixtures () = [fixture 0; fixture 7]
