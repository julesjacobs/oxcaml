module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Seg = Hmc_frame_segments
module Patch = Hmc_cell_patch
module Lower = Hmc_wasm_relayout
module Copy = Wasm_parallel_copy
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module R = Hmc_wasm_moves_fixture
let word (lo : W.limb) : W.t = {W.lo; hi = 0}
let rec fields (offset : B.u32) cells : (B.u32 * W.t) list = match cells with
  | Heap.Empty -> []
  | Heap.Cell (value, rest) -> if offset <= 4294967279 then
    (offset, V.tag value) :: (offset + 8, V.payload value) :: fields (offset + 16) rest else failwith "fixture offset"
let fixture (base : B.u32) (source : Lower.count) (destination : Lower.count) (count : Lower.count) =
  if base > 7 || source + count > 6 || destination + count > 6 then failwith "fixture bounds" else
  let source_index = Hmc_wasm_control_fixture.index source in
  let destination_index = Hmc_wasm_control_fixture.index destination in
  let length = Hmc_wasm_control_fixture.index count in
  let cells = Heap.Cell (V.Word (word 7), Heap.Cell (V.Closure_pointer 64, Heap.Cell (V.Word (word 42),
    Heap.Cell (V.Word (word 10), Heap.Cell (V.Boolean true, Heap.Cell (V.Nil, Heap.Cell (V.Word (word 99), Heap.Empty))))))) in
  match Seg.drop (D.S source_index) cells with
  | None -> failwith "source range"
  | Some start -> (match Seg.take length start with
    | None -> failwith "source length"
    | Some values -> (match Patch.write (D.S destination_index) values cells with
      | None -> failwith "destination range"
      | Some after_cells ->
        ghost_ (Hmc_cell_slice.take_length length start values ());
        let tail = B.Byte (42, B.End) in
        let before_frame = Wire.encode_cells cells tail in
        let after_frame = Wire.encode_cells after_cells tail in
        let memory = Hmc_wasm_frame_fixture.prefix base before_frame in
        let after = Wasm_memory_splice.replace memory base before_frame after_frame () in
        let plan = Lower.range source destination count Copy.End length () in
        ghost_ (Copy.apply_def Copy.End memory base;
          Hmc_wasm_range_read.correct values source count source_index cells start memory base before_frame tail ();
          Hmc_wasm_range_copy.correct values plan Copy.End source destination count destination_index
            memory memory after base before_frame after_frame cells after_cells tail ());
        let locals = S.Push (S.I32 base, S.Push (S.I64 (word 37), S.Push (S.I64 (word 99), S.Empty))) in
        let state = {X.memory; machine = {E.locals; stack = S.Push (S.I32 91, S.Empty)}} in
        ghost_ (Wasm_locals.get_def locals 0; Copy.correct plan 0 state base after ());
        let code : {code : Wasm_code.t | X.run code state === X.Done {X.memory = after; machine = state.X.machine}} @ immutable = Copy.emit plan 0 in
        (match X.run code state with
        | X.Done result -> if result.X.memory <> after || result.X.machine <> state.X.machine then failwith "range representation"
        | _ -> failwith "range execution");
        {R.memory; base; first = word 37; second = word 99; code; expected = fields 0 after_cells}))
let fixtures () =
  [fixture 0 2 3 2; fixture 7 2 3 2; fixture 0 3 2 2; fixture 7 3 2 2;
   fixture 0 2 2 2; fixture 7 2 2 2; fixture 0 6 6 0; fixture 7 6 6 0]
