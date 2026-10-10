module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module Heap = Hmc_heap_objects
module V = Hmc_tagged_cell
module Wire = Hmc_heap_wire
module Seg = Hmc_frame_segments
module Patch = Hmc_cell_patch
module Codec = Hmc_pointer_frame_codec
module Index = Hmc_u32_index
module Lower = Hmc_wasm_relayout
module Copy = Wasm_parallel_copy
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module R = Hmc_wasm_moves_fixture
let word (lo : W.limb) : W.t = {W.lo; hi = 0}
let fixture (base : B.u32) restore =
  if base > 7 then failwith "fixture base" else
  let context1 = D.Binding (D.Forall (D.Z, D.Word64), D.Empty_context) in
  let context2 = D.Binding (D.Forall (D.Z, D.Word64), context1) in
  let schema = G.Value (context1, D.Word64, G.Empty_temporaries) in
  let signature = {G.locals = context2; temporaries = G.Environment (context1, schema); accumulator = Some D.Word64} in
  let next = Hmc_wasm_control_fixture.index 11 in
  let instruction = if restore then G.Restore next else G.Save_environment next in
  let env_size = Codec.locals_size context2 in
  let saved_size = Codec.locals_size context1 in
  let first_size = if restore then saved_size else env_size in
  let second_size = if restore then Codec.temporaries_size schema else Codec.temporaries_size signature.G.temporaries in
  let first_index = if restore then D.S (D.S env_size) else D.S (D.S D.Z) in
  let second_index = if restore then D.S (D.S (D.add env_size saved_size)) else D.S (D.S env_size) in
  let destination = if restore then D.S (D.S D.Z) else D.S (D.S env_size) in
  let cells = Heap.Cell (V.Word (word 7), Heap.Cell (V.Closure_pointer 64, Heap.Cell (V.Word (word 42),
    Heap.Cell (V.Word (word 10), Heap.Cell (V.Word (word 20), Heap.Cell (V.Word (word 90),
    Heap.Cell (V.Word (word 30), Heap.Cell (V.Word (word 70),
    Heap.Cell (V.Word (word 999), Heap.Cell (V.Word (word 999), Heap.Cell (V.Word (word 999),
    Heap.Cell (V.Word (word 999), Heap.Empty)))))))))))) in
  match Index.encode 11 env_size, Index.encode 11 first_size, Index.encode 11 second_size with
  | Some env_count, Some first_count, Some second_count ->
    (match Seg.drop (D.S first_index) cells, Seg.drop (D.S second_index) cells with
    | Some first_start, Some second_start -> (match Seg.take first_size first_start, Seg.take second_size second_start with
      | Some first, Some second -> (match Patch.write (D.S destination) (Seg.append first second) cells with
        | None -> failwith "fixture destination"
        | Some after_cells ->
          ghost_ (Hmc_cell_slice.take_length first_size first_start first ();
            Hmc_cell_slice.take_length second_size second_start second ());
          let tail = B.Byte (42, B.End) in
          let before_frame = Wire.encode_cells cells tail in
          let after_frame = Wire.encode_cells after_cells tail in
          let memory = Hmc_wasm_frame_fixture.prefix base before_frame in
          let after = Wasm_memory_splice.replace memory base before_frame after_frame () in
          (match Lower.build signature instruction 11 11 with
          | None -> failwith "fixture builder"
          | Some fragment ->
            ghost_ (if restore then
              Hmc_wasm_relayout_restore.correct signature next context1 schema fragment 11 11 first second env_count first_count second_count
                first_start second_start memory after base before_frame after_frame cells after_cells tail ()
            else Hmc_wasm_relayout_save.correct signature next fragment 11 11 first second first_count second_count
                first_start second_start memory after base before_frame after_frame cells after_cells tail ());
            let locals = S.Push (S.I32 base, S.Push (S.I64 (word 37), S.Push (S.I64 (word 99), S.Empty))) in
            let state = {X.memory; machine = {E.locals; stack = S.Push (S.I32 91, S.Empty)}} in
            ghost_ (Wasm_locals.get_def locals 0; Copy.correct fragment.Lower.copies 0 state base after ());
            let code : {code : Wasm_code.t | X.run code state === X.Done {X.memory = after; machine = state.X.machine}} @ immutable = Copy.emit fragment.Lower.copies 0 in
            (match X.run code state with
            | X.Done result -> if result.X.memory <> after || result.X.machine <> state.X.machine then failwith "builder representation"
            | _ -> failwith "builder execution");
            {R.memory; base; first = word 37; second = word 99; code; expected = Hmc_wasm_range_fixture.fields 0 after_cells}))
      | _ -> failwith "fixture lengths")
    | _ -> failwith "fixture sources")
  | _ -> failwith "fixture counts"
let fixtures () = [fixture 0 false; fixture 7 false; fixture 0 true; fixture 7 true]
