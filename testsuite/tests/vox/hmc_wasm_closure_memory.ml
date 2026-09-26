module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Words = Hmc_wire_word_sequence
module Q = Wasm_word_sequence
module Raw = Wasm_words_read
module Cross = Wasm_cross_words
module Copy = Wasm_cross_copy
module Plan = Wasm_parallel_copy
module Lower = Hmc_wasm_closure_write
module Range = Hmc_wasm_range_copy
module Header = Hmc_wasm_header_update
module Index = Hmc_u32_index
module Bounds = Hmc_linear_bounds
module Bytes = Hmc_linear_bytes
module P = Hmc_linear_preservation
module Splice = Wasm_memory_splice
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
let[@def] (environment_position @ total) (u : unit) : Hmc_wasm_relayout.count = 2
type result = {memory : B.bytes; bytes : B.bytes; suffix : B.bytes}
let (correct @ total) : (captures : Heap.cells) @ immutable -> (count : Hmc_wasm_relayout.count) -> (fragment : Lower.fragment) @ immutable ->
    (state : X.state) @ immutable -> (frame_base : B.u32) -> (heap_base : B.u32) -> (limit : B.u32) ->
    (frame_local : B.u32) -> (heap_local : B.u32) ->
    {u : unit | Index.represents (Heap.length captures) count && 2 + count <= 268435452
      && fragment.Lower.bytes = 16 + 16 * count && heap_base + fragment.Lower.bytes <= limit
      && Hmc_wasm_relayout.range_is fragment.Lower.copies 2 0 (Heap.length captures) Plan.End
      && Range.reads state.X.memory frame_base (environment_position ()) captures && Bounds.covers state.X.memory limit
      && L.get state.X.machine.E.locals frame_local === Some (S.I32 frame_base)
      && L.get state.X.machine.E.locals heap_local === Some (S.I32 heap_base)} ->
    {out : result | X.run (Lower.emit fragment frame_local heap_local) state === X.Done {X.memory = out.memory; machine = state.X.machine}
      && Wire.decode (Wire.Closure_schema (Heap.length captures)) out.bytes === Some (Wire.Closure (fragment.Lower.code, captures), out.suffix)
      && V.length out.memory === V.length state.X.memory && P.equal_prefix heap_base state.X.memory out.memory
      && Bytes.drop out.memory heap_base === Some out.bytes
      && Bytes.drop state.X.memory (S.add32 heap_base fragment.Lower.bytes) === Some out.suffix
      && Bytes.drop out.memory (S.add32 heap_base fragment.Lower.bytes) === Some out.suffix
      && Bounds.covers out.memory limit} @ immutable =
  fun captures count fragment state frame_base heap_base limit frame_local heap_local premise ->
    ghost_ (environment_position_def ());
    let stop : B.u32 = heap_base + fragment.Lower.bytes in
    let _end = Bounds.suffix state.X.memory limit stop () in
    ghost_ (Bounds.covers_def state.X.memory stop);
    let before = Bounds.suffix state.X.memory stop heap_base () in
    let values = Words.words captures in
    let tag = Hmc_wasm_header_words.tag () in let code = Header.number fragment.Lower.code in
    let layout = Q.Word (tag, Q.Word (code, values)) in
    ghost_ (Bounds.distance_def heap_base stop;
      Words.size captures count (16 * count) ();
      Q.size_def layout fragment.Lower.bytes; Q.size_def (Q.Word (code, values)) (8 + 16 * count));
    let raw = Raw.read layout before fragment.Lower.bytes () in
    ghost_ (Cross.shape_def raw.Raw.words layout;
      match raw.Raw.words with Q.Word (_, rest) -> Cross.shape_def rest (Q.Word (code, values)) | _ -> ());
    match raw.Raw.words with
    | Q.Word (old_tag, Q.Word (old_code, old)) ->
      ghost_ (Cross.shape_def (Q.Word (old_code, old)) (Q.Word (code, values)));
      let suffix = raw.Raw.suffix in
      let prefix = Q.Word (old_tag, Q.Word (old_code, Q.End)) in
      let copied_words = Q.Word (old_tag, Q.Word (old_code, values)) in
      let copied_frame = Q.encode copied_words suffix in
      let copied = Splice.replace state.X.memory heap_base before copied_frame () in
      let tagged_words = Q.Word (tag, Q.Word (old_code, values)) in
      let tagged_frame = Q.encode tagged_words suffix in
      let tagged = Splice.replace state.X.memory heap_base before tagged_frame () in
      let bytes = Q.encode layout suffix in
      let memory = Splice.replace state.X.memory heap_base before bytes () in
      ghost_ (
        Q.size_def prefix 16; Q.size_def (Q.Word (old_code, Q.End)) 8; Q.size_def Q.End 0;
        Q.append_def prefix old; Q.append_def (Q.Word (old_code, Q.End)) old; Q.append_def Q.End old;
        Q.append_def prefix values; Q.append_def (Q.Word (old_code, Q.End)) values; Q.append_def Q.End values;
        Hmc_wasm_cross_range_words.correct captures fragment.Lower.copies Plan.End 2 0 count state.X.memory frame_base ();
        Range.tag_def 2; Range.tag_def 0; Copy.apply_def Plan.End state.X.memory frame_base heap_base;
        Cross.correct old values prefix fragment.Lower.copies Plan.End 48 16 state.X.memory state.X.memory copied frame_base heap_base before copied_frame suffix (16 * count) ();
        Splice.shared state.X.memory copied tagged heap_base ();
        Q.append_def Q.End copied_words; Q.append_def Q.End tagged_words;
        Wasm_sequence_update.at Q.End (Q.Word (old_code, values)) copied tagged copied_frame tagged_frame suffix old_tag tag heap_base 0 heap_base ();
        let tag_prefix = Q.Word (tag, Q.End) in
        Q.size_def tag_prefix 8;
        Q.append_def tag_prefix (Q.Word (old_code, values)); Q.append_def Q.End (Q.Word (old_code, values));
        Q.append_def tag_prefix (Q.Word (code, values)); Q.append_def Q.End (Q.Word (code, values));
        Splice.shared state.X.memory tagged memory heap_base ();
        Wasm_sequence_update.at tag_prefix values tagged memory tagged_frame bytes suffix old_code code heap_base 8 (heap_base + 8) ();
        Lower.zero_def (); Hmc_wasm_pc_update.offset_def ();
        Lower.correct fragment frame_local heap_local state frame_base heap_base copied tagged memory ();
        Copy.correct fragment.Lower.copies frame_local heap_local state frame_base heap_base copied ();
        Wasm_immediate_write.correct 0 heap_local tag {X.memory = copied; machine = state.X.machine} heap_base tagged ();
        Wasm_immediate_write.correct 8 heap_local code {X.memory = tagged; machine = state.X.machine} heap_base memory ();
        let cells = Heap.Cell (V.Word code, captures) in
        Words.words_def cells; V.tag_def (V.Word code); V.payload_def (V.Word code); Hmc_wasm_header_words.tag_def ();
        Words.recover cells bytes suffix (); Heap.length_def cells;
        Hmc_wasm_relayout_finish.closure fragment.Lower.code captures bytes suffix ();
        Q.prefix raw.Raw.words before before suffix suffix fragment.Lower.bytes ();
        Q.prefix layout bytes bytes suffix suffix fragment.Lower.bytes ();
        Wasm_cell.shift state.X.memory heap_base fragment.Lower.bytes stop before ();
        Wasm_cell.shift memory heap_base fragment.Lower.bytes stop bytes ();
        S.add32_def heap_base fragment.Lower.bytes;
        Bounds.same_length state.X.memory memory limit ());
      {memory; bytes; suffix}
    | _ -> unreachable_ ()
