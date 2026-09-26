module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Simple = Hmc_heap_simple
module Wire = Hmc_heap_wire
module Words = Hmc_wasm_wire_words
module H = Hmc_wasm_header_words
module Header = Hmc_wasm_header_update
module Q = Wasm_word_sequence
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module M = Wasm_memory
module P = Hmc_linear_preservation
module Bytes = Hmc_linear_bytes
module Splice = Wasm_memory_splice
module U = Wasm_sequence_update
module Write = Hmc_wasm_primitive_write
module Payload = Hmc_wasm_primitive_payload
let (correct @ total) : (operation : D.word_operation) @ immutable -> (left_offset : B.u32) ->
    (left : W.t) @ immutable -> (right : W.t) @ immutable -> (pc : W.limb) -> (current : V.value) @ immutable ->
    (cells : Heap.cells) @ immutable -> (count : D.index) @ immutable -> (state : X.state) @ immutable -> (after : B.bytes) @ immutable ->
    (base_local : B.u32) -> (base : B.u32) -> (before_frame : B.bytes) @ immutable -> (after_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | base <= 4294967248 && P.equal_prefix base state.X.memory after
      && Bytes.drop state.X.memory base === Some before_frame && Bytes.drop after base === Some after_frame
      && Wasm_locals.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && M.load state.X.memory base left_offset M.W64 === Some (S.I64 left)
      && M.load state.X.memory base (Payload.offset ()) M.W64 === Some (S.I64 right)
      && Wire.decode (Wire.Closure_schema (D.S (D.S count))) before_frame ===
        Some (Wire.Closure (pc, Heap.Cell (current, Heap.Cell (V.Word right, cells))), tail)
      && Wire.decode (Wire.Closure_schema (D.S (D.S count))) after_frame ===
        Some (Wire.Closure (pc, Heap.Cell (current, Heap.Cell (Simple.primitive operation left right, cells))), tail)} ->
    {u : unit | X.run (Write.emit operation left_offset base_local) state === X.Done {X.memory = after; machine = state.X.machine}} @ ghost =
  fun operation left_offset left right pc current cells count state after base_local base before_frame after_frame tail premise -> ghost_ (
    let value = Simple.primitive operation left right in
    let suffix = Words.header count before_frame pc current (V.Word right) cells tail () in
    let after_suffix = Words.header count after_frame pc current value cells tail () in
    Words.cells_unique count suffix after_suffix cells tail ();
    let word = Header.number pc in
    let ct = V.tag current in let cp = V.payload current in
    let ot = V.tag (V.Word right) in let op = V.payload (V.Word right) in
    let nt = V.tag value in let np = V.payload value in
    let middle_frame = Q.encode (H.layout word ct cp ot np) suffix in
    let middle = Splice.replace state.X.memory base before_frame middle_frame () in
    Splice.shared state.X.memory middle after base ();
    H.expose word ct cp ot op; H.expose word ct cp ot np; H.expose word ct cp nt np;
    H.five_def (); H.four_def (); U.zero_def (); Payload.offset_def (); Write.tag_offset_def ();
    U.at (H.prefix5 word ct cp ot) Q.End state.X.memory middle before_frame middle_frame suffix op np base 40 (base + 40) ();
    U.at (H.prefix4 word ct cp) (Q.Word (np, Q.End)) middle after middle_frame after_frame suffix ot nt base 32 (base + 32) ();
    Write.correct operation left_offset base_local base left right state middle after ())
