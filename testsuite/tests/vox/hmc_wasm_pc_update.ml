module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Words = Hmc_wasm_wire_words
module H = Hmc_wasm_header_words
module Header = Hmc_wasm_header_update
module Q = Wasm_word_sequence
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module M = Wasm_memory
module Bytes = Hmc_linear_bytes
module P = Hmc_linear_preservation
let[@def] (offset @ total) (unit : unit) : B.u32 = 8
let[@def] (emit @ total) (pc : W.limb) (base_local : B.u32) = Wasm_immediate_write.emit (offset ()) base_local (Header.number pc)
let (memory @ total) : (count : D.index) @ immutable -> (old_pc : W.limb) -> (pc : W.limb) ->
    (current : V.value) @ immutable -> (accumulator : V.value) @ immutable -> (cells : Heap.cells) @ immutable ->
    (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable -> (tail : B.bytes) @ immutable -> (base : B.u32) ->
    (before_frame : B.bytes) @ immutable -> (after_frame : B.bytes) @ immutable ->
    {u : unit | base <= 4294967280 && P.equal_prefix base before after
      && Bytes.drop before base === Some before_frame && Bytes.drop after base === Some after_frame
      && Wire.decode (Wire.Closure_schema (D.S (D.S count))) before_frame
        === Some (Wire.Closure (old_pc, Heap.Cell (current, Heap.Cell (accumulator, cells))), tail)
      && Wire.decode (Wire.Closure_schema (D.S (D.S count))) after_frame
        === Some (Wire.Closure (pc, Heap.Cell (current, Heap.Cell (accumulator, cells))), tail)} ->
    {u : unit | M.store before base (offset ()) (S.I64 (Header.number pc)) === Some after} @ ghost =
  fun count old_pc pc current accumulator cells before after tail base before_frame after_frame premise -> ghost_ (
    let left = Words.header count before_frame old_pc current accumulator cells tail () in
    let right = Words.header count after_frame pc current accumulator cells tail () in
    Words.cells_unique count left right cells tail ();
    let ct = V.tag current in let cp = V.payload current in let at = V.tag accumulator in let ap = V.payload accumulator in
    H.expose (Header.number old_pc) ct cp at ap; H.expose (Header.number pc) ct cp at ap;
    H.one_def (); offset_def ();
    Wasm_sequence_update.at (Q.Word (H.tag (), Q.End)) (H.rest ct cp at ap)
      before after before_frame after_frame left (Header.number old_pc) (Header.number pc) base 8 (base + 8) ())
let (correct @ total) : (pc : W.limb) -> (base_local : B.u32) -> (state : X.state) @ immutable ->
    (base : B.u32) -> (after : B.bytes) @ immutable ->
    {u : unit | Wasm_locals.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && M.store state.X.memory base (offset ()) (S.I64 (Header.number pc)) === Some after} ->
    {u : unit | X.run (emit pc base_local) state === X.Done {X.memory = after; machine = state.X.machine}} @ ghost =
  fun pc base_local state base after premise -> ghost_ (
    emit_def pc base_local; Wasm_immediate_write.correct (offset ()) base_local (Header.number pc) state base after ())
