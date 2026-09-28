module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Words = Hmc_wire_word_sequence
module Q = Wasm_word_sequence
module L = Hmc_linear_bytes
module P = Hmc_linear_preservation
module Index = Hmc_u32_index
module Reserve = Hmc_wasm_reservation
module Memory = Hmc_memory_object
let (words @ total) : (code : W.limb) -> (captures : Heap.cells) @ immutable -> (bytes : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    {u : unit | Wire.decode (Wire.Closure_schema (Heap.length captures)) bytes === Some (Wire.Closure (code, captures), suffix)} ->
    {u : unit | Q.decode (Words.words (Heap.Cell (V.Word (Header.number code), captures))) bytes === Some suffix} @ ghost =
  fun code captures bytes suffix premise -> ghost_ (
    Wire.decode_def (Wire.Closure_schema (Heap.length captures)) bytes;
    Header.number_def code;
    Wire.decode_cells_def (D.S (Heap.length captures)) bytes;
    Words.decode (D.S (Heap.length captures)) bytes (Heap.Cell (V.Word (Header.number code), captures)) suffix ())
let (correct @ total) : (memory : B.bytes) @ immutable -> (base : B.u32) -> (bytes : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    (code : W.limb) -> (captures : Heap.cells) @ immutable -> (count : Hmc_wasm_relayout.count) ->
    {u : unit | L.drop memory base === Some bytes && Index.represents (Heap.length captures) count
      && Wire.decode (Wire.Closure_schema (Heap.length captures)) bytes === Some (Wire.Closure (code, captures), suffix)} ->
    {u : unit | Memory.load memory base (Wire.Closure_schema (Heap.length captures)) === Some (Wire.Closure (code, captures))} @ ghost =
  fun memory base bytes suffix code captures count premise -> ghost_ (
    let object_ = Wire.Closure (code, captures) in
    let payload = Wire.encode object_ B.End in
    let cells = Heap.Cell (V.Word (Header.number code), captures) in
    let slots = D.S (Heap.length captures) in
    let width : B.u32 = 16 * (count + 1) in
    Wire.schema_def object_; Wire.slots_def object_; V.length_def B.End;
    Heap.length_def cells; Index.represents_def slots (count + 1);
    words code captures bytes suffix (); words code captures payload B.End ();
    Words.size cells (count + 1) width ();
    Q.prefix (Words.words cells) bytes payload suffix B.End width ();
    (match Reserve.reserve slots (count + 1) 0 width () with
    | None -> unreachable_ ()
    | Some stop ->
      Hmc_memory_extent.cells slots 0 width ();
      Hmc_linear_bounds.range_def (Wire.bytes_size slots D.Z) 0 width;
      P.take (Wire.bytes_size slots D.Z) width bytes payload ();
      Hmc_wasm_cons_stored.take_self payload;
      Memory.load_def memory base (Wire.Closure_schema (Heap.length captures));
      Memory.slots_def (Wire.Closure_schema (Heap.length captures));
      L.load_def memory base (Wire.bytes_size slots D.Z)))
