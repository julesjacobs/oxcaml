module B = Wasm_u32
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Image = Hmc_heap_image
module Memory = Hmc_memory_object
module L = Hmc_linear_bytes
module Words = Hmc_wire_word_sequence
module Q = Wasm_word_sequence
module Index = Hmc_u32_index
module M = Wasm_memory
module S = Wasm_scalar
let[@def] (two @ total) (u : unit) = D.S (D.S D.Z)
let[@def] (zero @ total) (u : unit) : B.u32 = 0
let[@def] (eight @ total) (u : unit) : B.u32 = 8
let[@def] (sixteen @ total) (u : unit) : B.u32 = 16
let[@def] (twenty_four @ total) (u : unit) : B.u32 = 24
let (cells @ total) : (bytes : B.bytes) @ immutable -> (head : V.value) @ immutable -> (tail : V.value) @ immutable ->
    (suffix : B.bytes) @ immutable ->
    {u : unit | Wire.decode Wire.Cons_schema bytes === Some (Wire.Cons (head, tail), suffix)} ->
    {u : unit | Wire.decode_cells (two ()) bytes === Some (Heap.Cell (head, Heap.Cell (tail, Heap.Empty)), suffix)} @ ghost =
  fun bytes head tail suffix premise -> ghost_ (
    Wire.decode_def Wire.Cons_schema bytes; two_def (); Wire.decode_cells_def (D.S (D.S D.Z)) bytes;
    match V.decode bytes with
    | None -> ()
    | Some (_, rest) -> Wire.decode_cells_def (D.S D.Z) rest;
      (match V.decode rest with None -> () | Some (_, remaining) -> Wire.decode_cells_def D.Z remaining))
let (read @ total) : (table : Hmc_closure_ir.table) @ immutable -> (heap : Heap.heap) @ immutable ->
    (memory : B.bytes) @ immutable -> (address : B.u32) -> (head : V.value) @ immutable -> (tail : V.value) @ immutable ->
    {u : unit | Heap.valid table heap && Image.related memory heap
      && Hmc_heap_preservation.lookup_object heap address === Some (Heap.Cons (head, tail))} ->
    {u : unit | address <= 4294967263
      && M.load memory address (zero ()) M.W64 === Some (S.I64 (V.tag head))
      && M.load memory address (eight ()) M.W64 === Some (S.I64 (V.payload head))
      && M.load memory address (sixteen ()) M.W64 === Some (S.I64 (V.tag tail))
      && M.load memory address (twenty_four ()) M.W64 === Some (S.I64 (V.payload tail))} @ ghost =
  fun table heap memory address head tail premise -> ghost_ (
    let object_ = Heap.Cons (head, tail) in
    let allocation = Hmc_heap_bounds.allocation table heap address object_ () in
    Heap.slots_def object_; Hmc_heap_extent.span_def (D.S (D.S D.Z)) address allocation.Heap.stop;
    Hmc_heap_extent.span_def (D.S D.Z) (address + 16) allocation.Heap.stop;
    Hmc_heap_extent.span_def D.Z (address + 32) allocation.Heap.stop;
    let wire = Image.fetch memory heap address object_ () in
    Image.schema_def object_; Wire.corresponds_def object_ wire;
    Memory.load_def memory address Wire.Cons_schema; Memory.slots_def Wire.Cons_schema;
    L.load_def memory address (Wire.bytes_size (D.S (D.S D.Z)) D.Z);
    match L.drop memory address with
    | None -> ()
    | Some bytes -> (match L.take (Wire.bytes_size (D.S (D.S D.Z)) D.Z) bytes with
      | None -> ()
      | Some payload ->
        cells payload head tail B.End ();
        let values = Heap.Cell (head, Heap.Cell (tail, Heap.Empty)) in
        two_def (); Heap.length_def values; Heap.length_def (Heap.Cell (tail, Heap.Empty)); Heap.length_def Heap.Empty;
        Index.represents_def D.Z 0; Index.represents_def (D.S D.Z) 1; Index.represents_def (D.S (D.S D.Z)) 2;
        Hmc_heap_extent.span_def (D.S (D.S D.Z)) 0 32;
        Hmc_heap_extent.span_def (D.S D.Z) 16 32; Hmc_heap_extent.span_def D.Z 32 32;
        Hmc_memory_extent.cells (D.S (D.S D.Z)) 0 32 ();
        Hmc_linear_bounds.range_def (Wire.bytes_size (D.S (D.S D.Z)) D.Z) 0 32;
        Hmc_linear_take_prefix.correct (Wire.bytes_size (D.S (D.S D.Z)) D.Z) 32 bytes payload ();
        Words.decode (D.S (D.S D.Z)) payload values B.End (); Words.size values 2 32 ();
        let suffix = Wasm_word_transport.sequence (Words.words values) payload bytes B.End 32 () in
        Words.recover values bytes suffix ();
        Hmc_heap_simple.lookup_def values D.Z; Hmc_heap_simple.lookup_def values (D.S D.Z);
        Hmc_heap_simple.lookup_def (Heap.Cell (tail, Heap.Empty)) D.Z;
        Hmc_wasm_cells_read.correct memory address bytes (D.S (D.S D.Z)) values suffix D.Z 0 0 8 head ();
        Hmc_wasm_cells_read.correct memory address bytes (D.S (D.S D.Z)) values suffix (D.S D.Z) 1 16 24 tail ();
        zero_def (); eight_def (); sixteen_def (); twenty_four_def ()))
