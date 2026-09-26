module B = Wasm_u32
module W = Hmc_word64
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
module Header = Hmc_wasm_header_update
module Seg = Hmc_frame_segments
module Range = Hmc_wasm_range_copy
let[@def] (position @ total) (u : unit) : Hmc_wasm_relayout.count = 0
let[@def] (offset @ total) (u : unit) : B.u32 = 8
let rec (take_self @ total) : (cells : Heap.cells) @ immutable ->
    {u : unit | Seg.take (Heap.length cells) cells === Some cells} @ ghost = fun cells -> ghost_ (
  Heap.length_def cells; Seg.take_def (Heap.length cells) cells;
  match cells with Heap.Empty -> () | Heap.Cell (_, rest) -> take_self rest)
type result = {code : W.limb; bytes : B.bytes; tail : B.bytes}
let (correct @ total) : (table : Hmc_closure_ir.table) @ immutable -> (heap : Heap.heap) @ immutable ->
    (memory : B.bytes) @ immutable -> (address : B.u32) -> (id : D.index) @ immutable -> (captures : Heap.cells) @ immutable -> (count : Hmc_wasm_relayout.count) ->
    {u : unit | Heap.valid table heap && Image.related memory heap && Index.represents (Heap.length captures) count
      && Hmc_heap_preservation.lookup_object heap address === Some (Heap.Closure (id, captures))} ->
    {out : result | Index.represents id out.code && address + 16 + 16 * count <= 4294967295
      && M.load memory address (offset ()) M.W64 === Some (S.I64 (Header.number out.code))
      && Range.reads memory address (position ()) captures
      && L.drop memory address === Some out.bytes
      && Wire.decode_cells (D.S (Heap.length captures)) out.bytes === Some (Heap.Cell (V.Word (Header.number out.code), captures), out.tail)} @ immutable =
  fun table heap memory address id captures count premise ->
    let object_ = Heap.Closure (id, captures) in
    let allocation = Hmc_heap_bounds.allocation table heap address object_ () in
    ghost_ (Heap.slots_def object_; Index.represents_def (D.S (Heap.length captures)) (count + 1);
      Hmc_wasm_reservation.span (D.S (Heap.length captures)) (count + 1) address allocation.Heap.stop ());
    let wire = Image.fetch memory heap address object_ () in
    ghost_ (Image.schema_def object_; Wire.corresponds_def object_ wire;
      Memory.load_def memory address (Wire.Closure_schema (Heap.length captures)); Memory.slots_def (Wire.Closure_schema (Heap.length captures));
      L.load_def memory address (Wire.bytes_size (D.S (Heap.length captures)) D.Z));
    match wire with
    | Wire.Cons _ -> unreachable_ ()
    | Wire.Closure (code, _) ->
    match L.drop memory address with
    | None -> unreachable_ ()
    | Some bytes ->
    match L.take (Wire.bytes_size (D.S (Heap.length captures)) D.Z) bytes with
    | None -> unreachable_ ()
    | Some payload ->
      let values = Heap.Cell (V.Word (Header.number code), captures) in
      let width : B.u32 = 16 * (count + 1) in
      ghost_ (Heap.length_def values;
        Hmc_wasm_closure_stored.words code captures payload B.End ();
        (match Hmc_wasm_reservation.reserve (D.S (Heap.length captures)) (count + 1) 0 width () with
        | None -> unreachable_ ()
        | Some stop -> Hmc_memory_extent.cells (D.S (Heap.length captures)) 0 width ());
        Hmc_linear_bounds.range_def (Wire.bytes_size (D.S (Heap.length captures)) D.Z) 0 width;
        Hmc_linear_take_prefix.correct (Wire.bytes_size (D.S (Heap.length captures)) D.Z) width bytes payload ();
        Words.size values (count + 1) width ());
      let tail = Wasm_word_transport.sequence (Words.words values) payload bytes B.End width () in
      ghost_ (Words.recover values bytes tail ();
        Hmc_heap_simple.lookup_def values D.Z; Index.represents_def D.Z 0;
        Hmc_wasm_cells_read.correct memory address bytes (D.S (Heap.length captures)) values tail D.Z 0 0 8 (V.Word (Header.number code)) ();
        V.payload_def (V.Word (Header.number code)); offset_def (); position_def ();
        Seg.drop_def (D.S D.Z) values; Seg.drop_def D.Z captures;
        take_self captures;
        Hmc_wasm_range_read.correct captures 0 count D.Z values captures memory address bytes tail ());
      {code; bytes; tail}
