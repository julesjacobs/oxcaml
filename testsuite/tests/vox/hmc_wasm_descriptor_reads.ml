module B = Wasm_u32
module D = Hm_declarative
module V = Hmc_tagged_cell
module H = Hmc_heap_objects
module R = Hmc_runtime_closures
module Descriptor = Hmc_runtime_descriptor
module Cells = Hmc_memory_cells
module Wire = Hmc_heap_wire
module Words = Hmc_wire_word_sequence
module L = Hmc_linear_bytes
module Index = Hmc_u32_index
module M = Wasm_memory
module S = Wasm_scalar
let[@def] (start_offset @ total) (u : unit) : B.u32 = 8
let[@def] (captures_offset @ total) (u : unit) : B.u32 = 12
let[@def] (recursive_offset @ total) (u : unit) : B.u32 = 24
let (correct @ total) : (memory : B.bytes) @ immutable -> (address : B.u32) ->
    (descriptor : R.descriptor) @ immutable ->
    {u : unit | address <= 4294967263 && Descriptor.load memory address === Some descriptor} ->
    {u : unit | M.load memory address (start_offset ()) M.W32 === Some (S.I32 descriptor.R.start)
      && M.load memory address (captures_offset ()) M.W32 === Some (S.I32 descriptor.R.captures)
      && M.load memory address (recursive_offset ()) M.W32 === Some (S.I32 (S.boolean descriptor.R.recursive))} @ ghost =
  fun memory address descriptor premise -> ghost_ (
    start_offset_def (); captures_offset_def (); recursive_offset_def ();
    Descriptor.load_def memory address; Descriptor.slots_def ();
    Cells.load_def memory address (Descriptor.slots ());
    L.load_def memory address (Wire.bytes_size (Descriptor.slots ()) D.Z);
    match L.drop memory address with
    | None -> ()
    | Some bytes ->
      match L.take (Wire.bytes_size (Descriptor.slots ()) D.Z) bytes with
      | None -> ()
      | Some payload ->
        match Wire.decode_cells (Descriptor.slots ()) payload with
        | Some (cells, B.End) ->
          Descriptor.decode_def cells;
          let values = Descriptor.cells descriptor in
          Descriptor.cells_def descriptor;
          Words.decode (Descriptor.slots ()) payload cells B.End ();
          H.length_def values; H.length_def (H.Cell (V.Boolean descriptor.R.recursive, H.Empty)); H.length_def H.Empty;
          Index.represents_def (D.S (D.S D.Z)) 2; Index.represents_def (D.S D.Z) 1; Index.represents_def D.Z 0;
          (match Hmc_wasm_reservation.reserve (Descriptor.slots ()) 2 0 32 () with
          | None -> unreachable_ ()
          | Some stop -> Hmc_memory_extent.cells (Descriptor.slots ()) 0 32 ());
          Hmc_linear_bounds.range_def (Wire.bytes_size (Descriptor.slots ()) D.Z) 0 32;
          Hmc_linear_take_prefix.correct (Wire.bytes_size (Descriptor.slots ()) D.Z) 32 bytes payload ();
          Words.size values 2 32 ();
          let tail = Wasm_word_transport.sequence (Words.words values) payload bytes B.End 32 () in
          Words.recover values bytes tail ();
          Hmc_heap_simple.lookup_def values D.Z;
          Hmc_heap_simple.lookup_def values (D.S D.Z);
          Hmc_heap_simple.lookup_def (H.Cell (V.Boolean descriptor.R.recursive, H.Empty)) D.Z;
          Hmc_wasm_cells_read.correct memory address bytes (Descriptor.slots ()) values tail D.Z 0 0 8
            (V.Word {Hmc_word64.lo = descriptor.R.start; hi = descriptor.R.captures}) ();
          Hmc_wasm_cells_read.correct memory address bytes (Descriptor.slots ()) values tail (D.S D.Z) 1 16 24
            (V.Boolean descriptor.R.recursive) ();
          V.payload_def (V.Word {Hmc_word64.lo = descriptor.R.start; hi = descriptor.R.captures});
          V.payload_def (V.Boolean descriptor.R.recursive); S.boolean_def descriptor.R.recursive;
          Wasm_memory_word_limbs.correct memory address 8 {Hmc_word64.lo = descriptor.R.start; hi = descriptor.R.captures} ();
          Wasm_memory_word_limbs.correct memory address 24 (V.payload (V.Boolean descriptor.R.recursive)) ();
          Wasm_memory_word_limbs.high_offset_def 8
        | _ -> ())
