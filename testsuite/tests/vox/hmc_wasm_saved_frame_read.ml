module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module V = Hmc_tagged_cell
module Codec = Hmc_pointer_frame_codec
module Cap = Hmc_frame_capacity
module Index = Hmc_u32_index
module Wire = Hmc_heap_wire
module Memory = Hmc_memory_object
module Saved = Hmc_memory_saved_frame
module Lookup = Hmc_memory_block_lookup
module Bytes = Hmc_linear_bytes
module Header = Hmc_wasm_header_update
module Words = Hmc_wire_word_sequence
let rec (decoded_length @ total) : (count : D.index) @ immutable -> (bytes : B.bytes) @ immutable ->
    (cells : H.cells) @ immutable -> (suffix : B.bytes) @ immutable ->
    {u : unit | Wire.decode_cells count bytes === Some (cells, suffix)} ->
    {u : unit | H.length cells === count} @ ghost = fun count bytes cells suffix premise -> ghost_ (
  Wire.decode_cells_def count bytes; H.length_def cells;
  match count, cells with
  | D.S rest, H.Cell (_, tail) ->
    (match V.decode bytes with None -> () | Some (_, after) -> decoded_length rest after tail suffix ())
  | _ -> ())
let rec (lookup_index @ total) : (blocks : G.table) @ immutable -> (code : B.u32) ->
    (pc : D.index) @ immutable -> (block : G.block) @ immutable ->
    {u : unit | Lookup.lookup blocks code === Some (pc, block)} ->
    {u : unit | Index.represents pc code && G.lookup blocks pc === Some block} @ ghost =
  fun blocks code pc block premise -> ghost_ (
    Lookup.lookup_def blocks code;
    (match blocks with
    | G.Empty -> ()
    | G.Add (_, rest) -> if Index.represents (G.size rest) code then () else lookup_index rest code pc block ());
    Lookup.correct blocks pc code ())
type result = {block : G.block; pc : B.u32; cells : H.cells; padding : H.cells; bytes : B.bytes; suffix : B.bytes}
let (read @ total) : (blocks : G.table) @ immutable -> (memory : B.bytes) @ immutable -> (base : B.u32) ->
    (capacity : Hmc_wasm_relayout.count) -> (activation : F.activation) @ immutable ->
    {u : unit | Saved.load blocks memory base === Some activation
      && Index.represents (Cap.capacity blocks) capacity && capacity < 268435452} ->
    {out : result | G.lookup blocks activation.F.pc === Some out.block && Index.represents activation.F.pc out.pc
      && Codec.decode out.block.G.signature activation.F.pc out.cells === Some (activation, out.padding)
      && H.length out.cells === Cap.capacity blocks
      && Bytes.drop memory base === Some out.bytes
      && Wire.decode_cells (D.S (H.length out.cells)) out.bytes ===
        Some (H.Cell (V.Word (Header.number out.pc), out.cells), out.suffix)} @ immutable =
  fun blocks memory base capacity activation premise ->
    ghost_ (Saved.load_def blocks memory base;
      Memory.load_def memory base (Wire.Closure_schema (Cap.capacity blocks));
      Memory.slots_def (Wire.Closure_schema (Cap.capacity blocks));
      Bytes.load_def memory base (Wire.bytes_size (D.S (Cap.capacity blocks)) D.Z));
    match Memory.load memory base (Wire.Closure_schema (Cap.capacity blocks)) with
    | Some (Wire.Closure (pc, cells)) ->
      (match Lookup.lookup blocks pc with
      | Some (label, block) ->
        ghost_ (lookup_index blocks pc label block (); Codec.decode_def block.G.signature label cells);
        (match Codec.decode block.G.signature label cells, Bytes.drop memory base with
        | Some (_, padding), Some bytes ->
          (match Bytes.take (Wire.bytes_size (D.S (Cap.capacity blocks)) D.Z) bytes with
          | Some payload ->
            ghost_ (Wire.decode_def (Wire.Closure_schema (Cap.capacity blocks)) payload;
              (match V.decode payload with
              | Some (_, rest) -> decoded_length (Cap.capacity blocks) rest cells B.End ()
              | None -> ()));
            let values = H.Cell (V.Word (Header.number pc), cells) in
            let width : B.u32 = 16 * (capacity + 1) in
            ghost_ (H.length_def values; Index.represents_def (D.S (H.length cells)) (capacity + 1);
              Hmc_wasm_closure_stored.words pc cells payload B.End ();
              (match Hmc_wasm_reservation.reserve (D.S (H.length cells)) (capacity + 1) 0 width () with
              | None -> unreachable_ ()
              | Some _ -> Hmc_memory_extent.cells (D.S (H.length cells)) 0 width ());
              Hmc_linear_bounds.range_def (Wire.bytes_size (D.S (H.length cells)) D.Z) 0 width;
              Hmc_linear_take_prefix.correct (Wire.bytes_size (D.S (H.length cells)) D.Z) width bytes payload ();
              Words.size values (capacity + 1) width ());
            let suffix = Wasm_word_transport.sequence (Words.words values) payload bytes B.End width () in
            ghost_ (Words.recover values bytes suffix ());
            {block; pc; cells; padding; bytes; suffix}
          | None -> unreachable_ ())
        | _ -> unreachable_ ())
      | None -> unreachable_ ())
    | _ -> unreachable_ ()
