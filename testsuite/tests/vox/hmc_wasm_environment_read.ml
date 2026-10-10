module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Frame = Hmc_heap_frame
module G = Hmc_cfg_ir
module Codec = Hmc_pointer_frame_codec
module Wire = Hmc_heap_wire
module Words = Hmc_wasm_wire_words
module Header = Hmc_wasm_header_words
module Q = Wasm_word_sequence
module L = Hmc_linear_bytes
module M = Wasm_memory
module S = Wasm_scalar
module Find = Hmc_wasm_environment_lookup
let[@def] (header_size @ total) (unit : unit) : B.u32 = 48
let (size @ total) (pc : W.t @ immutable) (ct : W.t @ immutable) (cp : W.t @ immutable)
    (at : W.t @ immutable) (ap : W.t @ immutable) :
    {u : unit | Q.size (Header.layout pc ct cp at ap) (header_size ())} @ ghost = ghost_ (
  header_size_def (); Header.layout_def pc ct cp at ap;
  Q.size_def (Header.layout pc ct cp at ap) 48;
  Q.size_def (Q.Word (pc, Q.Word (ct, Q.Word (cp, Q.Word (at, Q.Word (ap, Q.End)))))) 40;
  Q.size_def (Q.Word (ct, Q.Word (cp, Q.Word (at, Q.Word (ap, Q.End))))) 32;
  Q.size_def (Q.Word (cp, Q.Word (at, Q.Word (ap, Q.End)))) 24;
  Q.size_def (Q.Word (at, Q.Word (ap, Q.End))) 16;
  Q.size_def (Q.Word (ap, Q.End)) 8; Q.size_def Q.End 0)
let (correct @ total) : (memory : B.bytes) @ immutable -> (base : B.u32) -> (bytes : B.bytes) @ immutable ->
    (count : D.index) @ immutable -> (pc : W.limb) -> (cells : Heap.cells) @ immutable -> (tail : B.bytes) @ immutable ->
    (signature : G.signature) @ immutable -> (activation : Frame.activation) @ immutable -> (padding : Heap.cells) @ immutable ->
    (index : D.index) @ immutable -> (number : W.limb) -> (source_tag : B.u32) -> (source_payload : B.u32) -> (value : V.value) @ immutable ->
    {u : unit | source_tag = 48 + 16 * number && source_payload = source_tag + 8 && base + source_tag <= 4294967280
      && L.drop memory base === Some bytes && Hmc_u32_index.represents index number
      && Hmc_heap_simple.lookup activation.Frame.env index === Some value
      && Wire.decode (Wire.Closure_schema (D.S (D.S count))) bytes
        === Some (Wire.Closure (pc, Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, cells))), tail)
      && Codec.decode signature activation.Frame.pc (Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, cells)))
        === Some (activation, padding)} ->
    {u : unit | M.load memory base source_tag M.W64 === Some (S.I64 (V.tag value))
      && M.load memory base source_payload M.W64 === Some (S.I64 (V.payload value))} @ ghost =
  fun memory base bytes count pc cells tail signature activation padding index number source_tag source_payload value premise -> ghost_ (
    Codec.decode_def signature activation.Frame.pc (Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, cells)));
    (match Codec.decode_environment signature.G.locals cells with
    | None -> ()
    | Some (_, remaining) -> Find.environment signature.G.locals cells activation.Frame.env remaining index value ());
    let env_bytes = Words.header count bytes pc activation.Frame.current activation.Frame.accumulator cells tail () in
    let layout = Header.layout (Hmc_wasm_header_update.number pc) (V.tag activation.Frame.current) (V.payload activation.Frame.current)
      (V.tag activation.Frame.accumulator) (V.payload activation.Frame.accumulator) in
    size (Hmc_wasm_header_update.number pc) (V.tag activation.Frame.current) (V.payload activation.Frame.current)
      (V.tag activation.Frame.accumulator) (V.payload activation.Frame.accumulator);
    header_size_def (); Q.prefix layout bytes bytes env_bytes env_bytes 48 ();
    let suffix = Find.find count env_bytes cells tail index number (source_tag - 48) value () in
    Wasm_cell.shift bytes 48 (source_tag - 48) source_tag env_bytes ();
    Wasm_cell.shift memory base source_tag (base + source_tag) bytes ();
    match V.decode suffix with
    | None -> ()
    | Some (_, after) ->
      Wasm_cell.cell memory (base + source_tag) suffix value after ();
      Wasm_cell.tag_def memory (base + source_tag); Wasm_cell.payload_def memory (base + source_tag);
      M.load_def memory base source_tag M.W64; M.load_def memory base source_payload M.W64;
      M.load_def memory (base + source_tag) 0 M.W64; M.load_def memory (base + source_tag) 8 M.W64;
      M.address_def base source_tag M.W64; M.address_def base source_payload M.W64;
      M.address_def (base + source_tag) 0 M.W64; M.address_def (base + source_tag) 8 M.W64;
      M.size_def M.W64)
