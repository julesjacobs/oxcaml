module B = Wasm_u32
module D = Hm_declarative
module H = Hmc_heap_objects
module V = Hmc_tagged_cell
module F = Hmc_heap_frame
module G = Hmc_cfg_ir
module Codec = Hmc_pointer_frame_codec
module Seg = Hmc_frame_segments
module Wire = Hmc_heap_wire
module Bytes = Hmc_linear_bytes
module Index = Hmc_u32_index
module Header = Hmc_wasm_header_update
module Cut = Hmc_cell_capacity
module Frame = Hmc_wasm_frame_preservation
let rec (environment @ total) : (context : D.context) @ immutable -> (cells : H.cells) @ immutable ->
    (env : H.cells) @ immutable -> (rest : H.cells) @ immutable -> (padding : H.cells) @ immutable ->
    {u : unit | Codec.decode_environment context cells === Some (env, rest)} ->
    {u : unit | Codec.decode_environment context (Seg.append cells padding) === Some (env, Seg.append rest padding)} @ ghost =
  fun context cells env rest padding premise -> ghost_ (
    Codec.decode_environment_def context cells; Codec.decode_environment_def context (Seg.append cells padding);
    Seg.append_def cells padding;
    match context, cells with
    | D.Empty_context, _ -> ()
    | D.Binding (_, next), H.Cell (_, tail) ->
      (match Codec.decode_environment next tail with
      | None -> () | Some (inner, _) -> environment next tail inner rest padding ())
    | _ -> ())
let rec (temporaries @ total) : (schema : G.temporaries) @ immutable -> (cells : H.cells) @ immutable ->
    (runtime : F.temporaries) @ immutable -> (rest : H.cells) @ immutable -> (padding : H.cells) @ immutable ->
    {u : unit | Codec.decode_temporaries schema cells === Some (runtime, rest)} ->
    {u : unit | Codec.decode_temporaries schema (Seg.append cells padding) === Some (runtime, Seg.append rest padding)} @ ghost =
  fun schema cells runtime rest padding premise -> ghost_ (
    Codec.decode_temporaries_def schema cells; Codec.decode_temporaries_def schema (Seg.append cells padding);
    match schema with
    | G.Empty_temporaries -> ()
    | G.Environment (context, next) ->
      (match Codec.decode_environment context cells with
      | None -> ()
      | Some (env, tail) ->
        environment context cells env tail padding ();
        match Codec.decode_temporaries next tail with
        | None -> () | Some (inner, _) -> temporaries next tail inner rest padding ())
    | G.Value (context, _, next) ->
      (match cells with
      | H.Empty -> ()
      | H.Cell (_, tail) ->
        Seg.append_def cells padding;
        match Codec.decode_environment context tail with
        | None -> ()
        | Some (env, remaining) ->
          environment context tail env remaining padding ();
          match Codec.decode_temporaries next remaining with
          | None -> () | Some (inner, _) -> temporaries next remaining inner rest padding ()))
let (decode @ total) : (signature : G.signature) @ immutable -> (activation : F.activation) @ immutable ->
    (cells : H.cells) @ immutable -> (padding : H.cells) @ immutable ->
    {u : unit | Codec.decode signature activation.F.pc cells === Some (activation, H.Empty)} ->
    {u : unit | Codec.decode signature activation.F.pc (Seg.append cells padding) === Some (activation, padding)} @ ghost =
  fun signature activation cells padding premise -> ghost_ (
    Codec.decode_def signature activation.F.pc cells;
    Codec.decode_def signature activation.F.pc (Seg.append cells padding);
    Seg.append_def cells padding; Seg.append_def H.Empty padding;
    match cells with
    | H.Cell (_, (H.Cell (_, body) as rest)) ->
      Seg.append_def rest padding;
      (match Codec.decode_environment signature.G.locals body with
      | None -> ()
      | Some (env, remaining) ->
        environment signature.G.locals body env remaining padding ();
        temporaries signature.G.temporaries remaining activation.F.temporaries H.Empty padding ())
    | _ -> ())
let rec (split_wire @ total) : (prefix : H.cells) @ immutable -> (suffix : H.cells) @ immutable ->
    (bytes : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Wire.decode_cells (H.length (Seg.append prefix suffix)) bytes === Some (Seg.append prefix suffix, tail)} ->
    {middle : B.bytes | Wire.decode_cells (H.length prefix) bytes === Some (prefix, middle)
      && Wire.decode_cells (H.length suffix) middle === Some (suffix, tail)} @ immutable =
  fun prefix suffix bytes tail premise ->
    ghost_ (Seg.append_def prefix suffix; H.length_def prefix; H.length_def (Seg.append prefix suffix);
      Wire.decode_cells_def (H.length prefix) bytes;
      Wire.decode_cells_def (H.length (Seg.append prefix suffix)) bytes);
    match prefix with
    | H.Empty -> bytes
    | H.Cell (_, rest) ->
      (match V.decode bytes with
      | None -> unreachable_ ()
      | Some (_, remaining) -> split_wire rest suffix remaining tail ())
type result = {cells : H.cells; padding : H.cells}
let (correct @ total) : (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable -> (base : B.u32) -> (stop : B.u32) ->
    (old_pc : B.u32) -> (pc : B.u32) -> (old_cells : H.cells) @ immutable -> (cells : H.cells) @ immutable ->
    (capacity : Hmc_wasm_relayout.count) -> (used : Hmc_wasm_relayout.count) ->
    (old_bytes : B.bytes) @ immutable -> (bytes : B.bytes) @ immutable -> (old_tail : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    (signature : G.signature) @ immutable -> (activation : F.activation) @ immutable ->
    {u : unit | Index.represents (H.length old_cells) capacity && Index.represents (H.length cells) used && used <= capacity
      && stop = base + 16 + 16 * used
      && Bytes.drop before base === Some old_bytes && Bytes.drop after base === Some bytes
      && Wire.decode_cells (D.S (H.length old_cells)) old_bytes === Some (H.Cell (V.Word (Header.number old_pc), old_cells), old_tail)
      && Wire.decode_cells (D.S (H.length cells)) bytes === Some (H.Cell (V.Word (Header.number pc), cells), tail)
      && Bytes.drop after stop === Bytes.drop before stop
      && Codec.decode signature activation.F.pc cells === Some (activation, H.Empty)} ->
    {out : result | H.length out.cells === H.length old_cells && out.cells === Seg.append cells out.padding
      && Codec.decode signature activation.F.pc out.cells === Some (activation, out.padding)
      && Wire.decode_cells (D.S (H.length out.cells)) bytes === Some (H.Cell (V.Word (Header.number pc), out.cells), old_tail)} @ immutable =
  fun before after base stop old_pc pc old_cells cells capacity used old_bytes bytes old_tail tail signature activation premise ->
    ghost_ (Cut.numeric (H.length cells) (H.length old_cells) used capacity ());
    let cut = Cut.split (H.length cells) old_cells () in
    let old_prefix = H.Cell (V.Word (Header.number old_pc), cut.Cut.prefix) in
    let full = H.Cell (V.Word (Header.number pc), cells) in
    ghost_ (Seg.append_def old_prefix cut.Cut.suffix; H.length_def old_prefix; H.length_def full;
      H.length_def (Seg.append old_prefix cut.Cut.suffix));
    let middle = split_wire old_prefix cut.Cut.suffix old_bytes old_tail () in
    ghost_ (Index.represents_def (D.S (H.length cells)) (used + 1);
      Frame.suffix before base stop old_bytes old_prefix middle (used + 1) ();
      Frame.suffix after base stop bytes full tail (used + 1) ();
      Hmc_wire_cells_join.correct full cut.Cut.suffix bytes tail old_tail ();
      Seg.append_def full cut.Cut.suffix;
      H.length_def (Seg.append full cut.Cut.suffix);
      Hmc_wasm_range_four.length_append cells cut.Cut.suffix;
      decode signature activation cells cut.Cut.suffix ());
    {cells = Seg.append cells cut.Cut.suffix; padding = cut.Cut.suffix}

module K = Hmc_closure_ir
module Model = Hmc_frame_call_entry
module Decode = Hmc_frame_call_decode
let (callee_size @ total) : (entry : K.entry) @ immutable -> (start : D.index) @ immutable -> (pc : B.u32) ->
    (closure : B.u32) -> (argument : V.value) @ immutable -> (captures : H.cells) @ immutable ->
    (cells : H.cells) @ immutable -> (count : B.u32) -> (used : B.u32) ->
    {u : unit | Codec.environment entry.K.captured captures && Index.represents (H.length captures) count
      && used = (if entry.K.recursive then 4 + count else 3 + count)
      && Codec.decode (Model.signature entry) start cells === Some (Model.activation entry start (V.Closure_pointer closure) argument captures, H.Empty)} ->
    {u : unit | Index.represents (H.length cells) used} @ ghost =
  fun entry start pc closure argument captures cells count used premise -> ghost_ (
    Decode.correct entry start pc closure argument captures ();
    Hmc_frame_decode_unique.frame (Model.signature entry) start cells (Decode.cells entry closure argument captures)
      (Model.activation entry start (V.Closure_pointer closure) argument captures) H.Empty ();
    Decode.cells_def entry closure argument captures;
    let tail = if entry.K.recursive then H.Cell (V.Closure_pointer closure, captures) else captures in
    H.length_def (H.Cell (V.Closure_pointer closure, captures)); Index.represents_def (D.S (H.length captures)) (count + 1);
    H.length_def (H.Cell (argument, tail));
    H.length_def (H.Cell (V.Nil, H.Cell (argument, tail)));
    H.length_def (H.Cell (V.Closure_pointer closure, H.Cell (V.Nil, H.Cell (argument, tail))));
    Index.represents_def (H.length cells) used;
    Index.represents_def (D.S (D.S (H.length tail))) (used - 1);
    Index.represents_def (D.S (H.length tail)) (used - 2))
