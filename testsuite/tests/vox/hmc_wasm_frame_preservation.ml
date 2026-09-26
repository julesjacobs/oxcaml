module B = Wasm_u32
module D = Hm_declarative
module V = Hmc_tagged_cell
module H = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Words = Hmc_wire_word_sequence
module Q = Wasm_word_sequence
module L = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
module Index = Hmc_u32_index
let rec (same_length @ total) : (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (stop : B.u32) -> (tail : B.bytes) @ immutable ->
    {u : unit | L.drop before stop === Some tail && L.drop after stop === Some tail} ->
    {u : unit | V.length before === V.length after} @ ghost = fun before after stop tail premise -> ghost_ (
    L.drop_def before stop; L.drop_def after stop; V.length_def before; V.length_def after;
    if stop = 0 then () else match before, after with
    | B.Byte (_, rest), B.Byte (_, more) -> same_length rest more (stop - 1) tail ()
    | _ -> ())
let (suffix @ total) : (memory : B.bytes) @ immutable -> (base : B.u32) -> (stop : B.u32) ->
    (bytes : B.bytes) @ immutable -> (cells : H.cells) @ immutable -> (tail : B.bytes) @ immutable -> (count : B.u32) ->
    {u : unit | Index.represents (H.length cells) count && stop = base + 16 * count
      && L.drop memory base === Some bytes && Wire.decode_cells (H.length cells) bytes === Some (cells, tail)} ->
    {u : unit | L.drop memory stop === Some tail && Bounds.covers memory stop} @ ghost =
  fun memory base stop bytes cells tail count premise -> ghost_ (
    Words.decode (H.length cells) bytes cells tail ();
    Words.size cells count (stop - base) ();
    Q.prefix (Words.words cells) bytes bytes tail tail (stop - base) ();
    Wasm_cell.shift memory base (stop - base) stop bytes ();
    Bounds.covers_def memory stop)
let (correct @ total) : (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (base : B.u32) -> (stop : B.u32) -> (left : B.bytes) @ immutable -> (right : B.bytes) @ immutable ->
    (old_cells : H.cells) @ immutable -> (new_cells : H.cells) @ immutable -> (tail : B.bytes) @ immutable -> (count : B.u32) ->
    {u : unit | Index.represents (H.length old_cells) count && H.length new_cells === H.length old_cells
      && stop = base + 16 * count && L.drop before base === Some left && L.drop after base === Some right
      && Wire.decode_cells (H.length old_cells) left === Some (old_cells, tail)
      && Wire.decode_cells (H.length new_cells) right === Some (new_cells, tail)} ->
    {u : unit | L.drop before stop === Some tail && L.drop after stop === Some tail
      && Bounds.covers before stop && Bounds.covers after stop && V.length before === V.length after} @ ghost =
  fun before after base stop left right old_cells new_cells tail count premise -> ghost_ (
    suffix before base stop left old_cells tail count ();
    suffix after base stop right new_cells tail count ();
    same_length before after stop tail ())
