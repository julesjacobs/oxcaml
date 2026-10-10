module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module C = Wasm_word_memory
module Q = Wasm_word_sequence
let[@def] rec (words @ total) (cells : Heap.cells @ immutable) : Q.words @ immutable = match cells with
  | Heap.Empty -> Q.End
  | Heap.Cell (value, rest) -> Q.Word (V.tag value, Q.Word (V.payload value, words rest))
let rec (decode @ total) : (count : D.index) @ immutable -> (bytes : B.bytes) @ immutable ->
    (cells : Heap.cells) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Wire.decode_cells count bytes === Some (cells, tail)} ->
    {u : unit | Q.decode (words cells) bytes === Some tail} @ ghost = fun count bytes cells tail premise -> ghost_ (
    Wire.decode_cells_def count bytes; words_def cells;
    match count, cells with
    | D.Z, _ -> Q.decode_def Q.End bytes
    | D.S n, Heap.Cell (value, rest) -> (match V.decode bytes with
      | None -> ()
      | Some (_, after) ->
        let middle = Hmc_wasm_wire_words.parts bytes value after () in
        decode n after rest tail ();
        Q.decode_def (words cells) bytes;
        Q.decode_def (Q.Word (V.payload value, words rest)) middle;
        W.equal_def (V.tag value) (V.tag value); W.equal_def (V.payload value) (V.payload value))
    | _ -> ())
let rec (recover @ total) : (cells : Heap.cells) @ immutable -> (bytes : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Q.decode (words cells) bytes === Some tail} ->
    {u : unit | Wire.decode_cells (Heap.length cells) bytes === Some (cells, tail)} @ ghost = fun cells bytes tail premise -> ghost_ (
    words_def cells; Heap.length_def cells; Q.decode_def (words cells) bytes;
    Wire.decode_cells_def (Heap.length cells) bytes;
    match cells with
    | Heap.Empty -> ()
    | Heap.Cell (value, rest) -> (match C.decode bytes with
      | None -> ()
      | Some (tag, middle) ->
        Q.decode_def (Q.Word (V.payload value, words rest)) middle;
        W.equal_def (V.tag value) tag;
        (match C.decode middle with
        | None -> ()
        | Some (payload, after) ->
          W.equal_def (V.payload value) payload;
          recover rest after tail ();
          V.decode_def bytes; V.tag_def value; V.payload_def value;
          let _lo : W.limb = payload.W.lo in let _hi : W.limb = payload.W.hi in ())))
let rec (append @ total) : (first : Heap.cells) @ immutable -> (second : Heap.cells) @ immutable ->
    {u : unit | words (Hmc_frame_segments.append first second) === Q.append (words first) (words second)} @ ghost =
  fun first second -> ghost_ (
    Hmc_frame_segments.append_def first second; words_def first; words_def (Hmc_frame_segments.append first second);
    Q.append_def (words first) (words second);
    match first with
    | Heap.Empty -> ()
    | Heap.Cell (value, rest) ->
      Q.append_def (Q.Word (V.payload value, words rest)) (words second); append rest second)
let rec (size @ total) : (cells : Heap.cells) @ immutable -> (count : W.limb) -> (bytes : B.u32) ->
    {u : unit | Hmc_u32_index.represents (Heap.length cells) count && bytes = 16 * count} ->
    {u : unit | Q.size (words cells) bytes} @ ghost = fun cells count bytes premise -> ghost_ (
    Heap.length_def cells; Hmc_u32_index.represents_def (Heap.length cells) count;
    words_def cells; Q.size_def (words cells) bytes;
    match cells with
    | Heap.Empty -> ()
    | Heap.Cell (value, rest) ->
      Q.size_def (Q.Word (V.payload value, words rest)) (bytes - 8); size rest (count - 1) (bytes - 16) ())
