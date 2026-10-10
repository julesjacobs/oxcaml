module B = Wasm_u32
module W = Hmc_word64
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Header = Hmc_wasm_header_update
module Static = Hmc_wasm_call_header
module Layout = Hmc_wasm_call_header_layout
module Write = Wasm_mixed_write
module Match = Wasm_mixed_words
module Words = Hmc_wire_word_sequence
module Q = Wasm_word_sequence
module S = Wasm_scalar
module L = Wasm_locals
let[@def] (writes @ total) (recursive : bool) (pc_local : B.u32) (closure : B.u32) (argument_tag : B.u32) (argument_payload : B.u32) =
  match Static.writes recursive 0 closure argument_tag argument_payload with
  | Write.Write (offset, value, Write.Write (pc_offset, _, rest)) ->
    Write.Write (offset, value, Write.Write (pc_offset, Write.Pointer_local pc_local, rest))
  | _ -> Write.End
let[@def] (emit @ total) (recursive : bool) (pc_local : B.u32) (frame : B.u32) (closure : B.u32) (argument_tag : B.u32) (argument_payload : B.u32) =
  Write.emit (writes recursive pc_local closure argument_tag argument_payload) frame
let (layout @ total) : (recursive : bool) -> (pc : W.limb) -> (pc_local : B.u32) -> (closure : B.u32) -> (argument : V.value) @ immutable ->
    (closure_local : B.u32) -> (argument_tag : B.u32) -> (argument_payload : B.u32) -> (locals : S.stack) @ immutable ->
    {u : unit | L.get locals pc_local === Some (S.I32 pc)
      && L.get locals closure_local === Some (S.I32 closure)
      && L.get locals argument_tag === Some (S.I64 (V.tag argument))
      && L.get locals argument_payload === Some (S.I64 (V.payload argument))} ->
    {u : unit | Q.size (Words.words (Layout.cells recursive pc closure argument)) (Layout.width recursive)
      && Match.matches (writes recursive pc_local closure_local argument_tag argument_payload) (Wasm_mixed_memory.zero ())
        (Words.words (Layout.cells recursive pc closure argument)) locals} @ ghost =
  fun recursive pc pc_local closure argument closure_local argument_tag argument_payload locals premise -> ghost_ (
    Layout.correct recursive pc closure argument closure_local argument_tag argument_payload locals ();
    Static.writes_def recursive pc closure_local argument_tag argument_payload;
    Static.writes_def recursive 0 closure_local argument_tag argument_payload;
    writes_def recursive pc_local closure_local argument_tag argument_payload;
    Layout.cells_def recursive pc closure argument;
    let body = Heap.Cell (V.Closure_pointer closure, Heap.Cell (V.Nil, Heap.Cell (argument,
      if recursive then Heap.Cell (V.Closure_pointer closure, Heap.Empty) else Heap.Empty))) in
    let all = Heap.Cell (V.Word (Header.number pc), body) in
    Words.words_def all; V.tag_def (V.Word (Header.number pc)); V.payload_def (V.Word (Header.number pc));
    Wasm_mixed_memory.zero_def ();
    match Static.writes recursive pc closure_local argument_tag argument_payload with
    | Write.Write (_, _, Write.Write (_, _, rest)) ->
      Match.matches_def (Static.writes recursive pc closure_local argument_tag argument_payload) 0 (Words.words all) locals;
      Match.matches_def (Write.Write (8, Write.Constant (Header.number pc), rest)) 8 (Q.Word (Header.number pc, Words.words body)) locals;
      Write.read_def (Write.Pointer_local pc_local) locals;
      Match.matches_def (writes recursive pc_local closure_local argument_tag argument_payload) 0 (Words.words all) locals;
      Match.matches_def (Write.Write (8, Write.Pointer_local pc_local, rest)) 8 (Q.Word (Header.number pc, Words.words body)) locals
    | _ -> ())
