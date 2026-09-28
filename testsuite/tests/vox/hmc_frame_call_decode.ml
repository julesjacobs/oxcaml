module D = Hm_declarative
module W = Hmc_word64
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module K = Hmc_closure_ir
module G = Hmc_cfg_ir
module F = Hmc_heap_frame
module Codec = Hmc_pointer_frame_codec
module Seg = Hmc_frame_segments
module Entry = Hmc_frame_call_entry
module Layout = Hmc_wasm_call_header_layout
module Header = Hmc_wasm_header_update
let rec (environment @ total) : (context : D.context) @ immutable -> (cells : Heap.cells) @ immutable ->
    {u : unit | Codec.environment context cells} ->
    {u : unit | Codec.decode_environment context cells === Some (cells, Heap.Empty)} @ ghost =
  fun context cells premise -> ghost_ (
    Codec.environment_def context cells; Codec.decode_environment_def context cells;
    match context, cells with
    | D.Binding (_, rest), Heap.Cell (_, tail) -> environment rest tail ()
    | _ -> ())
let[@def] (cells @ total) (entry : K.entry @ immutable) (closure : W.limb) (argument : V.value @ immutable) (captures : Heap.cells @ immutable) =
  Heap.Cell (V.Closure_pointer closure, Heap.Cell (V.Nil, Heap.Cell (argument,
    (if entry.K.recursive then Heap.Cell (V.Closure_pointer closure, captures) else captures))))
let (correct @ total) : (entry : K.entry) @ immutable -> (start : D.index) @ immutable -> (pc : W.limb) ->
    (closure : W.limb) -> (argument : V.value) @ immutable -> (captures : Heap.cells) @ immutable ->
    {u : unit | Codec.environment entry.K.captured captures} ->
    {u : unit | Codec.decode (Entry.signature entry) start (cells entry closure argument captures) ===
        Some (Entry.activation entry start (V.Closure_pointer closure) argument captures, Heap.Empty)
      && Seg.append (Layout.cells entry.K.recursive pc closure argument) captures ===
        Heap.Cell (V.Word (Header.number pc), cells entry closure argument captures)} @ ghost =
  fun entry start pc closure argument captures premise -> ghost_ (
    environment entry.K.captured captures ();
    Entry.signature_def entry; Entry.activation_def entry start (V.Closure_pointer closure) argument captures;
    K.context_def entry; cells_def entry closure argument captures;
    Codec.decode_def (Entry.signature entry) start (cells entry closure argument captures);
    Codec.decode_environment_def (K.context entry) (Entry.activation entry start (V.Closure_pointer closure) argument captures).F.env;
    (if entry.K.recursive then
      Codec.decode_environment_def (D.Binding (D.Forall (D.Z, D.Function (entry.K.argument, entry.K.result)), entry.K.captured)) (Heap.Cell (V.Closure_pointer closure, captures)) else ());
    Codec.decode_temporaries_def G.Empty_temporaries Heap.Empty;
    Layout.cells_def entry.K.recursive pc closure argument;
    let self = if entry.K.recursive then Heap.Cell (V.Closure_pointer closure, Heap.Empty) else Heap.Empty in
    let a = Heap.Cell (argument, self) in let b = Heap.Cell (V.Nil, a) in
    let c = Heap.Cell (V.Closure_pointer closure, b) in let d = Heap.Cell (V.Word (Header.number pc), c) in
    Seg.append_def d captures; Seg.append_def c captures; Seg.append_def b captures; Seg.append_def a captures;
    Seg.append_def self captures; Seg.append_def Heap.Empty captures)
