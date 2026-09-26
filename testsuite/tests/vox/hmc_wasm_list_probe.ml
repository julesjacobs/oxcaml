module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Simple = Hmc_heap_simple
module Index = Hmc_u32_index
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module L = Wasm_locals
module Probe = Wasm_word_probe
let[@def] (offset @ total) (u : unit) : B.u32 = 32
let[@def] (nil_tag @ total) (u : unit) : W.limb = 2
let[@def] (is_nil @ total) (value : V.value @ immutable) = match value with V.Nil -> true | _ -> false
let[@def] (emit @ total) (base_local : B.u32) = Probe.emit (offset ()) (nil_tag ()) base_local
let (correct @ total) : (state : X.state) @ immutable -> (base : B.u32) -> (base_local : B.u32) ->
    (value : V.value) @ immutable -> (pc : W.limb) -> (current : V.value) @ immutable -> (body : Heap.cells) @ immutable ->
    (frame : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    {u : unit | base <= 4294967248 && L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && Hmc_linear_bytes.drop state.X.memory base === Some frame
      && Wire.decode_cells (D.S (D.S (D.S (Heap.length body)))) frame ===
        Some (Heap.Cell (V.Word (Header.number pc), Heap.Cell (current, Heap.Cell (value, body))), suffix)} ->
    {u : unit | X.run (emit base_local) state === X.Done {X.memory = state.X.memory;
      machine = {E.locals = state.X.machine.E.locals; stack = S.Push (S.I32 (S.boolean (is_nil value)), state.X.machine.E.stack)}}} @ ghost =
  fun state base base_local value pc current body frame suffix premise -> ghost_ (
    let cells = Heap.Cell (V.Word (Header.number pc), Heap.Cell (current, Heap.Cell (value, body))) in
    Simple.lookup_def cells (D.S (D.S D.Z));
    Simple.lookup_def (Heap.Cell (current, Heap.Cell (value, body))) (D.S D.Z);
    Simple.lookup_def (Heap.Cell (value, body)) D.Z;
    Index.represents_def D.Z 0; Index.represents_def (D.S D.Z) 1; Index.represents_def (D.S (D.S D.Z)) 2;
    Hmc_wasm_cells_read.correct state.X.memory base frame (D.S (D.S (D.S (Heap.length body)))) cells suffix (D.S (D.S D.Z)) 2 32 40 value ();
    V.tag_def value; Header.number_def (V.tag value).W.lo; offset_def (); nil_tag_def (); is_nil_def value;
    Probe.correct (offset ()) (nil_tag ()) (V.tag value).W.lo base_local base state ();
    emit_def base_local)
