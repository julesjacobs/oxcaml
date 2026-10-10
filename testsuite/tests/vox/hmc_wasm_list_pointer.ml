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
module Read = Wasm_pointer_read
let[@def] (offset @ total) (u : unit) : B.u32 = 40
let (correct @ total) : (state : X.state) @ immutable -> (base : B.u32) -> (base_local : B.u32) -> (object_local : B.u32) ->
    (address : B.u32) -> (pc : W.limb) -> (current : V.value) @ immutable -> (body : Heap.cells) @ immutable ->
    (frame : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    {u : unit | base <= 4294967248 && L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && L.can_set state.X.machine.E.locals object_local (S.I32 address)
      && Hmc_linear_bytes.drop state.X.memory base === Some frame
      && Wire.decode_cells (D.S (D.S (D.S (Heap.length body)))) frame ===
        Some (Heap.Cell (V.Word (Header.number pc), Heap.Cell (current, Heap.Cell (V.Cons_pointer address, body))), suffix)} ->
    {out : S.stack | X.run (Read.emit (offset ()) base_local object_local) state === X.Done {X.memory = state.X.memory; machine = {E.locals = out; stack = state.X.machine.E.stack}}
      && L.get out object_local === Some (S.I32 address)
      && L.same_types state.X.machine.E.locals out && L.replaced state.X.machine.E.locals object_local (S.I32 address) out} @ immutable =
  fun state base base_local object_local address pc current body frame suffix premise ->
    ghost_ (
      let value = V.Cons_pointer address in
      let cells = Heap.Cell (V.Word (Header.number pc), Heap.Cell (current, Heap.Cell (value, body))) in
      Simple.lookup_def cells (D.S (D.S D.Z));
      Simple.lookup_def (Heap.Cell (current, Heap.Cell (value, body))) (D.S D.Z);
      Simple.lookup_def (Heap.Cell (value, body)) D.Z;
      Index.represents_def D.Z 0; Index.represents_def (D.S D.Z) 1; Index.represents_def (D.S (D.S D.Z)) 2;
      Hmc_wasm_cells_read.correct state.X.memory base frame (D.S (D.S (D.S (Heap.length body)))) cells suffix (D.S (D.S D.Z)) 2 32 40 value ();
      V.payload_def value; Header.number_def address; offset_def ());
    Read.correct (offset ()) base_local object_local state base address ()
