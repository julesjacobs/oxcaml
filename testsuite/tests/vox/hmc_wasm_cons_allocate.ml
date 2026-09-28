module B = Wasm_u32
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module K = Hmc_closure_ir
module A = Hmc_heap_allocate
module Machine = Hmc_heap_machine
module Image = Hmc_heap_image
module Bounds = Hmc_linear_bounds
module Index = Hmc_u32_index
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Write = Hmc_wasm_cons_write
module Memory = Hmc_wasm_cons_memory
module Advance = Hmc_wasm_allocation_advance
module Four = Wasm_four_words
let[@def] (emit @ total) (base_local : B.u32) (head_tag : B.u32) (head_payload : B.u32) (tail_tag : B.u32) (tail_payload : B.u32) =
  E.append (Write.emit base_local head_tag head_payload tail_tag tail_payload) (Advance.emit (Four.width ()) base_local)
type result = {allocation : A.allocation; state : X.state}
let (correct @ total) : (table : K.table) @ immutable -> (heap : Heap.heap) @ immutable ->
    (head : V.value) @ immutable -> (tail : V.value) @ immutable ->
    (state : X.state) @ immutable -> (base : B.u32) -> (limit : B.u32) -> (base_local : B.u32) ->
    (head_tag : B.u32) -> (head_payload : B.u32) -> (tail_tag : B.u32) -> (tail_payload : B.u32) ->
    {u : unit | Heap.valid table heap && Heap.object_valid table (Heap.view heap) (Heap.Cons (head, tail))
      && Heap.used heap = base && base + 32 <= limit && Image.related state.X.memory heap && Bounds.covers state.X.memory limit
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && L.get state.X.machine.E.locals head_tag === Some (S.I64 (V.tag head))
      && L.get state.X.machine.E.locals head_payload === Some (S.I64 (V.payload head))
      && L.get state.X.machine.E.locals tail_tag === Some (S.I64 (V.tag tail))
      && L.get state.X.machine.E.locals tail_payload === Some (S.I64 (V.payload tail))} ->
    {out : result | Machine.allocate heap limit (Heap.Cons (head, tail)) === A.Allocated out.allocation
      && A.correct table heap limit (Heap.Cons (head, tail)) (A.Allocated out.allocation)
      && X.run (emit base_local head_tag head_payload tail_tag tail_payload) state === X.Done out.state
      && Image.related out.state.X.memory out.allocation.A.heap && Bounds.covers out.state.X.memory limit
      && V.length out.state.X.memory === V.length state.X.memory
      && Hmc_linear_preservation.equal_prefix base state.X.memory out.state.X.memory
      && Hmc_linear_bytes.drop out.state.X.memory (S.add32 base (Four.width ())) === Hmc_linear_bytes.drop state.X.memory (S.add32 base (Four.width ()))
      && Heap.used out.allocation.A.heap = base + 32
      && out.allocation.A.reference === V.Cons_pointer base
      && L.get out.state.X.machine.E.locals base_local === Some (S.I32 (Heap.used out.allocation.A.heap))
      && L.replaced state.X.machine.E.locals base_local (S.I32 (Heap.used out.allocation.A.heap)) out.state.X.machine.E.locals
      && out.state.X.machine.E.stack === state.X.machine.E.stack} @ immutable =
  fun table heap head tail state base limit base_local head_tag head_payload tail_tag tail_payload premise ->
    let cells = D.S (D.S D.Z) in
    ghost_ (Heap.slots_def (Heap.Cons (head, tail));
      Index.represents_def cells 2; Index.represents_def (D.S D.Z) 1; Index.represents_def D.Z 0;
      Hmc_wasm_reservation.fits cells 2 base limit ());
    let allocation = A.sufficient table heap limit (Heap.Cons (head, tail)) () in
    ghost_ (A.correct_def table heap limit (Heap.Cons (head, tail)) (A.Allocated allocation);
      Hmc_wasm_reservation.span cells 2 base (Heap.used allocation.A.heap) ();
      Heap.reference_def (Heap.Cons (head, tail)) base);
    let reserved = Hmc_wasm_reservation.reserve cells 2 base limit () in
    (match reserved with
    | None -> unreachable_ ()
    | Some _ ->
      let written = Memory.correct head tail state base limit base_local head_tag head_payload tail_tag tail_payload () in
      let middle = {state with X.memory = written.Memory.memory} in
      let locals = Advance.correct 32 base_local base limit middle () in
      let final = {X.memory = written.Memory.memory; machine = {E.locals; stack = state.X.machine.E.stack}} in
      ghost_ (Machine.allocate_def heap limit (Heap.Cons (head, tail));
        Image.preserve table state.X.memory written.Memory.memory heap base ();
        Hmc_wasm_cons_stored.correct written.Memory.memory base written.Memory.bytes written.Memory.suffix head tail ();
        Image.schema_def (Heap.Cons (head, tail)); Image.stored_def written.Memory.memory base (Heap.Cons (head, tail));
        Hmc_heap_wire.corresponds_def (Heap.Cons (head, tail)) (Hmc_heap_wire.Cons (head, tail));
        Image.related_def written.Memory.memory allocation.A.heap;
        Four.width_def (); S.add32_def base 32;
        emit_def base_local head_tag head_payload tail_tag tail_payload;
        X.append_correct (Write.emit base_local head_tag head_payload tail_tag tail_payload) (Advance.emit 32 base_local) state);
      {allocation; state = final})
let (exhausted @ total) : (heap : Heap.heap) @ immutable -> (head : V.value) @ immutable -> (tail : V.value) @ immutable ->
    (base : B.u32) -> (limit : B.u32) ->
    {u : unit | Heap.used heap = base && base + 32 > limit} ->
    {u : unit | Machine.allocate heap limit (Heap.Cons (head, tail)) === A.Exhausted} @ ghost =
  fun heap head tail base limit premise -> ghost_ (
    let cells = D.S (D.S D.Z) in
    Heap.slots_def (Heap.Cons (head, tail));
    Index.represents_def cells 2; Index.represents_def (D.S D.Z) 1; Index.represents_def D.Z 0;
    Hmc_wasm_reservation.fits cells 2 base limit ();
    let _ = Hmc_heap_extent.reserve cells base limit in
    Machine.allocate_def heap limit (Heap.Cons (head, tail)))
