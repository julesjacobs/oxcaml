module B = Wasm_u32
module C = Wasm_code
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
module Advance_prefix = Hmc_wasm_allocation_advance_prefix
module Allocate = Hmc_wasm_cons_allocate
module Closure = Hmc_wasm_cons_prefix
module Write_prefix = Wasm_frame_write_prefix
module Prefix = Wasm_instruction_prefix
module Range = Hmc_wasm_range_copy
module Plan = Wasm_parallel_copy
module Bytes = Hmc_linear_bytes
module P = Hmc_linear_preservation

module Four = Wasm_four_words
let (correct @ total) : (table : K.table) @ immutable -> (heap : Heap.heap) @ immutable ->
    (head : V.value) @ immutable -> (tail : V.value) @ immutable ->
    (state : X.state) @ immutable -> (base : B.u32) -> (limit : B.u32) -> (base_local : B.u32) ->
    (head_tag : B.u32) -> (head_payload : B.u32) -> (tail_tag : B.u32) -> (tail_payload : B.u32) -> (fuel : C.count) @ immutable ->
    {u : unit | Heap.valid table heap && Heap.object_valid table (Heap.view heap) (Heap.Cons (head, tail))
      && Heap.used heap = base && base + 32 <= limit && Image.related state.X.memory heap && Bounds.covers state.X.memory limit
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && L.get state.X.machine.E.locals head_tag === Some (S.I64 (V.tag head))
      && L.get state.X.machine.E.locals head_payload === Some (S.I64 (V.payload head))
      && L.get state.X.machine.E.locals tail_tag === Some (S.I64 (V.tag tail))
      && L.get state.X.machine.E.locals tail_payload === Some (S.I64 (V.payload tail))} ->
    {out : Allocate.result | Machine.allocate heap limit (Heap.Cons (head, tail)) === A.Allocated out.Allocate.allocation
      && A.correct table heap limit (Heap.Cons (head, tail)) (A.Allocated out.Allocate.allocation)
      && Heap.used out.Allocate.allocation.A.heap = base + (Four.width ())
      && X.run (Allocate.emit base_local head_tag head_payload tail_tag tail_payload) state === X.Done out.Allocate.state
      && match X.run (Prefix.take fuel (Allocate.emit base_local head_tag head_payload tail_tag tail_payload)) state with
        | X.Done current -> Bounds.covers current.X.memory limit
          && P.equal_prefix base state.X.memory current.X.memory
          && Bytes.drop state.X.memory (S.add32 base (Four.width ()))
            === Bytes.drop current.X.memory (S.add32 base (Four.width ()))
          && V.length current.X.memory === V.length state.X.memory
          && ((current.X.machine.E.locals === state.X.machine.E.locals
              && Image.related current.X.memory heap
              && (Write_prefix.partial (Write.writes head_tag head_payload tail_tag tail_payload) state.X.memory base state.X.machine.E.locals current.X.memory
                || current.X.memory === out.Allocate.state.X.memory))
            || (L.replaced state.X.machine.E.locals base_local (S.I32 (S.add32 base (Four.width ()))) current.X.machine.E.locals
              && L.get current.X.machine.E.locals base_local === Some (S.I32 (S.add32 base (Four.width ())))
              && current.X.memory === out.Allocate.state.X.memory
              && Image.related current.X.memory out.Allocate.allocation.A.heap))
        | _ -> false} @ ghost =
  fun table heap head tail state base limit base_local head_tag head_payload tail_tag tail_payload fuel premise -> ghost_ (
    let out = Allocate.correct table heap head tail state base limit base_local head_tag head_payload tail_tag tail_payload () in
    let written = Memory.correct head tail state base limit base_local head_tag head_payload tail_tag tail_payload () in
    let middle = {X.memory = written.Memory.memory; machine = state.X.machine} in
    let write = Write.emit base_local head_tag head_payload tail_tag tail_payload in
    let advance = Advance.emit (Four.width ()) base_local in
    Four.width_def ();
    let _ = Advance.correct (Four.width ()) base_local base limit middle () in
    Allocate.emit_def base_local head_tag head_payload tail_tag tail_payload;
    X.append_correct write advance state;
    Prefix.run_append fuel write advance state middle ();
    (match Prefix.remaining fuel write with
    | None ->
      S.add32_def base (Four.width ());
      Closure.correct table heap head tail state base (base + 32) limit base_local head_tag head_payload tail_tag tail_payload fuel ()
    | Some rest ->
      Image.preserve table state.X.memory middle.X.memory heap base ();
      Advance_prefix.correct (Four.width ()) base_local base limit middle rest ());
    out)
