module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module V = Hmc_tagged_cell
module Simple = Hmc_heap_simple
module E = Wasm_execution
module S = Wasm_scalar
module X = Wasm_memory_execution
module M = Wasm_memory
module Value = Hmc_wasm_primitive_value
module Payload = Hmc_wasm_primitive_payload
module Write = Wasm_immediate_write
let[@def] (tag_offset @ total) (unit : unit) : B.u32 = 32
let[@def] (emit @ total) (operation : D.word_operation @ immutable) (left_offset : B.u32) (base_local : B.u32) =
  E.append (Payload.emit operation left_offset base_local) (Write.emit (tag_offset ()) base_local (Value.tag operation))
let (correct @ total) : (operation : D.word_operation) @ immutable -> (left_offset : B.u32) -> (base_local : B.u32) -> (base : B.u32) ->
    (left : W.t) @ immutable -> (right : W.t) @ immutable -> (state : X.state) @ immutable ->
    (middle : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    {u : unit | Wasm_locals.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && M.load state.X.memory base left_offset M.W64 === Some (S.I64 left)
      && M.load state.X.memory base (Payload.offset ()) M.W64 === Some (S.I64 right)
      && M.store state.X.memory base (Payload.offset ()) (S.I64 (V.payload (Simple.primitive operation left right))) === Some middle
      && M.store middle base (tag_offset ()) (S.I64 (V.tag (Simple.primitive operation left right))) === Some after} ->
    {u : unit | X.run (emit operation left_offset base_local) state === X.Done {X.memory = after; machine = state.X.machine}} @ ghost =
  fun operation left_offset base_local base left right state middle after premise -> ghost_ (
    Simple.primitive_def operation left right; V.tag_def (Simple.primitive operation left right); Value.tag_def operation;
    Payload.correct operation left_offset base_local base left right state middle ();
    Write.correct (tag_offset ()) base_local (Value.tag operation) {X.memory = middle; machine = state.X.machine} base after ();
    emit_def operation left_offset base_local;
    X.append_correct (Payload.emit operation left_offset base_local) (Write.emit (tag_offset ()) base_local (Value.tag operation)) state)
