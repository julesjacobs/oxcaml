module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module V = Hmc_tagged_cell
module Simple = Hmc_heap_simple
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module M = Wasm_memory
module Value = Hmc_wasm_primitive_value
let[@def] (offset @ total) (unit : unit) : B.u32 = 40
let[@def] (prefix @ total) (left_offset : B.u32) (base_local : B.u32) =
  C.Next (I.Local_get base_local, C.Next (I.Local_get base_local, C.Next (I.I64_load (3, left_offset),
    C.Next (I.Local_get base_local, C.Next (I.I64_load (3, 40), C.Empty)))))
let[@def] (emit @ total) (operation : D.word_operation @ immutable) (left_offset : B.u32) (base_local : B.u32) =
  E.append (prefix left_offset base_local) (E.append (Value.emit operation) (C.Next (I.I64_store (3, 40), C.Empty)))
let (correct @ total) : (operation : D.word_operation) @ immutable -> (left_offset : B.u32) -> (base_local : B.u32) -> (base : B.u32) ->
    (left : W.t) @ immutable -> (right : W.t) @ immutable -> (state : X.state) @ immutable -> (after : B.bytes) @ immutable ->
    {u : unit | Wasm_locals.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && M.load state.X.memory base left_offset M.W64 === Some (S.I64 left)
      && M.load state.X.memory base (offset ()) M.W64 === Some (S.I64 right)
      && M.store state.X.memory base (offset ()) (S.I64 (V.payload (Simple.primitive operation left right))) === Some after} ->
    {u : unit | X.run (emit operation left_offset base_local) state === X.Done {X.memory = after; machine = state.X.machine}} @ ghost =
  fun operation left_offset base_local base left right state after premise -> ghost_ (
    offset_def (); prefix_def left_offset base_local; emit_def operation left_offset base_local;
    let rest = S.Push (S.I32 base, state.X.machine.E.stack) in
    let s1 = {state with X.machine = {state.X.machine with E.stack = rest}} in
    let s2 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 base, rest)}} in
    let s3 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I64 left, rest)}} in
    let s4 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 base, s3.X.machine.E.stack)}} in
    let loaded = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I64 right, s3.X.machine.E.stack)}} in
    let computed = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I64 (V.payload (Simple.primitive operation left right)), rest)}} in
    let c4 = C.Next (I.I64_load (3, 40), C.Empty) in
    let c3 = C.Next (I.Local_get base_local, c4) in
    let c2 = C.Next (I.I64_load (3, left_offset), c3) in
    let c1 = C.Next (I.Local_get base_local, c2) in
    X.run_def (prefix left_offset base_local) state; X.step_def (I.Local_get base_local) state; E.step_def (I.Local_get base_local) state.X.machine;
    X.run_def c1 s1; X.step_def (I.Local_get base_local) s1; E.step_def (I.Local_get base_local) s1.X.machine;
    X.run_def c2 s2; X.step_def (I.I64_load (3, left_offset)) s2; X.read_def M.W64 left_offset s2;
    X.run_def c3 s3; X.step_def (I.Local_get base_local) s3; E.step_def (I.Local_get base_local) s3.X.machine;
    X.run_def c4 s4; X.step_def (I.I64_load (3, 40)) s4; X.read_def M.W64 40 s4;
    X.run_def C.Empty loaded; Value.correct operation left right loaded rest ();
    let store = C.Next (I.I64_store (3, 40), C.Empty) in
    X.run_def store computed; X.step_def (I.I64_store (3, 40)) computed; X.write_def M.W64 40 computed;
    X.compatible_def (S.I64 (V.payload (Simple.primitive operation left right))) M.W64;
    M.width_def (S.I64 (V.payload (Simple.primitive operation left right)));
    X.run_def C.Empty {X.memory = after; machine = state.X.machine};
    X.append_correct (Value.emit operation) store loaded;
    X.append_correct (prefix left_offset base_local) (E.append (Value.emit operation) store) state)
