module B = Wasm_u32
module W = Hmc_word64
module I = Wasm_instruction
module C = Wasm_code
module V = Hmc_tagged_cell
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module M = Wasm_memory
module Header = Hmc_wasm_header_update
let[@def] (payload @ total) (frame_local : B.u32) (heap_local : B.u32) =
  C.Next (I.Local_get frame_local, C.Next (I.Local_get heap_local, C.Next (I.I32_const 32,
    C.Next (I.Plain I.I32_sub, C.Next (I.Plain I.I64_extend_i32_u, C.Next (I.I64_store (3, 40), C.Empty))))))
let[@def] (tag @ total) (unit : unit) : W.t @ immutable = {W.lo = 3; hi = 0}
let[@def] (tag_offset @ total) (unit : unit) : B.u32 = 32
let[@def] (payload_offset @ total) (unit : unit) : B.u32 = 40
let[@def] (emit @ total) (frame_local : B.u32) (heap_local : B.u32) =
  E.append (payload frame_local heap_local) (Wasm_immediate_write.emit (tag_offset ()) frame_local (tag ()))
let (correct @ total) : (frame_local : B.u32) -> (heap_local : B.u32) -> (frame_base : B.u32) ->
    (heap_base : B.u32) -> (heap_end : B.u32) -> (state : X.state) @ immutable -> (middle : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    {u : unit | heap_end = heap_base + 32
      && Wasm_locals.get state.X.machine.E.locals frame_local === Some (S.I32 frame_base)
      && Wasm_locals.get state.X.machine.E.locals heap_local === Some (S.I32 heap_end)
      && M.store state.X.memory frame_base (payload_offset ()) (S.I64 (V.payload (V.Cons_pointer heap_base))) === Some middle
      && M.store middle frame_base (tag_offset ()) (S.I64 (V.tag (V.Cons_pointer heap_base))) === Some after} ->
    {u : unit | X.run (emit frame_local heap_local) state === X.Done {X.memory = after; machine = state.X.machine}} @ ghost =
  fun frame_local heap_local frame_base heap_base heap_end state middle after premise -> ghost_ (
    payload_def frame_local heap_local; emit_def frame_local heap_local;
    tag_def (); tag_offset_def (); payload_offset_def (); V.tag_def (V.Cons_pointer heap_base); V.payload_def (V.Cons_pointer heap_base);
    S.sub32_def heap_end 32;
    let s1 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 frame_base, state.X.machine.E.stack)}} in
    let s2 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 heap_end, s1.X.machine.E.stack)}} in
    let s3 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 32, s2.X.machine.E.stack)}} in
    let s4 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 heap_base, s1.X.machine.E.stack)}} in
    let s5 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I64 (V.payload (V.Cons_pointer heap_base)), s1.X.machine.E.stack)}} in
    let c5 = C.Next (I.I64_store (3, 40), C.Empty) in
    let c4 = C.Next (I.Plain I.I64_extend_i32_u, c5) in
    let c3 = C.Next (I.Plain I.I32_sub, c4) in
    let c2 = C.Next (I.I32_const 32, c3) in
    let c1 = C.Next (I.Local_get heap_local, c2) in
    X.run_def (payload frame_local heap_local) state; X.step_def (I.Local_get frame_local) state; E.step_def (I.Local_get frame_local) state.X.machine;
    X.run_def c1 s1; X.step_def (I.Local_get heap_local) s1; E.step_def (I.Local_get heap_local) s1.X.machine;
    X.run_def c2 s2; X.step_def (I.I32_const 32) s2; E.step_def (I.I32_const 32) s2.X.machine; S.step_def (I.I32_const 32) s2.X.machine.E.stack;
    X.run_def c3 s3; X.step_def (I.Plain I.I32_sub) s3; E.step_def (I.Plain I.I32_sub) s3.X.machine; S.step_def (I.Plain I.I32_sub) s3.X.machine.E.stack;
    X.run_def c4 s4; X.step_def (I.Plain I.I64_extend_i32_u) s4; E.step_def (I.Plain I.I64_extend_i32_u) s4.X.machine; S.step_def (I.Plain I.I64_extend_i32_u) s4.X.machine.E.stack;
    X.run_def c5 s5; X.step_def (I.I64_store (3, 40)) s5; X.write_def M.W64 40 s5; X.compatible_def (S.I64 (V.payload (V.Cons_pointer heap_base))) M.W64;
    M.width_def (S.I64 (V.payload (V.Cons_pointer heap_base)));
    let next = {X.memory = middle; machine = state.X.machine} in
    X.run_def C.Empty next;
    Wasm_immediate_write.correct 32 frame_local (tag ()) next frame_base after ();
    X.append_correct (payload frame_local heap_local) (Wasm_immediate_write.emit 32 frame_local (tag ())) state)
