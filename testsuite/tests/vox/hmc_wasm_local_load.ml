module B = Wasm_u32
module W = Hmc_word64
module V = Hmc_tagged_cell
module H = Hmc_wasm_header_update
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module M = Wasm_memory
module F = Wasm_frame_literals
module Lit = Hmc_wasm_literal_load
module PC = Hmc_wasm_pc_update
let[@def] (tag_offset @ total) (unit : unit) : B.u32 = 32
let[@def] (payload_offset @ total) (unit : unit) : B.u32 = 40
let[@def] (emit @ total) (source_tag : B.u32) (source_payload : B.u32) (pc : W.limb) (base_local : B.u32) : C.t @ immutable =
  C.Next (I.Local_get base_local, C.Next (I.Local_get base_local, C.Next (I.I64_load (3, source_payload), C.Next (I.Local_get base_local, C.Next (I.Local_get base_local, C.Next (I.I64_load (3, source_tag), C.Next (I.I64_store (3, 32), C.Next (I.I64_store (3, 40), PC.emit pc base_local))))))))
let (correct @ total) : (source_tag : B.u32) -> (source_payload : B.u32) -> (pc : W.limb) -> (base_local : B.u32) ->
    (value : V.value) @ immutable -> (state : X.state) @ immutable -> (base : B.u32) -> (after : B.bytes) @ immutable ->
    {u : unit | Wasm_locals.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && M.load state.X.memory base source_tag M.W64 === Some (S.I64 (V.tag value))
      && M.load state.X.memory base source_payload M.W64 === Some (S.I64 (V.payload value))
      && F.apply (Lit.writes pc value) state.X.memory base === Some after} ->
    {u : unit | X.run (emit source_tag source_payload pc base_local) state === X.Done {X.memory = after; machine = state.X.machine}} @ ghost =
  fun source_tag source_payload pc base_local value state base after premise -> ghost_ (
    emit_def source_tag source_payload pc base_local; Lit.writes_def pc value;
    H.number_def pc; PC.offset_def ();
    let next = H.number pc in
    F.apply_def (Lit.writes pc value) state.X.memory base;
    let tagged = match M.store state.X.memory base 32 (S.I64 (V.tag value)) with
      | Some memory -> memory | None -> unreachable_ () in
    F.apply_def (F.Write (40, V.payload value, F.Write (8, next, F.End))) tagged base;
    let valued = match M.store tagged base 40 (S.I64 (V.payload value)) with
      | Some memory -> memory | None -> unreachable_ () in
    F.apply_def (F.Write (8, next, F.End)) valued base;
    (match M.store valued base 8 (S.I64 next) with None -> () | Some memory -> F.apply_def F.End memory base);
    let s0 = state in
    let s1 = {X.memory = state.X.memory; machine = {E.locals = state.X.machine.E.locals; stack = S.Push (S.I32 base, state.X.machine.E.stack)}} in
    let s2 = {X.memory = state.X.memory; machine = {E.locals = state.X.machine.E.locals; stack = S.Push (S.I32 base, s1.X.machine.E.stack)}} in
    let s3 = {X.memory = state.X.memory; machine = {E.locals = state.X.machine.E.locals; stack = S.Push (S.I64 (V.payload value), s1.X.machine.E.stack)}} in
    let s4 = {X.memory = state.X.memory; machine = {E.locals = state.X.machine.E.locals; stack = S.Push (S.I32 base, s3.X.machine.E.stack)}} in
    let s5 = {X.memory = state.X.memory; machine = {E.locals = state.X.machine.E.locals; stack = S.Push (S.I32 base, s4.X.machine.E.stack)}} in
    let s6 = {X.memory = state.X.memory; machine = {E.locals = state.X.machine.E.locals; stack = S.Push (S.I64 (V.tag value), s4.X.machine.E.stack)}} in
    let s7 = {X.memory = tagged; machine = {E.locals = state.X.machine.E.locals; stack = s3.X.machine.E.stack}} in
    let c8 = PC.emit pc base_local in
    let c7 = C.Next (I.I64_store (3, 40), c8) in
    let c6 = C.Next (I.I64_store (3, 32), c7) in
    let c5 = C.Next (I.I64_load (3, source_tag), c6) in
    let c4 = C.Next (I.Local_get base_local, c5) in
    let c3 = C.Next (I.Local_get base_local, c4) in
    let c2 = C.Next (I.I64_load (3, source_payload), c3) in
    let c1 = C.Next (I.Local_get base_local, c2) in
    let c0 = C.Next (I.Local_get base_local, c1) in
    X.run_def c0 s0; X.step_def (I.Local_get base_local) s0;
    E.step_def (I.Local_get base_local) s0.X.machine;
    X.run_def c1 s1; X.step_def (I.Local_get base_local) s1;
    E.step_def (I.Local_get base_local) s1.X.machine;
    X.run_def c2 s2; X.step_def (I.I64_load (3, source_payload)) s2;
    X.read_def M.W64 source_payload s2;
    X.run_def c3 s3; X.step_def (I.Local_get base_local) s3;
    E.step_def (I.Local_get base_local) s3.X.machine;
    X.run_def c4 s4; X.step_def (I.Local_get base_local) s4;
    E.step_def (I.Local_get base_local) s4.X.machine;
    X.run_def c5 s5; X.step_def (I.I64_load (3, source_tag)) s5;
    X.read_def M.W64 source_tag s5;
    X.run_def c6 s6; X.step_def (I.I64_store (3, 32)) s6;
    M.width_def (S.I64 (V.tag value)); X.compatible_def (S.I64 (V.tag value)) M.W64; X.write_def M.W64 32 s6;
    X.run_def c7 s7; X.step_def (I.I64_store (3, 40)) s7;
    M.width_def (S.I64 (V.payload value)); X.compatible_def (S.I64 (V.payload value)) M.W64; X.write_def M.W64 40 s7;
    PC.correct pc base_local {X.memory = valued; machine = state.X.machine} base after ())
