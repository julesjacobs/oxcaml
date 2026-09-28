module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module F = Hmc_heap_frame
module H = Hmc_heap_objects
module V = Hmc_tagged_cell
module Codec = Hmc_pointer_frame_codec
module Index = Hmc_u32_index
module Plan = Wasm_parallel_copy
module Copy = Wasm_cross_copy
module Restore = Hmc_wasm_frame_restore
module Retreat = Hmc_wasm_stack_retreat
module Return = Hmc_wasm_return_result
module Cells = Hmc_wasm_call_save_memory
module Wire = Hmc_heap_wire
module Memory = Wasm_scatter_memory
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module M = Wasm_memory
module Bytes = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
module P = Hmc_linear_preservation
let[@def] (emit @ total) (plan : Plan.plan @ immutable) (width : B.u32) (active_local : B.u32) (top_local : B.u32) =
  E.append (Retreat.emit width top_local) (E.append (Return.emit active_local top_local) (Copy.emit plan top_local active_local))
type result = {state : X.state; restored : Memory.result; returned : Return.result}
let (correct @ total) : (signature : G.signature) @ immutable -> (pc : D.index) @ immutable -> (encoded_pc : B.u32) ->
    (saved : F.activation) @ immutable -> (rest : H.cells) @ immutable -> (padding : H.cells) @ immutable -> (value : V.value) @ immutable ->
    (plan : Plan.plan) @ immutable -> (count : Hmc_wasm_relayout.count) -> (state : X.state) @ immutable ->
    (active : B.u32) -> (caller : B.u32) -> (top : B.u32) -> (limit : B.u32) ->
    (before : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable -> (active_local : B.u32) -> (top_local : B.u32) ->
    {u : unit | Index.represents (H.length (H.Cell (saved.F.current, H.Cell (saved.F.accumulator, rest)))) count
      && Restore.matches plan (H.length (H.Cell (saved.F.current, H.Cell (saved.F.accumulator, rest))))
      && count >= 2 && caller + 16 + 16 * count = top && top <= limit && active + 16 + 16 * count <= limit
      && active_local <> top_local && Bounds.covers state.X.memory limit
      && Bytes.drop state.X.memory caller === Some before
      && Wire.decode_cells (H.length (Cells.cells encoded_pc saved.F.current saved.F.accumulator rest)) before ===
        Some (Cells.cells encoded_pc saved.F.current saved.F.accumulator rest, suffix)
      && Codec.decode signature pc (H.Cell (saved.F.current, H.Cell (saved.F.accumulator, rest))) === Some (saved, padding)
      && M.load state.X.memory active (Return.tag_offset ()) M.W64 === Some (S.I64 (V.tag value))
      && M.load state.X.memory active (Return.payload_offset ()) M.W64 === Some (S.I64 (V.payload value))
      && L.get state.X.machine.E.locals active_local === Some (S.I32 active)
      && L.get state.X.machine.E.locals top_local === Some (S.I32 top)} ->
    {out : result | X.run (emit plan (Restore.width count) active_local top_local) state === X.Done out.state
      && out.state.X.memory === out.restored.Memory.memory && out.state.X.machine.E.stack === state.X.machine.E.stack
      && L.replaced state.X.machine.E.locals top_local (S.I32 caller) out.state.X.machine.E.locals
      && L.get out.state.X.machine.E.locals top_local === Some (S.I32 caller)
      && L.get out.state.X.machine.E.locals active_local === Some (S.I32 active)
      && Bytes.drop out.state.X.memory active === Some out.restored.Memory.bytes
      && Wire.decode_cells (H.length (Cells.cells encoded_pc saved.F.current value rest)) out.restored.Memory.bytes ===
        Some (Cells.cells encoded_pc saved.F.current value rest, out.restored.Memory.suffix)
      && Codec.decode signature pc (H.Cell (saved.F.current, H.Cell (value, rest))) === Some ({saved with F.accumulator = value}, padding)
      && P.equal_prefix active out.returned.Return.memory out.state.X.memory
      && not (Bytes.drop state.X.memory (S.add32 caller (Return.end_offset ())) === None)
      && Bytes.drop out.returned.Return.memory (S.add32 caller (Return.end_offset ())) === Bytes.drop state.X.memory (S.add32 caller (Return.end_offset ()))
      && P.equal_prefix caller state.X.memory out.returned.Return.memory
      && Bytes.drop out.returned.Return.memory (S.add32 active (Restore.width count)) === Some out.restored.Memory.suffix
      && Bytes.drop out.state.X.memory (S.add32 active (Restore.width count)) === Some out.restored.Memory.suffix
      && V.length out.state.X.memory === V.length state.X.memory && Bounds.covers out.state.X.memory limit} @ immutable =
  fun signature pc encoded_pc saved rest padding value plan count state active caller top limit before suffix active_local top_local premise ->
    ghost_ (Restore.width_def count);
    let locals = Retreat.correct (Restore.width count) top_local top state () in
    ghost_ (S.sub32_def top (Restore.width count); L.other_local state.X.machine.E.locals top_local (S.I32 caller) locals active_local ());
    let popped = {X.memory = state.X.memory; machine = {E.locals; stack = state.X.machine.E.stack}} in
    let returned = Return.update signature pc encoded_pc saved rest padding value popped active caller limit before suffix active_local top_local () in
    let middle = {X.memory = returned.Return.memory; machine = popped.X.machine} in
    let payload = H.Cell (saved.F.current, H.Cell (value, rest)) in
    ghost_ (H.length_def payload; H.length_def (H.Cell (value, rest));
      H.length_def (H.Cell (saved.F.current, H.Cell (saved.F.accumulator, rest))); H.length_def (H.Cell (saved.F.accumulator, rest));
      Cells.cells_def encoded_pc saved.F.current value rest);
    let restored = Restore.correct plan encoded_pc payload count middle caller active limit returned.Return.bytes suffix top_local active_local () in
    let final = {X.memory = restored.Memory.memory; machine = popped.X.machine} in
    ghost_ (emit_def plan (Restore.width count) active_local top_local;
      X.append_correct (Return.emit active_local top_local) (Copy.emit plan top_local active_local) popped;
      X.append_correct (Retreat.emit (Restore.width count) top_local) (E.append (Return.emit active_local top_local) (Copy.emit plan top_local active_local)) state);
    {state = final; restored; returned}
