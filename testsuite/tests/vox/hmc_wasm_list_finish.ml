module B = Wasm_u32
module Lower = Hmc_wasm_relayout
module Capture = Hmc_wasm_list_capture
module Copy = Wasm_parallel_copy
module Write = Wasm_frame_write
module PC = Hmc_wasm_pc_update
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
let[@def] (writes @ total) (slots : Capture.slots @ immutable) =
  Write.Write (48, slots.Capture.head_tag,
    Write.Write (56, slots.Capture.head_payload,
      Write.Write (64, slots.Capture.tail_tag,
        Write.Write (72, slots.Capture.tail_payload, Write.End))))
let[@def] (emit @ total) (fragment : Lower.fragment @ immutable) (base_local : B.u32) (slots : Capture.slots @ immutable) =
  E.append (Copy.emit fragment.Lower.copies base_local)
    (E.append (Write.emit (writes slots) base_local) (PC.emit fragment.Lower.pc base_local))
let (correct @ total) : (fragment : Lower.fragment) @ immutable -> (base_local : B.u32) -> (slots : Capture.slots) @ immutable ->
    (state : X.state) @ immutable -> (base : B.u32) ->
    (copied : B.bytes) @ immutable -> (written : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    {u : unit | Wasm_locals.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && Copy.apply fragment.Lower.copies state.X.memory base === Some copied
      && Write.apply (writes slots) copied base state.X.machine.E.locals === Some written
      && Lower.finish written base fragment.Lower.pc === Some after} ->
    {u : unit | X.run (emit fragment base_local slots) state === X.Done {X.memory = after; machine = state.X.machine}} @ ghost =
  fun fragment base_local slots state base copied written after premise -> ghost_ (
    Copy.correct fragment.Lower.copies base_local state base copied ();
    let middle = {X.memory = copied; machine = state.X.machine} in
    Write.correct (writes slots) base_local middle base written ();
    Lower.finish_def written base fragment.Lower.pc;
    PC.correct fragment.Lower.pc base_local {X.memory = written; machine = state.X.machine} base after ();
    X.append_correct (Write.emit (writes slots) base_local) (PC.emit fragment.Lower.pc base_local) middle;
    emit_def fragment base_local slots;
    X.append_correct (Copy.emit fragment.Lower.copies base_local)
      (E.append (Write.emit (writes slots) base_local) (PC.emit fragment.Lower.pc base_local)) state)
