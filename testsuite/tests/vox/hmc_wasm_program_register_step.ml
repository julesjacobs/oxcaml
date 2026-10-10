module B = Wasm_u32
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module G = Wasm_globals
module L = Wasm_locals
module GE = Wasm_global_execution
module T = Wasm_control
module F = Wasm_functions
module M = Wasm_calls
module H = Hmc_heap_objects
module D = Hm_declarative
module V = Hmc_tagged_cell
module Wire = Hmc_heap_wire
module Bytes = Hmc_linear_bytes
module Header = Hmc_wasm_header_update
module Read_header = Hmc_wasm_program_header
module Lower = Hmc_wasm_program_lower
module Block = Hmc_wasm_program_block
module Emit = Hmc_wasm_program_emit
module Assembly = Hmc_wasm_program_functions
module Runtime = Hmc_wasm_program_runtime
module Registers = Hmc_wasm_program_registers
module Round = Hmc_wasm_program_roundtrip
module Dispatch = Hmc_wasm_program_dispatch
let (controls @ total) : (registers : Registers.registers) @ immutable ->
    {u : unit | G.get (Registers.globals registers) (Dispatch.frame_global ()) === Some (S.I32 registers.Registers.frame)
      && G.get (Registers.globals registers) (Dispatch.status_global ()) === Some (S.I32 registers.Registers.status)} @ ghost =
  fun registers -> ghost_ (
    Registers.globals_def registers; Registers.values_def registers; Dispatch.frame_global_def (); Dispatch.status_global_def ();
    let g7 = S.Push (S.I64 registers.Registers.payload, S.Empty) in
    let g6 = S.Push (S.I64 registers.Registers.tag, g7) in
    let g5 = S.Push (S.I32 registers.Registers.status, g6) in
    let g4 = S.Push (S.I32 registers.Registers.stack_limit, g5) in
    let g3 = S.Push (S.I32 registers.Registers.top, g4) in
    let g2 = S.Push (S.I32 registers.Registers.heap_limit, g3) in
    let g1 = S.Push (S.I32 registers.Registers.heap, g2) in
    let g0 = S.Push (S.I32 registers.Registers.frame, g1) in
    G.get_def (Registers.globals registers) 0; G.get_def (Registers.globals registers) 5;
    L.get_def g0 0; L.get_def g0 5; L.get_def g1 4; L.get_def g2 3; L.get_def g3 2; L.get_def g4 1; L.get_def g5 0)
let (correct @ total) : (program : Lower.program) @ immutable -> (fragment : Block.fragment) @ immutable ->
    (module_ : F.module_) @ immutable -> (table_base : B.u32) -> (stack_base : B.u32) ->
    (before : Registers.registers) @ immutable -> (after : Registers.registers) @ immutable ->
    (memory : B.bytes) @ immutable -> (bytes : B.bytes) @ immutable -> (cells : H.cells) @ immutable -> (suffix : B.bytes) @ immutable -> (capacity : C.count) @ immutable -> (pc : B.u32) -> (index : B.u32) ->
    (body_fuel : C.count) @ immutable -> (final : X.state) @ immutable ->
    {u : unit | before.Registers.frame <= 4294967280 && Bytes.drop memory before.Registers.frame === Some bytes
      && Wire.decode_cells (D.S (H.length cells)) bytes === Some (H.Cell (V.Word (Header.number pc), cells), suffix)
      && F.signature module_.F.signatures (Dispatch.void_signature ()) === Some F.Void
      && F.element module_.F.table pc === Some index
      && F.lookup module_.F.functions index === Some (Assembly.function_ program fragment (Runtime.config table_base stack_base))
      && after.Registers.frame = before.Registers.frame && after.Registers.heap_limit = before.Registers.heap_limit
      && after.Registers.stack_limit = before.Registers.stack_limit
      && Registers.exports final.X.machine.E.locals after && final.X.machine.E.stack === S.Empty
      && T.run body_fuel
        {T.code = Emit.emit program fragment (Runtime.config table_base stack_base).Assembly.locals table_base stack_base;
          labels = Round.labels (Runtime.config table_base stack_base);
          state = {X.memory; machine = {E.locals = Registers.locals before; stack = S.Empty}}}
        === T.Running {T.code = T.Empty; labels = Round.labels (Runtime.config table_base stack_base); state = final}} ->
    {u : unit | M.run (Dispatch.cost (Round.cost (Runtime.config table_base stack_base) body_fuel) after.Registers.status) module_
      (Dispatch.loop (Registers.globals before) memory (C.Succ capacity)) ===
      (if after.Registers.status = 0 then M.Running (Dispatch.loop (Registers.globals after) final.X.memory (C.Succ capacity))
      else M.Finished {GE.globals = Registers.globals after; execution = {X.memory = final.X.memory;
        machine = {E.locals = S.Empty; stack = S.Push (S.I32 after.Registers.status, S.Empty)}}})} @ ghost =
  fun program fragment module_ table_base stack_base before after memory bytes cells suffix capacity pc index body_fuel final premise -> ghost_ (
    Read_header.correct memory before.Registers.frame bytes pc cells suffix ();
    Registers.globals_def before; Runtime.config_def table_base stack_base;
    let imported = Registers.import before (Registers.globals before) memory table_base stack_base () in
    let exported = Registers.export before after final table_base stack_base () in
    controls before; controls after;
    Dispatch.generated_iteration program fragment (Runtime.config table_base stack_base) module_ (Registers.globals before)
      memory capacity before.Registers.frame pc index imported final {GE.globals = exported; execution = final} body_fuel after.Registers.status ())
let (complete @ total) : (program : Lower.program) @ immutable -> (fragment : Block.fragment) @ immutable ->
    (module_ : F.module_) @ immutable -> (table_base : B.u32) -> (stack_base : B.u32) ->
    (before : Registers.registers) @ immutable ->
    (memory : B.bytes) @ immutable -> (bytes : B.bytes) @ immutable -> (cells : H.cells) @ immutable -> (suffix : B.bytes) @ immutable -> (capacity : C.count) @ immutable -> (pc : B.u32) -> (index : B.u32) ->
    (body_fuel : C.count) @ immutable -> (final : X.state) @ immutable ->
    {u : unit | before.Registers.frame <= 4294967280 && Bytes.drop memory before.Registers.frame === Some bytes
      && Wire.decode_cells (D.S (H.length cells)) bytes === Some (H.Cell (V.Word (Header.number pc), cells), suffix)
      && F.signature module_.F.signatures (Dispatch.void_signature ()) === Some F.Void
      && F.element module_.F.table pc === Some index
      && F.lookup module_.F.functions index === Some (Assembly.function_ program fragment (Runtime.config table_base stack_base))
      && final.X.machine.E.stack === S.Empty
      && T.run body_fuel
        {T.code = Emit.emit program fragment (Runtime.config table_base stack_base).Assembly.locals table_base stack_base;
          labels = Round.labels (Runtime.config table_base stack_base);
          state = {X.memory; machine = {E.locals = Registers.locals before; stack = S.Empty}}}
        === T.Running {T.code = T.Empty; labels = Round.labels (Runtime.config table_base stack_base); state = final}} ->
    {after : Registers.registers | Registers.exports final.X.machine.E.locals after
      && after.Registers.frame = before.Registers.frame && after.Registers.heap_limit = before.Registers.heap_limit
      && after.Registers.stack_limit = before.Registers.stack_limit
      && M.run (Dispatch.cost (Round.cost (Runtime.config table_base stack_base) body_fuel) after.Registers.status) module_
      (Dispatch.loop (Registers.globals before) memory (C.Succ capacity)) ===
      (if after.Registers.status = 0 then M.Running (Dispatch.loop (Registers.globals after) final.X.memory (C.Succ capacity))
      else M.Finished {GE.globals = Registers.globals after; execution = {X.memory = final.X.memory;
        machine = {E.locals = S.Empty; stack = S.Push (S.I32 after.Registers.status, S.Empty)}}})} @ immutable =
  fun program fragment module_ table_base stack_base before memory bytes cells suffix capacity pc index body_fuel final premise ->
    let config = Runtime.config table_base stack_base in
    let initial = {T.code = Emit.emit program fragment config.Assembly.locals table_base stack_base;
      labels = Round.labels config; state = {X.memory; machine = {E.locals = Registers.locals before; stack = S.Empty}}} in
    let done_ = {T.code = T.Empty; labels = Round.labels config; state = final} in
    let after = Hmc_wasm_program_register_export.read before body_fuel initial done_ () in
    ghost_ (correct program fragment module_ table_base stack_base before after memory bytes cells suffix capacity pc index body_fuel final ());
    after
