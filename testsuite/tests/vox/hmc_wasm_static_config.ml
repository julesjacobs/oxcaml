module B = Wasm_u32
module F = Wasm_functions
module V = Wasm_static_types
module G = Wasm_globals
module L = Wasm_locals
module S = Wasm_scalar
module R = Wasm_global_registers
module Registers = Hmc_wasm_program_registers
module Runtime = Hmc_wasm_program_runtime
module Assembly = Hmc_wasm_program_functions
module Binding = Wasm_static_registers
let (local @ total) : (index : B.u32) -> {u : unit | index < 18} ->
    {u : unit | V.local (Runtime.local_types ()) index === Some (if index < 12 then V.I32 else V.I64)} @ ghost =
  fun index premise -> ghost_ (
    Runtime.local_types_def ();
    (if index >= 0 then V.local_def (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals))))))))))))))))))) (index - 0) else ());
    (if index >= 1 then V.local_def (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals)))))))))))))))))) (index - 1) else ());
    (if index >= 2 then V.local_def (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals))))))))))))))))) (index - 2) else ());
    (if index >= 3 then V.local_def (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals)))))))))))))))) (index - 3) else ());
    (if index >= 4 then V.local_def (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals))))))))))))))) (index - 4) else ());
    (if index >= 5 then V.local_def (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals)))))))))))))) (index - 5) else ());
    (if index >= 6 then V.local_def (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals))))))))))))) (index - 6) else ());
    (if index >= 7 then V.local_def (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals)))))))))))) (index - 7) else ());
    (if index >= 8 then V.local_def (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals))))))))))) (index - 8) else ());
    (if index >= 9 then V.local_def (F.Local32 (F.Local32 (F.Local32 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals)))))))))) (index - 9) else ());
    (if index >= 10 then V.local_def (F.Local32 (F.Local32 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals))))))))) (index - 10) else ());
    (if index >= 11 then V.local_def (F.Local32 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals)))))))) (index - 11) else ());
    (if index >= 12 then V.local_def (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals))))))) (index - 12) else ());
    (if index >= 13 then V.local_def (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals)))))) (index - 13) else ());
    (if index >= 14 then V.local_def (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals))))) (index - 14) else ());
    (if index >= 15 then V.local_def (F.Local64 (F.Local64 (F.Local64 (F.No_locals)))) (index - 15) else ());
    (if index >= 16 then V.local_def (F.Local64 (F.Local64 (F.No_locals))) (index - 16) else ());
    (if index >= 17 then V.local_def (F.Local64 (F.No_locals)) (index - 17) else ()))
let (global @ total) : (registers : Registers.registers) @ immutable -> (index : B.u32) -> {u : unit | index < 8} ->
    {u : unit | V.global (Registers.globals registers) index === Some (if index < 6 then V.I32 else V.I64)
      && G.writable (Registers.globals registers).G.permissions index === (index = 1 || index = 3 || index >= 5)} @ ghost =
  fun registers index premise -> ghost_ (
    Registers.globals_def registers; Registers.values_def registers; Registers.permissions_def ();
    G.get_def (Registers.globals registers) index; V.global_def (Registers.globals registers) index;
    (if index >= 0 then L.get_def (S.Push (S.I32 registers.Registers.frame, S.Push (S.I32 registers.Registers.heap, S.Push (S.I32 registers.Registers.heap_limit, S.Push (S.I32 registers.Registers.top, S.Push (S.I32 registers.Registers.stack_limit, S.Push (S.I32 registers.Registers.status, S.Push (S.I64 registers.Registers.tag, S.Push (S.I64 registers.Registers.payload, S.Empty))))))))) (index - 0) else ());
    (if index >= 1 then L.get_def (S.Push (S.I32 registers.Registers.heap, S.Push (S.I32 registers.Registers.heap_limit, S.Push (S.I32 registers.Registers.top, S.Push (S.I32 registers.Registers.stack_limit, S.Push (S.I32 registers.Registers.status, S.Push (S.I64 registers.Registers.tag, S.Push (S.I64 registers.Registers.payload, S.Empty)))))))) (index - 1) else ());
    (if index >= 2 then L.get_def (S.Push (S.I32 registers.Registers.heap_limit, S.Push (S.I32 registers.Registers.top, S.Push (S.I32 registers.Registers.stack_limit, S.Push (S.I32 registers.Registers.status, S.Push (S.I64 registers.Registers.tag, S.Push (S.I64 registers.Registers.payload, S.Empty))))))) (index - 2) else ());
    (if index >= 3 then L.get_def (S.Push (S.I32 registers.Registers.top, S.Push (S.I32 registers.Registers.stack_limit, S.Push (S.I32 registers.Registers.status, S.Push (S.I64 registers.Registers.tag, S.Push (S.I64 registers.Registers.payload, S.Empty)))))) (index - 3) else ());
    (if index >= 4 then L.get_def (S.Push (S.I32 registers.Registers.stack_limit, S.Push (S.I32 registers.Registers.status, S.Push (S.I64 registers.Registers.tag, S.Push (S.I64 registers.Registers.payload, S.Empty))))) (index - 4) else ());
    (if index >= 5 then L.get_def (S.Push (S.I32 registers.Registers.status, S.Push (S.I64 registers.Registers.tag, S.Push (S.I64 registers.Registers.payload, S.Empty)))) (index - 5) else ());
    (if index >= 6 then L.get_def (S.Push (S.I64 registers.Registers.tag, S.Push (S.I64 registers.Registers.payload, S.Empty))) (index - 6) else ());
    (if index >= 7 then L.get_def (S.Push (S.I64 registers.Registers.payload, S.Empty)) (index - 7) else ());
    (if index >= 0 then G.writable_def (G.Global (false, G.Global (true, G.Global (false, G.Global (true, G.Global (false, G.Global (true, G.Global (true, G.Global (true, G.Empty))))))))) (index - 0) else ());
    (if index >= 1 then G.writable_def (G.Global (true, G.Global (false, G.Global (true, G.Global (false, G.Global (true, G.Global (true, G.Global (true, G.Empty)))))))) (index - 1) else ());
    (if index >= 2 then G.writable_def (G.Global (false, G.Global (true, G.Global (false, G.Global (true, G.Global (true, G.Global (true, G.Empty))))))) (index - 2) else ());
    (if index >= 3 then G.writable_def (G.Global (true, G.Global (false, G.Global (true, G.Global (true, G.Global (true, G.Empty)))))) (index - 3) else ());
    (if index >= 4 then G.writable_def (G.Global (false, G.Global (true, G.Global (true, G.Global (true, G.Empty))))) (index - 4) else ());
    (if index >= 5 then G.writable_def (G.Global (true, G.Global (true, G.Global (true, G.Empty)))) (index - 5) else ());
    (if index >= 6 then G.writable_def (G.Global (true, G.Global (true, G.Empty))) (index - 6) else ());
    (if index >= 7 then G.writable_def (G.Global (true, G.Empty)) (index - 7) else ()))
let (config @ total) : (module_ : F.module_) @ immutable -> (registers : Registers.registers) @ immutable -> (table_base : B.u32) -> (stack_base : B.u32) ->
    {u : unit | let context = {V.module_; globals = Registers.globals registers; locals = Runtime.local_types (); result = F.Void} in
      Hmc_wasm_static_emit.locals_typed context (Runtime.config table_base stack_base).Assembly.locals
      && Binding.bindings context (Runtime.config table_base stack_base).Assembly.loads
      && Binding.bindings context (Runtime.config table_base stack_base).Assembly.stores
      && Binding.writable context (Runtime.config table_base stack_base).Assembly.stores} @ ghost =
  fun module_ registers table_base stack_base -> ghost_ (
    let context = {V.module_; globals = Registers.globals registers; locals = Runtime.local_types (); result = F.Void} in
    Runtime.config_def table_base stack_base;
    local 0 ();
    local 1 ();
    local 2 ();
    local 3 ();
    local 4 ();
    local 5 ();
    local 6 ();
    local 7 ();
    local 8 ();
    local 9 ();
    local 10 ();
    local 11 ();
    local 12 ();
    local 13 ();
    local 14 ();
    local 15 ();
    local 16 ();
    local 17 ();
    global registers 0 ();
    global registers 1 ();
    global registers 2 ();
    global registers 3 ();
    global registers 4 ();
    global registers 5 ();
    global registers 6 ();
    global registers 7 ();
    Hmc_wasm_static_emit.locals_typed_def context (Runtime.config table_base stack_base).Assembly.locals;
    Hmc_wasm_static_structured.locals_typed_def context (Runtime.config table_base stack_base).Assembly.locals.Hmc_wasm_program_emit.structured;
    Hmc_wasm_static_cons.slots_typed_def context (Runtime.config table_base stack_base).Assembly.locals.Hmc_wasm_program_emit.structured.Hmc_wasm_structured_block.scratch;
    Hmc_wasm_static_call_data.descriptor_typed_def context (Runtime.config table_base stack_base).Assembly.locals.Hmc_wasm_program_emit.descriptor;
    Binding.bindings_def context R.End; Binding.writable_def context R.End;
    Binding.binding_def context 0 0;
    Binding.binding_def context 1 1;
    Binding.binding_def context 2 2;
    Binding.binding_def context 3 4;
    Binding.binding_def context 4 5;
    Binding.binding_def context 5 11;
    Binding.binding_def context 6 12;
    Binding.binding_def context 7 13;
    Binding.bindings_def context (R.Binding (7, 13, R.End));
    Binding.bindings_def context (R.Binding (6, 12, R.Binding (7, 13, R.End)));
    Binding.bindings_def context (R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End))));
    Binding.bindings_def context (R.Binding (4, 5, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End)))));
    Binding.bindings_def context (R.Binding (3, 4, R.Binding (4, 5, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End))))));
    Binding.bindings_def context (R.Binding (2, 2, R.Binding (3, 4, R.Binding (4, 5, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End)))))));
    Binding.bindings_def context (R.Binding (1, 1, R.Binding (2, 2, R.Binding (3, 4, R.Binding (4, 5, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End))))))));
    Binding.bindings_def context (R.Binding (0, 0, R.Binding (1, 1, R.Binding (2, 2, R.Binding (3, 4, R.Binding (4, 5, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End)))))))));
    Binding.bindings_def context (R.Binding (7, 13, R.End));
    Binding.writable_def context (R.Binding (7, 13, R.End));
    Binding.bindings_def context (R.Binding (6, 12, R.Binding (7, 13, R.End)));
    Binding.writable_def context (R.Binding (6, 12, R.Binding (7, 13, R.End)));
    Binding.bindings_def context (R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End))));
    Binding.writable_def context (R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End))));
    Binding.bindings_def context (R.Binding (3, 4, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End)))));
    Binding.writable_def context (R.Binding (3, 4, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End)))));
    Binding.bindings_def context (R.Binding (1, 1, R.Binding (3, 4, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End))))));
    Binding.writable_def context (R.Binding (1, 1, R.Binding (3, 4, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End)))))))
