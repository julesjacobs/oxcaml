module B = Wasm_u32
module F = Wasm_functions
module V = Wasm_static_types
module Check = Wasm_static_control
module T = Wasm_control
module Registers = Hmc_wasm_program_registers
module Runtime = Hmc_wasm_program_runtime
module Assembly = Hmc_wasm_program_functions
module Lower = Hmc_wasm_program_lower
module Table = Hmc_wasm_program_table
module Block = Hmc_wasm_program_block
module Emit = Hmc_wasm_static_emit
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
let (function_ @ total) : (module_ : F.module_) @ immutable -> (registers : Registers.registers) @ immutable ->
    (program : Lower.program) @ immutable -> (fragment : Block.fragment) @ immutable -> (table_base : B.u32) -> (stack_base : B.u32) ->
    {u : unit | Emit.padding_typed {V.module_; globals = Registers.globals registers; locals = Runtime.local_types (); result = F.Void} fragment} ->
    {u : unit | Check.function_ module_ (Registers.globals registers) (Assembly.function_ program fragment (Runtime.config table_base stack_base))} @ ghost =
  fun module_ registers program fragment table_base stack_base premise -> ghost_ (
    let config = Runtime.config table_base stack_base in
    let context = {V.module_; globals = Registers.globals registers; locals = Runtime.local_types (); result = F.Void} in
    let empty = V.initial () in
    let body = Hmc_wasm_program_emit.emit program fragment config.Assembly.locals table_base stack_base in
    Hmc_wasm_static_config.config module_ registers table_base stack_base;
    Runtime.config_def table_base stack_base;
    Emit.emit context (Check.Label (Check.Root F.Void)) program fragment config.Assembly.locals table_base stack_base empty ();
    V.initial_def (); V.consume_result_def F.Void empty; V.finish_def F.Void empty;
    Wasm_static_registers.wrapper context (Check.Root F.Void) config.Assembly.loads body config.Assembly.stores T.Empty empty empty ();
    Check.check_def context (Check.Root F.Void) T.Empty empty;
    Assembly.function__def program fragment config;
    Check.function__def module_ (Registers.globals registers) (Assembly.function_ program fragment config))
let rec (functions @ total) : (module_ : F.module_) @ immutable -> (registers : Registers.registers) @ immutable ->
    (program : Lower.program) @ immutable -> (globals : Hmc_heap_machine.globals) @ immutable ->
    (source : G.table) @ immutable -> (rewritten : I.table) @ immutable -> (blocks : Table.table) @ immutable ->
    (capacity : Hmc_wasm_relayout.count) -> (max_pc : B.u32) -> (table_base : B.u32) -> (stack_base : B.u32) -> (tail : F.functions) @ immutable ->
    {u : unit | Table.corresponds globals source rewritten blocks capacity max_pc && Check.functions module_ (Registers.globals registers) tail} ->
    {u : unit | Check.functions module_ (Registers.globals registers) (Assembly.functions program blocks (Runtime.config table_base stack_base) tail)} @ ghost =
  fun module_ registers program globals source rewritten blocks capacity max_pc table_base stack_base tail premise -> ghost_ (
    Table.corresponds_def globals source rewritten blocks capacity max_pc;
    Assembly.functions_def program blocks (Runtime.config table_base stack_base) tail;
    match source, rewritten, blocks with
    | G.Add (block, rest), I.Add (instruction, code), Table.Add (_, fragment, remaining) ->
      let context = {V.module_; globals = Registers.globals registers; locals = Runtime.local_types (); result = F.Void} in
      Emit.padding_from_correspondence context globals block.G.signature instruction capacity max_pc fragment ();
      function_ module_ registers program fragment table_base stack_base ();
      functions module_ registers program globals rest code remaining capacity max_pc table_base stack_base tail ();
      Check.functions_def module_ (Registers.globals registers) (Assembly.functions program blocks (Runtime.config table_base stack_base) tail)
    | _ -> ())
