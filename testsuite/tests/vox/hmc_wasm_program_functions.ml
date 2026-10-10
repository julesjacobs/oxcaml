module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module Source = Hmc_tail_ir
module Index = Hmc_u32_index
module Table = Hmc_wasm_program_table
module Block = Hmc_wasm_program_block
module Lower = Hmc_wasm_program_lower
module Emit = Hmc_wasm_program_emit
module Registers = Wasm_global_registers
module Wrapper = Wasm_register_block
module T = Wasm_control
module F = Wasm_functions
let[@def] (reverse_index @ total) (count : B.u32) (index : B.u32) : B.u32 =
  if index < count then count - 1 - index else 0
let[@def] rec (ordered @ total) (table : Table.table @ immutable) (count : B.u32) = ghost_ (
  match table with
  | Table.Empty -> count = 0
  | Table.Add (label, _, rest) -> count > 0 && label = count - 1 && ordered rest label)
let rec (source_order @ total) : (globals : Hmc_heap_machine.globals) @ immutable -> (blocks : G.table) @ immutable ->
    (rewritten : Source.table) @ immutable -> (target : Table.table) @ immutable -> (capacity : Hmc_wasm_relayout.count) ->
    (max_pc : B.u32) -> (count : B.u32) ->
    {u : unit | Table.corresponds globals blocks rewritten target capacity max_pc && Index.represents (G.size blocks) count} ->
    {u : unit | ordered target count} @ ghost = fun globals blocks rewritten target capacity max_pc count premise -> ghost_ (
  Table.corresponds_def globals blocks rewritten target capacity max_pc; G.size_def blocks; ordered_def target count;
  Index.represents_def (G.size blocks) count;
  match blocks, rewritten, target with
  | G.Add (_, rest), Source.Add (_, code), Table.Add (label, _, tail) ->
    Index.unique (G.size rest) label (count - 1) ();
    source_order globals rest code tail capacity max_pc label ()
  | _ -> ())
type config = {locals : Emit.locals; local_types : F.local_types; loads : Registers.plan; stores : Registers.plan;
  table_base : B.u32; stack_base : B.u32}
let[@def] (function_ @ total) (program : Lower.program @ immutable) (fragment : Block.fragment @ immutable) (config : config @ immutable) =
  {F.result = F.Void; locals = config.local_types;
    code = Wrapper.emit config.loads (Emit.emit program fragment config.locals config.table_base config.stack_base) config.stores T.Empty}
let[@def] rec (functions @ total) (program : Lower.program @ immutable) (blocks : Table.table @ immutable)
    (config : config @ immutable) (tail : F.functions @ immutable) = match blocks with
  | Table.Empty -> tail
  | Table.Add (_, fragment, rest) -> F.Function (function_ program fragment config, functions program rest config tail)
let rec (lookup_correct @ total) : (program : Lower.program) @ immutable -> (blocks : Table.table) @ immutable ->
    (config : config) @ immutable -> (tail : F.functions) @ immutable -> (count : B.u32) -> (label : B.u32) ->
    {u : unit | ordered blocks count && label < count} ->
    {u : unit | F.lookup (functions program blocks config tail) (reverse_index count label) ===
      (match Table.lookup blocks label with None -> None | Some fragment -> Some (function_ program fragment config))} @ ghost =
  fun program blocks config tail count label premise -> ghost_ (
    ordered_def blocks count; reverse_index_def count label;
    functions_def program blocks config tail; Table.lookup_def blocks label;
    F.lookup_def (functions program blocks config tail) (reverse_index count label);
    match blocks with
    | Table.Empty -> ()
    | Table.Add (head, _, rest) -> if label = head then () else (
      reverse_index_def head label; lookup_correct program rest config tail head label ()) )
let[@def] rec (elements @ total) (length : D.index @ immutable) (count : B.u32) = match length with
  | D.Z -> F.No_elements
  | D.S rest -> if count = 0 then F.No_elements else F.Element (Some (count - 1), elements rest (count - 1))
let rec (element_correct @ total) : (length : D.index) @ immutable -> (count : B.u32) -> (label : B.u32) ->
    {u : unit | Index.represents length count && label < count} ->
    {u : unit | F.element (elements length count) label === Some (reverse_index count label)} @ ghost =
  fun length count label premise -> ghost_ (
    Index.represents_def length count; elements_def length count; reverse_index_def count label;
    F.element_def (elements length count) label;
    match length with
    | D.Z -> ()
    | D.S rest -> if label = 0 then () else (
      reverse_index_def (count - 1) (label - 1); element_correct rest (count - 1) (label - 1) ()))
let[@def] (assemble @ total) (program : Lower.program @ immutable) (length : D.index @ immutable) (count : B.u32)
    (config : config @ immutable) (dispatcher : F.function_ @ immutable) =
  {F.functions = functions program program.Lower.blocks config (F.Function (dispatcher, F.No_functions));
    signatures = F.Signature (F.Void, F.Signature (dispatcher.F.result, F.No_signatures)); table = elements length count}
let (dispatch_target @ total) : (program : Lower.program) @ immutable -> (length : D.index) @ immutable -> (count : B.u32) ->
    (config : config) @ immutable -> (dispatcher : F.function_) @ immutable -> (label : B.u32) -> (fragment : Block.fragment) @ immutable ->
    {u : unit | ordered program.Lower.blocks count && Index.represents length count && label < count
      && Table.lookup program.Lower.blocks label === Some fragment} ->
    {u : unit | F.element (assemble program length count config dispatcher).F.table label === Some (reverse_index count label)
      && F.lookup (assemble program length count config dispatcher).F.functions (reverse_index count label) === Some (function_ program fragment config)} @ ghost =
  fun program length count config dispatcher label fragment premise -> ghost_ (
    assemble_def program length count config dispatcher;
    element_correct length count label ();
    lookup_correct program program.Lower.blocks config (F.Function (dispatcher, F.No_functions)) count label ())
let[@def] (zero @ total) (unit : unit) : B.u32 = 0
let rec (tail_lookup @ total) : (program : Lower.program) @ immutable -> (blocks : Table.table) @ immutable ->
    (config : config) @ immutable -> (tail : F.functions) @ immutable -> (count : B.u32) ->
    {u : unit | ordered blocks count} ->
    {u : unit | F.lookup (functions program blocks config tail) count === F.lookup tail (zero ())} @ ghost =
  fun program blocks config tail count premise -> ghost_ (
    ordered_def blocks count; functions_def program blocks config tail; zero_def ();
    F.lookup_def (functions program blocks config tail) count;
    match blocks with
    | Table.Empty -> ()
    | Table.Add (label, _, rest) ->
      tail_lookup program rest config tail label ())
let (main_correct @ total) : (program : Lower.program) @ immutable -> (length : D.index) @ immutable -> (count : B.u32) ->
    (config : config) @ immutable -> (dispatcher : F.function_) @ immutable ->
    {u : unit | ordered program.Lower.blocks count} ->
    {u : unit | F.lookup (assemble program length count config dispatcher).F.functions count === Some dispatcher} @ ghost =
  fun program length count config dispatcher premise -> ghost_ (
    assemble_def program length count config dispatcher;
    tail_lookup program program.Lower.blocks config (F.Function (dispatcher, F.No_functions)) count ();
    zero_def ();
    F.lookup_def (F.Function (dispatcher, F.No_functions)) (zero ()))
let rec (element_reject @ total) : (length : D.index) @ immutable -> (count : B.u32) -> (label : B.u32) ->
    {u : unit | Index.represents length count && label >= count} ->
    {u : unit | F.element (elements length count) label === None} @ ghost = fun length count label premise -> ghost_ (
  Index.represents_def length count; elements_def length count; F.element_def (elements length count) label;
  match length with D.Z -> () | D.S rest -> element_reject rest (count - 1) (label - 1) ())
let (assemble_checked @ total) : (program : Lower.program) @ immutable -> (length : D.index) @ immutable -> (count : B.u32) ->
    (config : config) @ immutable -> (dispatcher : F.function_) @ immutable ->
    {out : Wasm_shallow_module.program option | match out with None -> true | Some checked ->
      checked.Wasm_shallow_module.module_ === assemble program length count config dispatcher
      && checked.Wasm_shallow_module.entry = count} @ immutable =
  fun program length count config dispatcher ->
    Wasm_shallow_module.check (assemble program length count config dispatcher) count
