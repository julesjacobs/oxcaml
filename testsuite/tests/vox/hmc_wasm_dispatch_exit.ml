module B = Wasm_u32
module W = Hmc_word64
module S = Wasm_scalar
module T = Wasm_control
module X = Wasm_memory_execution
module Block = Hmc_wasm_structured_block
module Source = Hmc_wasm_structured_table
module Table = Hmc_wasm_dispatch_code
module Select = Hmc_wasm_structured_select
module Continue = Wasm_control_branch_continue
let rec (correct @ total) : (source : Source.table) @ immutable -> (target : Table.table) @ immutable ->
    (locals : Block.locals) @ immutable -> (depth : B.u32) -> (pc : W.limb) -> (fragment : Block.fragment) @ immutable ->
    (outer : T.labels) @ immutable -> (selection_state : X.state) @ immutable -> (state : X.state) @ immutable ->
    {u : unit | Table.corresponds source target locals depth && Source.lookup source pc === Some fragment} ->
    {u : unit | T.branch (Table.exit_depth source pc depth)
        (Continue.labels T.Empty (Select.selection target pc locals.Block.frame outer selection_state).T.labels) state
      === T.branch depth outer state} @ ghost =
  fun source target locals depth pc fragment outer selection_state state premise -> ghost_ (
    Table.corresponds_def source target locals depth; Source.lookup_def source pc;
    Table.exit_depth_def source pc depth;
    Select.selection_def target pc locals.Block.frame outer selection_state;
    match source, target with
    | Source.Add (label, _, rest), Table.Add (_, _, tail) ->
      S.add32_def depth 1; S.add32_def depth 2;
      if pc = label then (
        Continue.labels_def T.Empty (Select.label outer);
        T.branch_def (S.add32 depth 2) (Continue.labels T.Empty (Select.label outer)) state;
        Select.label_def outer; T.branch_def (depth + 1) (Select.label outer) state)
      else (
        correct rest tail locals (S.add32 depth 1) pc fragment (Select.label outer) selection_state state ();
        Select.label_def outer; T.branch_def (S.add32 depth 1) (Select.label outer) state)
    | _ -> ())
let (valid @ total) : (source : Source.table) @ immutable -> (target : Table.table) @ immutable ->
    (locals : Block.locals) @ immutable -> (depth : B.u32) -> (pc : W.limb) -> (fragment : Block.fragment) @ immutable ->
    (outer : T.labels) @ immutable -> (state : X.state) @ immutable ->
    {u : unit | Table.corresponds source target locals depth && Source.lookup source pc === Some fragment
      && Wasm_control_branch_target.valid depth outer} ->
    {u : unit | Wasm_control_branch_target.valid (Table.exit_depth source pc depth)
      (Continue.labels T.Empty (Select.selection target pc locals.Block.frame outer state).T.labels)} @ ghost =
  fun source target locals depth pc fragment outer state premise -> ghost_ (
    let exit = Wasm_control_branch_target.target depth outer state () in
    correct source target locals depth pc fragment outer state state ();
    Wasm_control_branch_target.valid_of_running (Table.exit_depth source pc depth)
      (Continue.labels T.Empty (Select.selection target pc locals.Block.frame outer state).T.labels) state exit ())
