module B = Wasm_u32
module W = Hmc_word64
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module M = Wasm_memory
module T = Wasm_control
module Fuel = Wasm_control_compose
module Table = Hmc_wasm_table_lower
module Block = Hmc_wasm_block_lower
module Loop = Hmc_wasm_dispatch_loop
module Header = Hmc_wasm_header_update
type trace = Stop | More of W.limb * Block.fragment * X.state * trace [@@inductive]
let[@def] rec (valid @ total) (trace : trace @ immutable) (table : Table.table @ immutable)
    (base_local : B.u32) (base : B.u32) (before : X.state @ immutable) = ghost_ (match trace with
  | Stop -> true
  | More (pc, fragment, after, rest) -> Table.lookup table pc === Some fragment
    && before.X.machine.E.stack === S.Empty && after.X.machine.E.stack === S.Empty
    && Wasm_locals.get before.X.machine.E.locals base_local === Some (S.I32 base)
    && M.load before.X.memory base (Hmc_wasm_pc_update.offset ()) M.W64 === Some (S.I64 (Header.number pc))
    && X.run (Block.emit fragment base_local) before === X.Done after
    && valid rest table base_local base after)
let[@def] rec (cost @ total) (trace : trace @ immutable) (table : Table.table @ immutable) (base_local : B.u32) = match trace with
  | Stop -> C.Zero | More (pc, fragment, _, rest) -> Fuel.add (Loop.cost table pc base_local fragment) (cost rest table base_local)
let[@def] rec (last @ total) (trace : trace @ immutable) (before : X.state @ immutable) = match trace with
  | Stop -> before | More (_, _, after, rest) -> last rest after
let rec (correct @ total) : (trace : trace) @ immutable -> (table : Table.table) @ immutable ->
    (base_local : B.u32) -> (base : B.u32) -> (outer : T.labels) @ immutable -> (before : X.state) @ immutable ->
    {u : unit | valid trace table base_local base before} ->
    {u : unit | T.run (cost trace table base_local) (Loop.configuration table base_local outer before)
      === T.Running (Loop.configuration table base_local outer (last trace before))} @ ghost =
  fun trace table base_local base outer before premise -> ghost_ (
    valid_def trace table base_local base before; cost_def trace table base_local; last_def trace before;
    match trace with
    | Stop -> T.run_def C.Zero (Loop.configuration table base_local outer before)
    | More (pc, fragment, after, rest) ->
      Loop.correct table pc base_local base fragment outer before after ();
      correct rest table base_local base outer after ();
      Fuel.correct (Loop.cost table pc base_local fragment) (cost rest table base_local) (Loop.configuration table base_local outer before))
