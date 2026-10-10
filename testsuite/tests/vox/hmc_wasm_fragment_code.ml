module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Lower = Hmc_wasm_simple_lower
module Index = Hmc_u32_index
let (jump @ total) : (next : D.index) @ immutable -> (pc : W.limb) -> (fragment : Lower.fragment) @ immutable ->
    (base_local : B.u32) -> {u : unit | Lower.corresponds (G.Jump next) fragment && Index.represents next pc} ->
    {u : unit | Lower.emit fragment base_local === Hmc_wasm_pc_update.emit pc base_local} @ ghost =
  fun next pc fragment base_local premise -> ghost_ (
    Lower.corresponds_def (G.Jump next) fragment; Lower.emit_def fragment base_local;
    match fragment with Lower.Jump number -> Index.unique next number pc () | _ -> ())
let (branch @ total) : (yes : D.index) @ immutable -> (no : D.index) @ immutable ->
    (yes_pc : W.limb) -> (no_pc : W.limb) -> (fragment : Lower.fragment) @ immutable -> (base_local : B.u32) ->
    {u : unit | Lower.corresponds (G.Branch (yes, no)) fragment
      && Index.represents yes yes_pc && Index.represents no no_pc} ->
    {u : unit | Lower.emit fragment base_local === Hmc_wasm_branch.emit yes_pc no_pc base_local} @ ghost =
  fun yes no yes_pc no_pc fragment base_local premise -> ghost_ (
    Lower.corresponds_def (G.Branch (yes, no)) fragment; Lower.emit_def fragment base_local;
    match fragment with Lower.Branch (a, b) -> Index.unique yes a yes_pc (); Index.unique no b no_pc () | _ -> ())
let (literal @ total) : (atom : G.atom) @ immutable -> (ty : D.mono) @ immutable -> (derivation : D.typing) @ immutable ->
    (next : D.index) @ immutable -> (value : V.value) @ immutable -> (pc : W.limb) ->
    (fragment : Lower.fragment) @ immutable -> (base_local : B.u32) ->
    {u : unit | Lower.corresponds (G.Load (atom, ty, derivation, next)) fragment
      && Hmc_wasm_literal_load.literal atom === Some value && Index.represents next pc} ->
    {u : unit | Lower.emit fragment base_local === Hmc_wasm_literal_load.emit pc value base_local} @ ghost =
  fun atom ty derivation next value pc fragment base_local premise -> ghost_ (
    Hmc_wasm_literal_load.literal_def atom;
    Lower.corresponds_def (G.Load (atom, ty, derivation, next)) fragment; Lower.emit_def fragment base_local;
    match fragment with Lower.Literal (_, number) -> Index.unique next number pc () | _ -> ())
let (local @ total) : (index : D.index) @ immutable -> (ty : D.mono) @ immutable -> (derivation : D.typing) @ immutable ->
    (next : D.index) @ immutable -> (number : Lower.slot) -> (pc : W.limb) ->
    (fragment : Lower.fragment) @ immutable -> (base_local : B.u32) ->
    {u : unit | Lower.corresponds (G.Load (G.Local index, ty, derivation, next)) fragment
      && Index.represents index number && Index.represents next pc} ->
    {u : unit | Lower.emit fragment base_local === Hmc_wasm_local_load.emit (Lower.slot_tag number) (Lower.slot_payload number) pc base_local} @ ghost =
  fun index ty derivation next number pc fragment base_local premise -> ghost_ (
    Hmc_wasm_literal_load.literal_def (G.Local index);
    Lower.corresponds_def (G.Load (G.Local index, ty, derivation, next)) fragment; Lower.emit_def fragment base_local;
    match fragment with Lower.Local (a, b) -> Index.unique index a number (); Index.unique next b pc () | _ -> ())
