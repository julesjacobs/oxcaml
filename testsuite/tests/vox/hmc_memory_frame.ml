module D = Hm_declarative
module W = Hmc_word64
module B = Wasm_u32
module V = Hmc_tagged_cell
module M = Hmc_heap_objects
module F = Hmc_heap_frame
module G = Hmc_cfg_ir
module Codec = Hmc_pointer_frame_codec
module Cells = Hmc_memory_cells
module Bounds = Hmc_linear_bounds
module Preserve = Hmc_linear_preservation
module E = Hmc_heap_extent
module L = Hmc_linear_bytes

let[@def] (load @ total) (memory : B.bytes @ immutable) (address : W.limb)
    (signature : G.signature @ immutable) (pc : D.index @ immutable) =
  match Cells.load memory address (Codec.size signature) with
  | None -> None
  | Some cells -> (match Codec.decode signature pc cells with Some (a, M.Empty) -> Some a | _ -> None)
let (store @ total) : (memory : B.bytes) @ immutable -> (limit : W.limb) -> (address : W.limb) -> (stop : W.limb) ->
    (signature : G.signature) @ immutable -> (a : F.activation) @ immutable ->
    {u : unit | Codec.shape signature a && Bounds.covers memory limit && E.span (Codec.size signature) address stop && stop <= limit} ->
    {out : B.bytes | load out address signature a.F.pc === Some a && V.length out === V.length memory
      && Bounds.covers out limit && Preserve.equal_prefix address memory out && L.drop memory stop === L.drop out stop} @ immutable =
  fun memory limit address stop signature a premise ->
    let cells = Codec.encode signature a M.Empty () in
    ghost_ (M.length_def M.Empty; Hm_abstraction_proofs.add_zero (Codec.size signature));
    let out = Cells.store memory limit address stop cells () in
    ghost_ (load_def out address signature a.F.pc); out
let (preserve @ total) : (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable -> (boundary : W.limb) ->
    (address : W.limb) -> (stop : W.limb) -> (signature : G.signature) @ immutable -> (pc : D.index) @ immutable ->
    {u : unit | Preserve.equal_prefix boundary before after && E.span (Codec.size signature) address stop && stop <= boundary} ->
    {u : unit | load before address signature pc === load after address signature pc} @ ghost =
  fun before after boundary address stop signature pc premise -> ghost_ (
    Cells.preserve before after boundary address stop (Codec.size signature) ();
    load_def before address signature pc; load_def after address signature pc)
