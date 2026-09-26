module D = Hm_declarative
module W = Hmc_word64
module B = Wasm_u32
module C = Hmc_tagged_cell
module M = Hmc_heap_objects
module Wire = Hmc_heap_wire
module L = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
module Preserve = Hmc_linear_preservation
module E = Hmc_heap_extent

let[@def] (load @ total) (memory : B.bytes @ immutable) (address : W.limb) (count : D.index @ immutable) =
  match L.load memory address (Wire.bytes_size count D.Z) with None -> None | Some bytes ->
    match Wire.decode_cells count bytes with Some (cells, B.End) -> Some cells | _ -> None
let (store @ total) : (memory : B.bytes) @ immutable -> (limit : W.limb) -> (address : W.limb) -> (stop : W.limb) ->
    (cells : M.cells) @ immutable ->
    {u : unit | Bounds.covers memory limit && E.span (M.length cells) address stop && stop <= limit} ->
    {out : B.bytes | load out address (M.length cells) === Some cells && C.length out === C.length memory
      && Bounds.covers out limit && Preserve.equal_prefix address memory out && L.drop memory stop === L.drop out stop} @ immutable =
  fun memory limit address stop cells premise ->
    let payload = Wire.encode_cells cells B.End in
    ghost_ (C.length_def B.End; Hmc_memory_extent.cells (M.length cells) address stop ();
      Bounds.fits memory limit address stop payload ());
    match L.store memory address payload with
    | None -> unreachable_ ()
    | Some out ->
      ghost_ (load_def out address (M.length cells); Bounds.same_length memory out limit ();
        Preserve.before_store memory address payload out (); Preserve.after_store memory out address stop payload ()); out
let (preserve @ total) : (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable -> (boundary : W.limb) ->
    (address : W.limb) -> (stop : W.limb) -> (count : D.index) @ immutable ->
    {u : unit | Preserve.equal_prefix boundary before after && E.span count address stop && stop <= boundary} ->
    {u : unit | load before address count === load after address count} @ ghost = fun before after boundary address stop count premise -> ghost_ (
  Hmc_memory_extent.cells count address stop ();
  Preserve.load before after boundary address stop (Wire.bytes_size count D.Z) ();
  load_def before address count; load_def after address count)
