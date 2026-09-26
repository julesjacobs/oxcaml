module B = Wasm_u32
module D = Hm_declarative
module W = Hmc_word64
module V = Hmc_tagged_cell
module M = Hmc_heap_objects
module R = Hmc_runtime_closures
module Cells = Hmc_memory_cells
module E = Hmc_heap_extent
module Bounds = Hmc_linear_bounds
module Preserve = Hmc_linear_preservation
module L = Hmc_linear_bytes
let[@def] (slots @ total) (u : unit) = D.S (D.S D.Z)
let[@def] (cells @ total) (descriptor : R.descriptor @ immutable) =
  M.Cell (V.Word {W.lo = descriptor.R.start; hi = descriptor.R.captures}, M.Cell (V.Boolean descriptor.R.recursive, M.Empty))
let[@def] (decode @ total) (cells : M.cells @ immutable) = match cells with
  | M.Cell (V.Word word, M.Cell (V.Boolean recursive, M.Empty)) -> Some {R.start = word.W.lo; captures = word.W.hi; recursive}
  | _ -> None
let[@def] (load @ total) (memory : B.bytes @ immutable) (address : W.limb) =
  match Cells.load memory address (slots ()) with None -> None | Some cells -> decode cells
let (store @ total) : (memory : B.bytes) @ immutable -> (limit : W.limb) -> (address : W.limb) -> (stop : W.limb) ->
    (descriptor : R.descriptor) @ immutable ->
    {u : unit | Bounds.covers memory limit && E.span (slots ()) address stop && stop <= limit} ->
    {out : B.bytes | load out address === Some descriptor && V.length out === V.length memory && Bounds.covers out limit
      && Preserve.equal_prefix address memory out && L.drop memory stop === L.drop out stop} @ immutable =
  fun memory limit address stop descriptor premise ->
    let payload = cells descriptor in
    ghost_ (cells_def descriptor; slots_def (); M.length_def payload;
      M.length_def (M.Cell (V.Boolean descriptor.R.recursive, M.Empty)); M.length_def M.Empty);
    let out = Cells.store memory limit address stop payload () in
    ghost_ (load_def out address; decode_def payload); out
