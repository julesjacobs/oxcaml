module D = Hm_declarative
module W = Hmc_word64
module B = Wasm_u32
module C = Hmc_tagged_cell
module Wire = Hmc_heap_wire
module L = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
module Extent = Hmc_memory_extent
module Preserve = Hmc_linear_preservation
module E = Hmc_heap_extent

let[@def] (slots @ total) (schema : Wire.schema @ immutable) = match schema with
  | Wire.Cons_schema -> D.S (D.S D.Z) | Wire.Closure_schema captures -> D.S captures
let[@def] (load @ total) (memory : B.bytes @ immutable) (address : W.limb) (schema : Wire.schema @ immutable) =
  match L.load memory address (Wire.bytes_size (slots schema) D.Z) with
  | None -> None
  | Some bytes -> (match Wire.decode schema bytes with Some (object_, B.End) -> Some object_ | _ -> None)
let (store @ total) : (memory : B.bytes) @ immutable -> (limit : W.limb) -> (address : W.limb) -> (stop : W.limb) ->
    (object_ : Wire.object_) @ immutable ->
    {u : unit | Bounds.covers memory limit && E.span (Wire.slots object_) address stop && stop <= limit} ->
    {out : B.bytes | load out address (Wire.schema object_) === Some object_
      && C.length out === C.length memory && Bounds.covers out limit
      && Preserve.equal_prefix address memory out && L.drop memory stop === L.drop out stop} @ immutable = fun memory limit address stop object_ premise ->
  let payload = Wire.encode object_ B.End in
  ghost_ (C.length_def B.End; Wire.schema_def object_; Wire.slots_def object_; slots_def (Wire.schema object_);
    Extent.cells (Wire.slots object_) address stop ();
    Bounds.fits memory limit address stop payload ());
  match L.store memory address payload with
  | None -> unreachable_ ()
  | Some out ->
    ghost_ (load_def out address (Wire.schema object_);
      Bounds.same_length memory out limit ();
      Preserve.before_store memory address payload out ();
      Preserve.after_store memory out address stop payload ()); out
let (preserve @ total) : (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable -> (boundary : W.limb) ->
    (address : W.limb) -> (stop : W.limb) -> (schema : Wire.schema) @ immutable ->
    {u : unit | Preserve.equal_prefix boundary before after && E.span (slots schema) address stop && stop <= boundary} ->
    {u : unit | load before address schema === load after address schema} @ ghost =
  fun before after boundary address stop schema premise -> ghost_ (
    Extent.cells (slots schema) address stop ();
    Preserve.load before after boundary address stop (Wire.bytes_size (slots schema) D.Z) ();
    load_def before address schema; load_def after address schema)
