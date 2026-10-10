module B = Wasm_u32
module H = Hmc_heap_objects
module Image = Hmc_heap_image
module Memory = Hmc_memory_object
module Bounds = Hmc_linear_bounds
module Bytes = Hmc_linear_bytes
module Wire = Hmc_heap_wire
module D = Hm_declarative
let[@def] rec (above @ total) (heap : H.heap @ immutable) (boundary : B.u32) = ghost_ (
  match heap with
  | H.Empty_heap base -> boundary <= base
  | H.Allocate (allocation, rest) -> boundary <= allocation.H.address && above rest boundary)
let rec (preserve @ total) : (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (heap : H.heap) @ immutable -> (boundary : B.u32) ->
    {u : unit | above heap boundary && Image.related before heap
      && Bounds.covers before boundary && Bounds.covers after boundary
      && Bytes.drop before boundary === Bytes.drop after boundary} ->
    {u : unit | Image.related after heap} @ ghost = fun before after heap boundary premise -> ghost_ (
    above_def heap boundary; Image.related_def before heap; Image.related_def after heap;
    match heap with
    | H.Empty_heap _ -> ()
    | H.Allocate (allocation, rest) ->
      let schema = Image.schema allocation.H.object_ in
      Hmc_memory_suffix.load before after boundary allocation.H.address (Wire.bytes_size (Memory.slots schema) D.Z) ();
      Memory.load_def before allocation.H.address schema; Memory.load_def after allocation.H.address schema;
      Image.stored_def before allocation.H.address allocation.H.object_;
      Image.stored_def after allocation.H.address allocation.H.object_;
      preserve before after rest boundary ())
let rec (origin @ total) : (table : Hmc_closure_ir.table) @ immutable -> (heap : H.heap) @ immutable ->
    (base : B.u32) -> (boundary : B.u32) ->
    {u : unit | H.valid table heap && Hmc_heap_preservation.extends heap (H.Empty_heap base) && boundary <= base} ->
    {u : unit | above heap boundary && base <= H.used heap} @ ghost = fun table heap base boundary premise -> ghost_ (
    H.valid_def table heap; Hmc_heap_preservation.extends_def heap (H.Empty_heap base);
    above_def heap boundary; H.used_def heap;
    match heap with
    | H.Empty_heap _ -> ()
    | H.Allocate (allocation, rest) ->
      origin table rest base boundary ();
      Hmc_heap_extent.ordered (H.slots allocation.H.object_) allocation.H.address allocation.H.stop ())

let rec (objects_above @ total) : (heap : H.heap) @ immutable -> (boundary : B.u32) ->
    {u : unit | above heap boundary} -> {u : unit | Hmc_heap_image_suffix.above heap boundary} @ ghost =
  fun heap boundary premise -> ghost_ (
    above_def heap boundary; Hmc_heap_image_suffix.above_def heap boundary;
    match heap with H.Empty_heap _ -> () | H.Allocate (_, rest) -> objects_above rest boundary ())
