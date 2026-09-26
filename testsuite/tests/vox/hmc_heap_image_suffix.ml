module B = Wasm_u32
module W = Hmc_word64
module L = Hmc_linear_bytes
module Heap = Hmc_heap_objects
module Image = Hmc_heap_image
module Memory = Hmc_memory_object
let[@def] rec (above @ total) (heap : Heap.heap @ immutable) (boundary : W.limb) = ghost_ (
  match heap with
  | Heap.Empty_heap _ -> true
  | Heap.Allocate (allocation, rest) -> boundary <= allocation.Heap.address && above rest boundary)
let rec (seek @ total) : (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (boundary : W.limb) -> (address : W.limb) ->
    {u : unit | boundary <= address && not (L.drop before boundary === None) && L.drop before boundary === L.drop after boundary} ->
    {u : unit | L.drop before address === L.drop after address} @ ghost =
  fun before after boundary address premise -> ghost_ (
    L.drop_def before boundary; L.drop_def after boundary;
    if boundary = 0 then () else (
      L.drop_def before address; L.drop_def after address;
      match before, after with
      | B.Byte (_, left), B.Byte (_, right) -> seek left right (boundary - 1) (address - 1) ()
      | _ -> ()))
let rec (preserve @ total) : (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (heap : Heap.heap) @ immutable -> (boundary : W.limb) ->
    {u : unit | above heap boundary && Image.related before heap && not (L.drop before boundary === None) && L.drop before boundary === L.drop after boundary} ->
    {u : unit | Image.related after heap} @ ghost = fun before after heap boundary premise -> ghost_ (
    above_def heap boundary; Image.related_def before heap; Image.related_def after heap;
    match heap with
    | Heap.Empty_heap _ -> ()
    | Heap.Allocate (allocation, rest) ->
      seek before after boundary allocation.Heap.address ();
      Image.stored_def before allocation.Heap.address allocation.Heap.object_;
      Image.stored_def after allocation.Heap.address allocation.Heap.object_;
      Memory.load_def before allocation.Heap.address (Image.schema allocation.Heap.object_);
      Memory.load_def after allocation.Heap.address (Image.schema allocation.Heap.object_);
      L.load_def before allocation.Heap.address (Hmc_heap_wire.bytes_size (Memory.slots (Image.schema allocation.Heap.object_)) Hm_declarative.Z);
      L.load_def after allocation.Heap.address (Hmc_heap_wire.bytes_size (Memory.slots (Image.schema allocation.Heap.object_)) Hm_declarative.Z);
      preserve before after rest boundary ())
