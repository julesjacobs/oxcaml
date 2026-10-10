module D = Hm_declarative
module W = Hmc_word64
module B = Wasm_u32
module C = Hmc_tagged_cell
module M = Hmc_heap_objects
module K = Hmc_closure_ir
module E = Hmc_heap_extent
module Wire = Hmc_heap_wire
module Memory = Hmc_memory_object
module Bounds = Hmc_linear_bounds
module Preserve = Hmc_linear_preservation
module Index = Hmc_u32_index

module Image = Hmc_heap_image
module Above = Hmc_heap_image_suffix
let (store @ total) : (table : K.table) @ immutable -> (capacity : W.limb) -> (memory : B.bytes) @ immutable ->
    (heap : M.heap) @ immutable -> (limit : W.limb) -> (allocation : M.allocation) @ immutable ->
    {u : unit | M.valid table heap && Image.related memory heap && Bounds.covers memory limit
      && allocation.M.address = M.used heap && E.span (M.slots allocation.M.object_) allocation.M.address allocation.M.stop
      && allocation.M.stop <= limit && Image.object_encodable capacity allocation.M.object_} ->
    {out : B.bytes | Image.related out (M.Allocate (allocation, heap)) && Bounds.covers out limit && C.length out === C.length memory && Preserve.equal_prefix allocation.M.address memory out} @ immutable =
  fun table capacity memory heap limit allocation premise ->
    ghost_ (Image.object_encodable_def capacity allocation.M.object_);
    match Wire.lower capacity allocation.M.object_ with
    | None -> unreachable_ ()
    | Some wire ->
      let out = Memory.store memory limit allocation.M.address allocation.M.stop wire () in
      ghost_ (Image.preserve table memory out heap allocation.M.address ();
        Image.schema_agreement allocation.M.object_ wire ();
        Image.stored_def out allocation.M.address allocation.M.object_; Image.related_def out (M.Allocate (allocation, heap))); out
let rec (reflexive @ total) : (memory : B.bytes) @ immutable -> (boundary : W.limb) ->
    {u : unit | Bounds.covers memory boundary} ->
    {u : unit | Preserve.equal_prefix boundary memory memory} @ ghost = fun memory boundary premise -> ghost_ (
    Bounds.covers_def memory boundary; Hmc_linear_bytes.drop_def memory boundary; Preserve.equal_prefix_def boundary memory memory;
    if boundary = 0 then () else match memory with
    | B.End -> ()
    | B.Byte (_, rest) -> Bounds.covers_def rest (boundary - 1); reflexive rest (boundary - 1) ())
let rec (transitive @ total) : (first : B.bytes) @ immutable -> (middle : B.bytes) @ immutable -> (last : B.bytes) @ immutable ->
    (boundary : W.limb) -> {u : unit | Preserve.equal_prefix boundary first middle && Preserve.equal_prefix boundary middle last} ->
    {u : unit | Preserve.equal_prefix boundary first last} @ ghost = fun first middle last boundary premise -> ghost_ (
    Preserve.equal_prefix_def boundary first middle; Preserve.equal_prefix_def boundary middle last; Preserve.equal_prefix_def boundary first last;
    if boundary = 0 then () else match first, middle, last with
    | B.Byte (_, a), B.Byte (_, b), B.Byte (_, c) -> transitive a b c (boundary - 1) ()
    | _ -> ())
let rec (materialize @ total) : (table : K.table) @ immutable -> (capacity : W.limb) -> (memory : B.bytes) @ immutable ->
    (heap : M.heap) @ immutable -> (limit : W.limb) -> (boundary : W.limb) ->
    {u : unit | M.valid table heap && M.used heap <= limit && Image.encodable capacity heap && Bounds.covers memory limit
      && Above.above heap boundary && boundary <= limit && Bounds.covers memory boundary} ->
    {out : B.bytes | Image.related out heap && Bounds.covers out limit && C.length out === C.length memory
      && Preserve.equal_prefix boundary memory out} @ immutable =
  fun table capacity memory heap limit boundary premise ->
    ghost_ (M.valid_def table heap; M.used_def heap; Image.encodable_def capacity heap; Above.above_def heap boundary);
    match heap with
    | M.Empty_heap _ -> ghost_ (Image.related_def memory heap; reflexive memory boundary ()); memory
    | M.Allocate (a, rest) ->
      ghost_ (E.ordered (M.slots a.M.object_) a.M.address a.M.stop ());
      let previous = materialize table capacity memory rest limit boundary () in
      let out = store table capacity previous rest limit a () in
      ghost_ (Preserve.shrink a.M.address boundary previous out (); transitive memory previous out boundary ()); out
