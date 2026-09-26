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

let[@def] (schema @ total) (object_ : M.object_ @ immutable) = match object_ with
  | M.Cons _ -> Wire.Cons_schema | M.Closure (_, captures) -> Wire.Closure_schema (M.length captures)
let (schema_slots @ total) : (object_ : M.object_) @ immutable ->
    {u : unit | Memory.slots (schema object_) === M.slots object_} @ ghost = fun object_ -> ghost_ (
  schema_def object_; Memory.slots_def (schema object_); M.slots_def object_)
let (schema_agreement @ total) : (source : M.object_) @ immutable -> (wire : Wire.object_) @ immutable ->
    {u : unit | Wire.corresponds source wire} -> {u : unit | schema source === Wire.schema wire} @ ghost = fun source wire premise -> ghost_ (
  Wire.corresponds_def source wire; schema_def source; Wire.schema_def wire)
let[@def] (stored @ total) (memory : B.bytes @ immutable) (address : W.limb) (object_ : M.object_ @ immutable) = ghost_ (
  match Memory.load memory address (schema object_) with None -> false | Some wire -> Wire.corresponds object_ wire)
let[@def] rec (related @ total) (memory : B.bytes @ immutable) (heap : M.heap @ immutable) = ghost_ (match heap with
  | M.Empty_heap _ -> true | M.Allocate (a, rest) -> stored memory a.M.address a.M.object_ && related memory rest)
let[@def] (object_encodable @ total) (capacity : W.limb) (object_ : M.object_ @ immutable) = match object_ with
  | M.Cons _ -> true | M.Closure (code, _) -> Index.fits code capacity
let[@def] rec (encodable @ total) (capacity : W.limb) (heap : M.heap @ immutable) = match heap with
  | M.Empty_heap _ -> true | M.Allocate (a, rest) -> object_encodable capacity a.M.object_ && encodable capacity rest
let rec (preserve @ total) : (table : K.table) @ immutable -> (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (heap : M.heap) @ immutable -> (boundary : W.limb) ->
    {u : unit | M.valid table heap && related before heap && M.used heap <= boundary && Preserve.equal_prefix boundary before after} ->
    {u : unit | related after heap} @ ghost = fun table before after heap boundary premise -> ghost_ (
  M.valid_def table heap; M.used_def heap; related_def before heap; related_def after heap;
  match heap with M.Empty_heap _ -> () | M.Allocate (a, rest) ->
    E.ordered (M.slots a.M.object_) a.M.address a.M.stop ();
    schema_slots a.M.object_; Memory.preserve before after boundary a.M.address a.M.stop (schema a.M.object_) ();
    stored_def before a.M.address a.M.object_; stored_def after a.M.address a.M.object_;
    preserve table before after rest boundary ())
let (store @ total) : (table : K.table) @ immutable -> (capacity : W.limb) -> (memory : B.bytes) @ immutable ->
    (heap : M.heap) @ immutable -> (limit : W.limb) -> (allocation : M.allocation) @ immutable ->
    {u : unit | M.valid table heap && related memory heap && Bounds.covers memory limit
      && allocation.M.address = M.used heap && E.span (M.slots allocation.M.object_) allocation.M.address allocation.M.stop
      && allocation.M.stop <= limit && object_encodable capacity allocation.M.object_} ->
    {out : B.bytes | related out (M.Allocate (allocation, heap)) && Bounds.covers out limit && C.length out === C.length memory} @ immutable =
  fun table capacity memory heap limit allocation premise ->
    ghost_ (object_encodable_def capacity allocation.M.object_);
    match Wire.lower capacity allocation.M.object_ with
    | None -> unreachable_ ()
    | Some wire ->
      let out = Memory.store memory limit allocation.M.address allocation.M.stop wire () in
      ghost_ (preserve table memory out heap allocation.M.address ();
        schema_agreement allocation.M.object_ wire ();
        stored_def out allocation.M.address allocation.M.object_; related_def out (M.Allocate (allocation, heap))); out
let rec (materialize @ total) : (table : K.table) @ immutable -> (capacity : W.limb) -> (memory : B.bytes) @ immutable ->
    (heap : M.heap) @ immutable -> (limit : W.limb) ->
    {u : unit | M.valid table heap && M.used heap <= limit && encodable capacity heap && Bounds.covers memory limit} ->
    {out : B.bytes | related out heap && Bounds.covers out limit && C.length out === C.length memory} @ immutable =
  fun table capacity memory heap limit premise ->
    ghost_ (M.valid_def table heap; M.used_def heap; encodable_def capacity heap);
    match heap with
    | M.Empty_heap _ -> ghost_ (related_def memory heap); memory
    | M.Allocate (a, rest) ->
      ghost_ (E.ordered (M.slots a.M.object_) a.M.address a.M.stop ());
      let previous = materialize table capacity memory rest limit () in
      store table capacity previous rest limit a ()
let rec (fetch @ total) : (memory : B.bytes) @ immutable -> (heap : M.heap) @ immutable ->
    (address : W.limb) -> (object_ : M.object_) @ immutable ->
    {u : unit | related memory heap && Hmc_heap_preservation.lookup_object heap address === Some object_} ->
    {wire : Wire.object_ | Memory.load memory address (schema object_) === Some wire && Wire.corresponds object_ wire} @ immutable =
  fun memory heap address object_ premise ->
    ghost_ (related_def memory heap; Hmc_heap_preservation.lookup_object_def heap address);
    match heap with
    | M.Empty_heap _ -> unreachable_ ()
    | M.Allocate (a, rest) ->
      if address = a.M.address then (
        ghost_ (stored_def memory address object_);
        match Memory.load memory address (schema object_) with None -> unreachable_ () | Some wire -> wire)
      else fetch memory rest address object_ ()
