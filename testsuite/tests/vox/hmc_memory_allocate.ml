module W = Hmc_word64
module B = Wasm_u32
module C = Hmc_tagged_cell
module M = Hmc_heap_objects
module K = Hmc_closure_ir
module E = Hmc_heap_extent
module A = Hmc_heap_allocate
module P = Hmc_heap_preservation
module Index = Hmc_u32_index
module Wire = Hmc_heap_wire
module Image = Hmc_heap_image
module Bounds = Hmc_linear_bounds
module Memory = Hmc_memory_object

type allocation = {memory : B.bytes; frontier : W.limb; reference : C.value}
type result = Allocated of allocation | Exhausted [@@inductive]
let[@def] (correct @ total) (table : K.table @ immutable) (memory : B.bytes @ immutable) (heap : M.heap @ immutable)
    (limit : W.limb) (object_ : M.object_ @ immutable) (out : result @ immutable) = ghost_ (match out with
  | Exhausted -> not (E.fits (M.slots object_) (M.used heap) limit)
  | Allocated out ->
    let next = M.Allocate ({M.address = M.used heap; stop = out.frontier; object_}, heap) in
    A.correct table heap limit object_ (A.Allocated {A.heap = next; reference = out.reference})
    && Image.related out.memory next && Bounds.covers out.memory limit && C.length out.memory === C.length memory
    && Hmc_linear_bytes.drop memory limit === Hmc_linear_bytes.drop out.memory limit)
let (allocate @ total) : (table : K.table) @ ghost -> (capacity : W.limb) -> (memory : B.bytes) @ immutable ->
    (heap : M.heap) @ ghost -> (frontier : W.limb) -> (limit : W.limb) -> (object_ : M.object_) @ immutable ->
    {u : unit | Index.fits (K.size table) capacity && M.valid table heap && Image.related memory heap
      && Bounds.covers memory limit && frontier = M.used heap && frontier <= limit
      && M.object_valid table (M.view heap) object_} ->
    {out : result | correct table memory heap limit object_ out} @ immutable = fun table capacity memory heap frontier limit object_ premise ->
  match E.reserve (M.slots object_) frontier limit with
  | None -> ghost_ (correct_def table memory heap limit object_ Exhausted); Exhausted
  | Some stop ->
    ghost_ (Hmc_heap_code_bounds.object_ table capacity (M.view heap) object_ (); Image.object_encodable_def capacity object_);
    (match Wire.lower capacity object_ with
    | None -> unreachable_ ()
    | Some wire ->
      let updated = Memory.store memory limit frontier stop wire () in
      let reference = M.reference object_ frontier in
      ghost_ (
        let next = M.Allocate ({M.address = frontier; stop; object_}, heap) in
        M.valid_def table next; M.used_def next; P.extends_def heap heap; P.extends_def next heap;
        M.decode_def next reference; M.reference_def object_ frontier; M.view_def next;
        M.decode_value_def (M.view next) reference; M.lookup_def (M.view next) frontier;
        M.object_valid_def table (M.view heap) object_; M.decode_object_def (M.view heap) object_;
        A.correct_def table heap limit object_ (A.Allocated {A.heap = next; reference});
        Image.preserve table memory updated heap frontier (); Image.schema_agreement object_ wire ();
        Image.stored_def updated frontier object_; Image.related_def updated next);
      ghost_ (let _ = Bounds.suffix memory limit stop () in
        let _ = Bounds.suffix updated limit stop () in
        Bounds.covers_def memory stop; Bounds.covers_def updated stop;
        Hmc_memory_suffix.drop memory updated stop limit ());
      let out = Allocated {memory = updated; frontier = stop; reference} in
      ghost_ (correct_def table memory heap limit object_ out); out)
