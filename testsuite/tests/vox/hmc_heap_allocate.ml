module W = Hmc_word64
module C = Hmc_tagged_cell
module K = Hmc_closure_ir
module M = Hmc_heap_objects
module E = Hmc_heap_extent
module P = Hmc_heap_preservation

type allocation = {heap : M.heap; reference : C.value}
type result = Allocated of allocation | Exhausted [@@inductive]
let[@def] (correct @ total) (table : K.table @ immutable) (heap : M.heap @ immutable) (limit : W.limb)
    (object_ : M.object_ @ immutable) (out : result @ immutable) = ghost_ (match out with
      | Exhausted -> not (E.fits (M.slots object_) (M.used heap) limit)
      | Allocated a -> M.valid table a.heap && P.extends a.heap heap && M.used a.heap <= limit
        && E.span (M.slots object_) (M.used heap) (M.used a.heap)
        && a.heap === M.Allocate ({M.address = M.used heap; stop = M.used a.heap; object_}, heap)
        && a.reference === M.reference object_ (M.used heap)
        && M.decode a.heap a.reference === M.decode_object (M.view heap) object_)
let (allocate @ total) : (table : K.table) @ immutable -> (heap : M.heap) @ immutable -> (limit : W.limb) ->
    (object_ : M.object_) @ immutable ->
    {u : unit | M.valid table heap && M.used heap <= limit && M.object_valid table (M.view heap) object_} ->
    {out : result | correct table heap limit object_ out} @ immutable =
  fun table heap limit object_ premise ->
    match E.reserve (M.slots object_) (M.used heap) limit with
    | None -> ghost_ (correct_def table heap limit object_ Exhausted); Exhausted
    | Some stop ->
      let next = M.Allocate ({M.address = M.used heap; stop; object_}, heap) in
      let reference = M.reference object_ (M.used heap) in
      ghost_ (M.valid_def table next; M.used_def next;
        P.extends_def heap heap; P.extends_def next heap;
        M.decode_def next reference; M.reference_def object_ (M.used heap); M.view_def next;
        M.decode_value_def (M.view next) reference; M.lookup_def (M.view next) (M.used heap);
        M.object_valid_def table (M.view heap) object_; M.decode_object_def (M.view heap) object_);
      let out = Allocated {heap = next; reference} in
      ghost_ (correct_def table heap limit object_ out); out

let (sufficient @ total) : (table : K.table) @ immutable -> (heap : M.heap) @ immutable -> (limit : W.limb) ->
    (object_ : M.object_) @ immutable ->
    {u : unit | M.valid table heap && M.used heap <= limit && M.object_valid table (M.view heap) object_
      && E.fits (M.slots object_) (M.used heap) limit} ->
    {out : allocation | correct table heap limit object_ (Allocated out)} @ immutable = fun table heap limit object_ premise ->
  let result = allocate table heap limit object_ () in
  ghost_ (correct_def table heap limit object_ result);
  match result with Allocated out -> out | Exhausted -> unreachable_ ()
