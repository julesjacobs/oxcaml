module D = Hm_declarative
module W = Hmc_word64
module K = Hmc_closure_ir
module M = Hmc_heap_objects
module P = Hmc_heap_preservation
module E = Hmc_heap_extent

let[@def] rec (base @ total) (heap : M.heap @ immutable) = match heap with M.Empty_heap start -> start | M.Allocate (_, rest) -> base rest
let rec (ordered @ total) : (table : K.table) @ immutable -> (heap : M.heap) @ immutable ->
    {u : unit | M.valid table heap} -> {u : unit | base heap <= M.used heap} @ ghost = fun table heap premise -> ghost_ (
  M.valid_def table heap; base_def heap; M.used_def heap;
  match heap with M.Empty_heap _ -> () | M.Allocate (a, rest) ->
    ordered table rest (); E.ordered (M.slots a.M.object_) a.M.address a.M.stop ())
let rec (allocation @ total) : (table : K.table) @ immutable -> (heap : M.heap) @ immutable ->
    (address : W.limb) -> (object_ : M.object_) @ immutable ->
    {u : unit | M.valid table heap && P.lookup_object heap address === Some object_} ->
    {out : M.allocation | out.M.address = address && out.M.object_ === object_
      && base heap <= address && out.M.stop <= M.used heap && E.span (M.slots object_) address out.M.stop} @ immutable =
  fun table heap address object_ premise ->
    ghost_ (M.valid_def table heap; P.lookup_object_def heap address; base_def heap; M.used_def heap);
    match heap with
    | M.Empty_heap _ -> unreachable_ ()
    | M.Allocate (a, rest) ->
      ghost_ (ordered table rest (); E.ordered (M.slots a.M.object_) a.M.address a.M.stop ());
      if address = a.M.address then a else allocation table rest address object_ ()
let rec (slot @ total) : (cells : D.index) @ immutable -> (index : D.index) @ immutable -> (start : W.limb) -> (stop : W.limb) ->
    {u : unit | E.span cells start stop && D.present cells index} ->
    {address : W.limb | E.span index start address && start <= address && address + 16 <= stop} =
  fun cells index start stop premise ->
    ghost_ (E.span_def cells start stop; D.present_def cells index; E.ordered cells start stop ());
    match cells, index with
    | D.S _, D.Z -> ghost_ (E.span_def D.Z start start); start
    | D.S rest, D.S i ->
      let address = slot rest i (start + 16) stop () in
      ghost_ (E.span_def index start address); address
    | _ -> unreachable_ ()
let (object_slot @ total) : (table : K.table) @ immutable -> (heap : M.heap) @ immutable ->
    (reference : W.limb) -> (object_ : M.object_) @ immutable -> (index : D.index) @ immutable ->
    {u : unit | M.valid table heap && P.lookup_object heap reference === Some object_ && D.present (M.slots object_) index} ->
    {address : W.limb | base heap <= address && address + 16 <= M.used heap
      && E.span index reference address} = fun table heap reference object_ index premise ->
  let bounds = allocation table heap reference object_ () in
  let address = slot (M.slots object_) index reference bounds.M.stop () in address
