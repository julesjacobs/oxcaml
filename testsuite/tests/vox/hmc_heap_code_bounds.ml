module D = Hm_declarative
module W = Hmc_word64
module K = Hmc_closure_ir
module M = Hmc_heap_objects
module H = Hmc_frame_shape
module R = Hmc_closure_semantics
module I = Hmc_u32_index
module Cap = Hmc_frame_capacity
module Image = Hmc_heap_image

let rec (weaken @ total) : (left : D.index) @ immutable -> (right : D.index) @ immutable ->
    {u : unit | Cap.le left right} -> {u : unit | Cap.le left (D.S right)} @ ghost = fun left right premise -> ghost_ (
  Cap.le_def left right; Cap.le_def left (D.S right);
  match left, right with D.S a, D.S b -> weaken a b () | _ -> ())
let rec (lookup @ total) : (table : K.table) @ immutable -> (id : D.index) @ immutable -> (entry : K.entry) @ immutable ->
    {u : unit | K.lookup table id === Some entry} -> {u : unit | Cap.le id (K.size table)} @ ghost = fun table id entry premise -> ghost_ (
  K.lookup_def table id; K.size_def table;
  match table with K.Empty -> () | K.Add (_, rest) ->
    if Hm_elaboration_check.index_equal id (K.size rest) then (Cap.reflexive id; weaken id id ())
    else (lookup rest id entry (); weaken id (K.size rest) ()))
let rec (fits_smaller @ total) : (small : D.index) @ immutable -> (large : D.index) @ immutable -> (capacity : W.limb) ->
    {u : unit | Cap.le small large && I.fits large capacity} -> {u : unit | I.fits small capacity} @ ghost =
  fun small large capacity premise -> ghost_ (
    Cap.le_def small large; I.fits_def small capacity; I.fits_def large capacity;
    match small, large with D.S a, D.S b -> fits_smaller a b (capacity - 1) () | _ -> ())
let (object_ @ total) : (table : K.table) @ immutable -> (capacity : W.limb) -> (view : M.view) @ immutable ->
    (object_ : M.object_) @ immutable -> {u : unit | I.fits (K.size table) capacity && M.object_valid table view object_} ->
    {u : unit | Image.object_encodable capacity object_} @ ghost = fun table capacity view object_ premise -> ghost_ (
  Image.object_encodable_def capacity object_; M.object_valid_def table view object_; M.decode_object_def view object_;
  match object_ with
  | M.Cons _ -> ()
  | M.Closure (id, captures) ->
    (match M.decode_environment view captures with None -> () | Some env ->
      H.value_def table (R.V.Closure (id, env)); H.valid_def table (R.V.Closure (id, env));
      match K.lookup table id with None -> () | Some entry -> lookup table id entry (); fits_smaller id (K.size table) capacity ()))
let rec (encodable @ total) : (table : K.table) @ immutable -> (capacity : W.limb) -> (heap : M.heap) @ immutable ->
    {u : unit | I.fits (K.size table) capacity && M.valid table heap} ->
    {u : unit | Image.encodable capacity heap} @ ghost = fun table capacity heap premise -> ghost_ (
  M.valid_def table heap; Image.encodable_def capacity heap;
  match heap with M.Empty_heap _ -> () | M.Allocate (a, rest) -> object_ table capacity (M.view rest) a.M.object_ (); encodable table capacity rest ())
