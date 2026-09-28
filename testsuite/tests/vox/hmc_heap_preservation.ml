module W = Hmc_word64
module C = Hmc_tagged_cell
module R = Hmc_closure_semantics
module K = Hmc_closure_ir
module H = Hmc_frame_shape
module E = Hmc_heap_extent
module M = Hmc_heap_objects

let rec (lookup_valid @ total) : (table : K.table) @ immutable -> (heap : M.heap) @ immutable ->
    (address : W.limb) -> (value : R.V.value) @ immutable ->
    {u : unit | M.valid table heap && M.lookup (M.view heap) address === Some value} ->
    {u : unit | address < M.used heap && H.value table value} @ ghost = fun table heap address value premise -> ghost_ (
  M.valid_def table heap; M.view_def heap; M.lookup_def (M.view heap) address; M.used_def heap;
  match heap with
  | M.Empty_heap _ -> ()
  | M.Allocate (a, rest) ->
    M.slots_def a.M.object_; E.ordered (M.slots a.M.object_) a.M.address a.M.stop ();
    if address = a.M.address then M.object_valid_def table (M.view rest) a.M.object_
    else lookup_valid table rest address value ())
let (decode_valid @ total) : (table : K.table) @ immutable -> (heap : M.heap) @ immutable ->
    (reference : C.value) @ immutable -> (value : R.V.value) @ immutable ->
    {u : unit | M.valid table heap && M.decode heap reference === Some value} ->
    {u : unit | H.value table value} @ ghost = fun table heap reference value premise -> ghost_ (
  M.decode_def heap reference; M.decode_value_def (M.view heap) reference;
  match reference with
  | C.Cons_pointer address | C.Closure_pointer address -> lookup_valid table heap address value ()
  | _ -> H.value_def table value; H.valid_def table value; H.first_class_def value)
let[@def] rec (extends @ total) (larger : M.heap @ immutable) (smaller : M.heap @ immutable) = ghost_ (
  larger === smaller || match larger with M.Empty_heap _ -> false | M.Allocate (_, rest) -> extends rest smaller)
let rec (transitive @ total) : (a : M.heap) @ immutable -> (b : M.heap) @ immutable -> (c : M.heap) @ immutable ->
    {u : unit | extends a b && extends b c} -> {u : unit | extends a c} @ ghost = fun a b c premise -> ghost_ (
  extends_def a b; extends_def a c;
  if a === b then () else match a with M.Empty_heap _ -> () | M.Allocate (_, rest) -> transitive rest b c ())
let rec (lookup_preserve @ total) : (table : K.table) @ immutable -> (larger : M.heap) @ immutable -> (smaller : M.heap) @ immutable ->
    (address : W.limb) -> (value : R.V.value) @ immutable ->
    {u : unit | M.valid table larger && M.valid table smaller && extends larger smaller
      && M.lookup (M.view smaller) address === Some value} ->
    {u : unit | M.lookup (M.view larger) address === Some value} @ ghost = fun table larger smaller address value premise -> ghost_ (
  extends_def larger smaller;
  if larger === smaller then () else (
    M.valid_def table larger; M.view_def larger; M.lookup_def (M.view larger) address;
    match larger with M.Empty_heap _ -> () | M.Allocate (a, rest) ->
      lookup_preserve table rest smaller address value (); lookup_valid table rest address value ()))
let (decode_preserve @ total) : (table : K.table) @ immutable -> (larger : M.heap) @ immutable -> (smaller : M.heap) @ immutable ->
    (reference : C.value) @ immutable -> (value : R.V.value) @ immutable ->
    {u : unit | M.valid table larger && M.valid table smaller && extends larger smaller && M.decode smaller reference === Some value} ->
    {u : unit | M.decode larger reference === Some value} @ ghost = fun table larger smaller reference value premise -> ghost_ (
  M.decode_def smaller reference; M.decode_def larger reference;
  M.decode_value_def (M.view smaller) reference; M.decode_value_def (M.view larger) reference;
  match reference with C.Cons_pointer address | C.Closure_pointer address -> lookup_preserve table larger smaller address value () | _ -> ())
let rec (environment_preserve @ total) : (table : K.table) @ immutable -> (larger : M.heap) @ immutable -> (smaller : M.heap) @ immutable ->
    (captures : M.cells) @ immutable -> (env : R.V.value) @ immutable ->
    {u : unit | M.valid table larger && M.valid table smaller && extends larger smaller
      && M.decode_environment (M.view smaller) captures === Some env} ->
    {u : unit | M.decode_environment (M.view larger) captures === Some env} @ ghost = fun table larger smaller captures env premise -> ghost_ (
  M.decode_environment_def (M.view smaller) captures; M.decode_environment_def (M.view larger) captures;
  match captures with M.Empty -> () | M.Cell (head, rest) ->
    (match M.decode_value (M.view smaller) head, M.decode_environment (M.view smaller) rest with
    | Some value, Some tail ->
      M.decode_def smaller head; M.decode_def larger head; decode_preserve table larger smaller head value ();
      environment_preserve table larger smaller rest tail ()
    | _ -> ()))
let (object_preserve @ total) : (table : K.table) @ immutable -> (larger : M.heap) @ immutable -> (smaller : M.heap) @ immutable ->
    (object_ : M.object_) @ immutable -> (value : R.V.value) @ immutable ->
    {u : unit | M.valid table larger && M.valid table smaller && extends larger smaller
      && M.decode_object (M.view smaller) object_ === Some value} ->
    {u : unit | M.decode_object (M.view larger) object_ === Some value} @ ghost = fun table larger smaller object_ value premise -> ghost_ (
  M.decode_object_def (M.view smaller) object_; M.decode_object_def (M.view larger) object_;
  match object_ with
  | M.Cons (head, tail) ->
    (match M.decode_value (M.view smaller) head, M.decode_value (M.view smaller) tail with
    | Some h, Some t ->
      M.decode_def smaller head; M.decode_def smaller tail; M.decode_def larger head; M.decode_def larger tail;
      decode_preserve table larger smaller head h (); decode_preserve table larger smaller tail t ()
    | _ -> ())
  | M.Closure (_, captures) -> (match M.decode_environment (M.view smaller) captures with
    | None -> () | Some env -> environment_preserve table larger smaller captures env ()))

let[@def] rec (lookup_object @ total) (heap : M.heap @ immutable) (address : W.limb) = match heap with
  | M.Empty_heap _ -> None | M.Allocate (a, rest) -> if address = a.M.address then Some a.M.object_ else lookup_object rest address
let rec (fetch @ total) : (table : K.table) @ immutable -> (heap : M.heap) @ immutable -> (address : W.limb) ->
    (value : R.V.value) @ immutable -> {u : unit | M.valid table heap && M.lookup (M.view heap) address === Some value} ->
    {object_ : M.object_ | lookup_object heap address === Some object_ && M.decode_object (M.view heap) object_ === Some value} @ immutable =
  fun table heap address value premise ->
    ghost_ (M.valid_def table heap; M.view_def heap; M.lookup_def (M.view heap) address; lookup_object_def heap address);
    match heap with
    | M.Empty_heap _ -> unreachable_ ()
    | M.Allocate (a, rest) ->
      ghost_ (extends_def rest rest; extends_def heap rest);
      if address = a.M.address then (
        ghost_ (object_preserve table heap rest a.M.object_ value ()); a.M.object_)
      else
        let object_ = fetch table rest address value () in
        ghost_ (object_preserve table heap rest object_ value ()); object_
