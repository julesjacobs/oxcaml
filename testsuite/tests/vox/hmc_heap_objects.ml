module D = Hm_declarative
module W = Hmc_word64
module C = Hmc_tagged_cell
module R = Hmc_closure_semantics
module K = Hmc_closure_ir
module H = Hmc_frame_shape
module E = Hmc_heap_extent

type cells = Empty | Cell of C.value * cells [@@inductive]
let[@def] rec (length @ total) (cells : cells @ immutable) = match cells with Empty -> D.Z | Cell (_, rest) -> D.S (length rest)
type object_ = Cons of C.value * C.value | Closure of D.index * cells [@@inductive]
let[@def] (slots @ total) (object_ : object_ @ immutable) = match object_ with
  | Cons _ -> D.S (D.S D.Z) | Closure (_, captures) -> D.S (length captures)
let[@def] (reference @ total) (object_ : object_ @ immutable) (address : W.limb) = match object_ with
  | Cons _ -> C.Cons_pointer address | Closure _ -> C.Closure_pointer address

type view = No_values | Value of W.limb * R.V.value option * view [@@inductive]
let[@def] rec (lookup @ total) (view : view @ immutable) (address : W.limb) = match view with
  | No_values -> None | Value (at, value, rest) -> if address = at then value else lookup rest address
let[@def] (decode_value @ total) (view : view @ immutable) (value : C.value @ immutable) = match value with
  | C.Boolean b -> Some (if b then R.V.True else R.V.False)
  | C.Word w -> Some (R.V.Word w) | C.Nil -> Some R.V.Nil
  | C.Cons_pointer address -> (match lookup view address with Some (R.V.Cons _ as value) -> Some value | _ -> None)
  | C.Closure_pointer address -> (match lookup view address with Some (R.V.Closure _ as value) -> Some value | _ -> None)
let[@def] rec (decode_environment @ total) (view : view @ immutable) (cells : cells @ immutable) = match cells with
  | Empty -> Some R.V.Empty
  | Cell (head, rest) -> (match decode_value view head, decode_environment view rest with
    | Some head, Some tail -> Some (R.V.Bind (head, tail)) | _ -> None)
let[@def] (decode_object @ total) (view : view @ immutable) (object_ : object_ @ immutable) = match object_ with
  | Cons (head, tail) -> (match decode_value view head, decode_value view tail with
    | Some head, Some tail -> Some (R.V.Cons (head, tail)) | _ -> None)
  | Closure (code, captures) -> (match decode_environment view captures with None -> None | Some env -> Some (R.V.Closure (code, env)))
let[@def] (object_valid @ total) (table : K.table @ immutable) (view : view @ immutable) (object_ : object_ @ immutable) = ghost_ (
  match decode_object view object_ with None -> false | Some value -> H.value table value)

type allocation = {address : W.limb; stop : W.limb; object_ : object_}
type heap = Empty_heap of W.limb | Allocate of allocation * heap [@@inductive]
let[@def] (used @ total) (heap : heap @ immutable) : W.limb = match heap with Empty_heap base -> base | Allocate (a, _) -> a.stop
let[@def] rec (view @ total) (heap : heap @ immutable) = match heap with
  | Empty_heap _ -> No_values
  | Allocate (a, rest) -> let previous = view rest in Value (a.address, decode_object previous a.object_, previous)
let[@def] (decode @ total) (heap : heap @ immutable) (value : C.value @ immutable) = decode_value (view heap) value
let[@def] rec (valid @ total) (table : K.table @ immutable) (heap : heap @ immutable) = ghost_ (match heap with
  | Empty_heap _ -> true
  | Allocate (a, rest) -> valid table rest && a.address = used rest && E.span (slots a.object_) a.address a.stop
    && object_valid table (view rest) a.object_)
