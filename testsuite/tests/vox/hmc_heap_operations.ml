module D = Hm_declarative
module W = Hmc_word64
module C = Hmc_tagged_cell
module R = Hmc_closure_semantics
module K = Hmc_closure_ir
module H = Hmc_frame_shape
module M = Hmc_heap_objects
module P = Hmc_heap_preservation
module A = Hmc_heap_allocate

let[@def] (live @ total) (heap : M.heap @ immutable) (value : C.value @ immutable) = ghost_ (not (M.decode heap value === None))
let (cons @ total) : (table : K.table) @ immutable -> (heap : M.heap) @ immutable -> (limit : W.limb) ->
    (head : C.value) @ immutable -> (tail : C.value) @ immutable ->
    {u : unit | M.valid table heap && M.used heap <= limit && live heap head && live heap tail} ->
    {out : A.result | A.correct table heap limit (M.Cons (head, tail)) out} @ immutable = fun table heap limit head tail premise ->
  let object_ = M.Cons (head, tail) in
  ghost_ (live_def heap head; live_def heap tail; M.decode_def heap head; M.decode_def heap tail;
    M.object_valid_def table (M.view heap) object_; M.decode_object_def (M.view heap) object_;
    match M.decode heap head, M.decode heap tail with
    | Some h, Some t ->
      P.decode_valid table heap head h (); P.decode_valid table heap tail t ();
      H.value_def table h; H.value_def table t;
      H.value_def table (R.V.Cons (h, t)); H.valid_def table (R.V.Cons (h, t)); H.first_class_def (R.V.Cons (h, t))
    | _ -> ());
  A.allocate table heap limit object_ ()
let rec (environment_valid @ total) : (table : K.table) @ immutable -> (heap : M.heap) @ immutable ->
    (captures : M.cells) @ immutable -> (env : R.V.value) @ immutable ->
    {u : unit | M.valid table heap && M.decode_environment (M.view heap) captures === Some env} ->
    {u : unit | H.valid table env} @ ghost = fun table heap captures env premise -> ghost_ (
  M.decode_environment_def (M.view heap) captures;
  match captures with
  | M.Empty -> H.valid_def table env
  | M.Cell (head, rest) -> (match M.decode_value (M.view heap) head, M.decode_environment (M.view heap) rest with
    | Some value, Some tail ->
      M.decode_def heap head; P.decode_valid table heap head value (); H.value_def table value;
      environment_valid table heap rest tail (); H.valid_def table env
    | _ -> ()))
let (closure @ total) : (table : K.table) @ immutable -> (heap : M.heap) @ immutable -> (limit : W.limb) ->
    (code : D.index) @ immutable -> (captures : M.cells) @ immutable ->
    {u : unit | M.valid table heap && M.used heap <= limit
      && (match K.lookup table code, M.decode_environment (M.view heap) captures with
        | Some entry, Some env -> H.environment entry.K.captured env | _ -> false)} ->
    {out : A.result | A.correct table heap limit (M.Closure (code, captures)) out} @ immutable = fun table heap limit code captures premise ->
  let object_ = M.Closure (code, captures) in
  ghost_ (M.object_valid_def table (M.view heap) object_; M.decode_object_def (M.view heap) object_;
    match K.lookup table code, M.decode_environment (M.view heap) captures with
    | Some _, Some env ->
      environment_valid table heap captures env ();
      H.value_def table (R.V.Closure (code, env)); H.valid_def table (R.V.Closure (code, env)); H.first_class_def (R.V.Closure (code, env))
    | _ -> ());
  A.allocate table heap limit object_ ()

type pair = {head : C.value; tail : C.value}
let (read_cons @ total) : (table : K.table) @ immutable -> (heap : M.heap) @ immutable -> (reference : C.value) @ immutable ->
    (head : R.V.value) @ immutable -> (tail : R.V.value) @ immutable ->
    {u : unit | M.valid table heap && M.decode heap reference === Some (R.V.Cons (head, tail))} ->
    {out : pair | M.decode heap out.head === Some head && M.decode heap out.tail === Some tail} @ immutable =
  fun table heap reference head tail premise ->
    ghost_ (M.decode_def heap reference; M.decode_value_def (M.view heap) reference);
    match reference with
    | C.Cons_pointer address ->
      let object_ = P.fetch table heap address (R.V.Cons (head, tail)) () in
      ghost_ (M.decode_object_def (M.view heap) object_);
      (match object_ with
      | M.Cons (head, tail) -> ghost_ (M.decode_def heap head; M.decode_def heap tail); {head; tail}
      | _ -> unreachable_ ())
    | _ -> unreachable_ ()

type invocation = {code : D.index; environment : M.cells}
let (invoke @ total) : (table : K.table) @ immutable -> (heap : M.heap) @ immutable ->
    (reference : C.value) @ immutable -> (arg : C.value) @ immutable ->
    (code : D.index) @ immutable -> (captures : R.V.value) @ immutable -> (argument : R.V.value) @ immutable ->
    {u : unit | M.valid table heap && M.decode heap reference === Some (R.V.Closure (code, captures))
      && M.decode heap arg === Some argument} ->
    {out : invocation | out.code === code && match K.lookup table code with None -> false | Some entry ->
      M.decode_environment (M.view heap) out.environment === Some (R.V.Bind (argument,
        if entry.K.recursive then R.V.Bind (R.V.Closure (code, captures), captures) else captures))} @ immutable =
  fun table heap reference arg code captures argument premise ->
    ghost_ (M.decode_def heap reference; M.decode_value_def (M.view heap) reference;
      P.decode_valid table heap reference (R.V.Closure (code, captures)) ();
      H.value_def table (R.V.Closure (code, captures)); H.valid_def table (R.V.Closure (code, captures)));
    match reference with
    | C.Closure_pointer address ->
      let object_ = P.fetch table heap address (R.V.Closure (code, captures)) () in
      ghost_ (M.decode_object_def (M.view heap) object_);
      (match object_, K.lookup table code with
      | M.Closure (id, values), Some entry ->
        let tail = if entry.K.recursive then M.Cell (reference, values) else values in
        let environment = M.Cell (arg, tail) in
        ghost_ (M.decode_environment_def (M.view heap) environment; M.decode_def heap arg;
          if entry.K.recursive then M.decode_environment_def (M.view heap) tail else ());
        {code = id; environment}
      | _ -> unreachable_ ())
    | _ -> unreachable_ ()
