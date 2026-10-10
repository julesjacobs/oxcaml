module D = Hm_declarative
module B = Wasm_u32
module C = Hmc_tagged_cell
module M = Hmc_heap_objects
module P = Hmc_heap_preservation
module K = Hmc_closure_ir
module R = Hmc_closure_semantics
module H = Hmc_frame_shape
module F = Hmc_frame_codec
module Wire = Hmc_heap_wire
module Memory = Hmc_memory_object
module Header = Hmc_memory_header
module Lookup = Hmc_memory_lookup
module Image = Hmc_heap_image

type pair = {head : C.value; tail : C.value}
let[@def] (read_cons @ total) (memory : B.bytes @ immutable) (reference : C.value @ immutable) = match reference with
  | C.Cons_pointer address -> (match Memory.load memory address Wire.Cons_schema with
    | Some (Wire.Cons (head, tail)) -> Some {head; tail} | _ -> None)
  | _ -> None
let (cons_correct @ total) : (table : K.table) @ immutable -> (heap : M.heap) @ immutable -> (memory : B.bytes) @ immutable ->
    (reference : C.value) @ immutable -> (head : R.V.value) @ immutable -> (tail : R.V.value) @ immutable ->
    {u : unit | M.valid table heap && Image.related memory heap && M.decode heap reference === Some (R.V.Cons (head, tail))} ->
    {u : unit | match read_cons memory reference with None -> false | Some out ->
      M.decode heap out.head === Some head && M.decode heap out.tail === Some tail
      && (match reference with C.Cons_pointer address -> P.lookup_object heap address === Some (M.Cons (out.head, out.tail)) | _ -> false)} @ ghost =
  fun table heap memory reference head tail premise -> ghost_ (
    read_cons_def memory reference; M.decode_def heap reference; M.decode_value_def (M.view heap) reference;
    match reference with
    | C.Cons_pointer address ->
      let object_ = P.fetch table heap address (R.V.Cons (head, tail)) () in
      M.decode_object_def (M.view heap) object_;
      let wire = Image.fetch memory heap address object_ () in
      Image.schema_def object_; Wire.corresponds_def object_ wire;
      (match object_ with M.Cons (a, b) -> M.decode_def heap a; M.decode_def heap b | _ -> ())
    | _ -> ())
let rec (capture_size @ total) : (context : D.context) @ immutable -> (view : M.view) @ immutable ->
    (captures : M.cells) @ immutable -> (env : R.V.value) @ immutable ->
    {u : unit | M.decode_environment view captures === Some env && H.environment context env} ->
    {u : unit | M.length captures === F.locals_size context} @ ghost = fun context view captures env premise -> ghost_ (
  M.decode_environment_def view captures; H.environment_def context env; M.length_def captures; F.locals_size_def context;
  match context, captures with
  | D.Binding (_, rest), M.Cell (_, tail) ->
    (match M.decode_environment view tail with None -> () | Some remaining -> capture_size rest view tail remaining ())
  | _ -> ())
type closure = {code : D.index; captures : M.cells}
let[@def] (read_closure @ total) (table : K.table @ immutable) (memory : B.bytes @ immutable) (reference : C.value @ immutable) = match reference with
  | C.Closure_pointer address -> (match Header.code memory address with
    | None -> None
    | Some number -> (match Lookup.lookup table number with
      | None -> None
      | Some (code, entry) -> (match Memory.load memory address (Wire.Closure_schema (F.locals_size entry.K.captured)) with
        | Some (Wire.Closure (stored_code, captures)) -> if stored_code = number then Some {code; captures} else None
        | _ -> None)))
  | _ -> None
let (closure_correct @ total) : (table : K.table) @ immutable -> (heap : M.heap) @ immutable -> (memory : B.bytes) @ immutable ->
    (reference : C.value) @ immutable -> (code : D.index) @ immutable -> (env : R.V.value) @ immutable ->
    {u : unit | M.valid table heap && Image.related memory heap && M.decode heap reference === Some (R.V.Closure (code, env))} ->
    {u : unit | match read_closure table memory reference with None -> false | Some out ->
      out.code === code && M.decode_environment (M.view heap) out.captures === Some env
      && (match reference with C.Closure_pointer address -> P.lookup_object heap address === Some (M.Closure (out.code, out.captures)) | _ -> false)} @ ghost =
  fun table heap memory reference code env premise -> ghost_ (
    read_closure_def table memory reference; M.decode_def heap reference; M.decode_value_def (M.view heap) reference;
    P.decode_valid table heap reference (R.V.Closure (code, env)) ();
    H.value_def table (R.V.Closure (code, env)); H.valid_def table (R.V.Closure (code, env));
    match reference with
    | C.Closure_pointer address ->
      let object_ = P.fetch table heap address (R.V.Closure (code, env)) () in
      M.decode_object_def (M.view heap) object_;
      let wire = Image.fetch memory heap address object_ () in
      Image.schema_def object_; Wire.corresponds_def object_ wire;
      (match object_, wire, K.lookup table code with
      | M.Closure (_, captures), Wire.Closure (number, _), Some entry ->
        capture_size entry.K.captured (M.view heap) captures env ();
        Header.load memory address (M.length captures) number captures (); Header.code_def memory address;
        Lookup.correct table code number ()
      | _ -> ())
    | _ -> ())
