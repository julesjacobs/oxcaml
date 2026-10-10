module D = Hm_declarative
module W = Hmc_word64
module B = Wasm_u32
module C = Hmc_tagged_cell
module M = Hmc_heap_objects
module I = Hmc_u32_index

let[@def] rec (bytes_size @ total) (cells : D.index @ immutable) (tail : D.index @ immutable) = match cells with
  | D.Z -> tail | D.S n -> C.sixteen (bytes_size n tail)
let[@def] rec (decode_cells @ total) (count : D.index @ immutable) (bytes : B.bytes @ immutable) = match count with
  | D.Z -> Some (M.Empty, bytes)
  | D.S n -> (match C.decode bytes with None -> None | Some (value, rest) ->
    match decode_cells n rest with None -> None | Some (tail, remaining) -> Some (M.Cell (value, tail), remaining))
let rec (encode_cells @ total) : (cells : M.cells) @ immutable -> (tail : B.bytes) @ immutable ->
    {out : B.bytes | decode_cells (M.length cells) out === Some (cells, tail)
      && C.length out === bytes_size (M.length cells) (C.length tail)} @ immutable = fun cells tail ->
  ghost_ (M.length_def cells; bytes_size_def (M.length cells) (C.length tail));
  match cells with
  | M.Empty -> ghost_ (decode_cells_def D.Z tail); tail
  | M.Cell (head, rest) ->
    let after = encode_cells rest tail in
    let out = C.encode head after in
    ghost_ (decode_cells_def (M.length cells) out); out

type object_ = Cons of C.value * C.value | Closure of W.limb * M.cells [@@inductive]
type schema = Cons_schema | Closure_schema of D.index [@@inductive]
let[@def] (schema @ total) (object_ : object_ @ immutable) = match object_ with Cons _ -> Cons_schema | Closure (_, captures) -> Closure_schema (M.length captures)
let[@def] (slots @ total) (object_ : object_ @ immutable) = match object_ with Cons _ -> D.S (D.S D.Z) | Closure (_, captures) -> D.S (M.length captures)
let[@def] (decode @ total) (schema : schema @ immutable) (bytes : B.bytes @ immutable) = match schema with
  | Cons_schema -> (match C.decode bytes with None -> None | Some (head, rest) ->
    match C.decode rest with None -> None | Some (tail, remaining) -> Some (Cons (head, tail), remaining))
  | Closure_schema count -> (match C.decode bytes with
    | Some (C.Word code, rest) -> if code.W.hi <> 0 then None else
      (match decode_cells count rest with None -> None | Some (captures, remaining) -> Some (Closure (code.W.lo, captures), remaining))
    | _ -> None)
let (encode @ total) : (object_ : object_) @ immutable -> (tail : B.bytes) @ immutable ->
    {out : B.bytes | decode (schema object_) out === Some (object_, tail)
      && C.length out === bytes_size (slots object_) (C.length tail)} @ immutable = fun object_ tail ->
  ghost_ (schema_def object_; slots_def object_);
  match object_ with
  | Cons (head, next) ->
    let after = C.encode next tail in let out = C.encode head after in
    ghost_ (decode_def Cons_schema out; bytes_size_def (D.S (D.S D.Z)) (C.length tail);
      bytes_size_def (D.S D.Z) (C.length tail); bytes_size_def D.Z (C.length tail)); out
  | Closure (code, captures) ->
    let after = encode_cells captures tail in
    let out = C.encode (C.Word {W.lo = code; hi = 0}) after in
    ghost_ (decode_def (Closure_schema (M.length captures)) out; bytes_size_def (D.S (M.length captures)) (C.length tail)); out
let[@def] (corresponds @ total) (source : M.object_ @ immutable) (target : object_ @ immutable) = ghost_ (match source, target with
  | M.Cons (a, b), Cons (x, y) -> a === x && b === y
  | M.Closure (code, captures), Closure (number, values) -> I.represents code number && captures === values
  | _ -> false)
let (lower @ total) : (capacity : W.limb) -> (source : M.object_) @ immutable ->
    {out : object_ option | match out with
      | None -> (match source with M.Closure (code, _) -> not (I.fits code capacity) | _ -> false)
      | Some target -> corresponds source target && slots target === M.slots source} @ immutable = fun capacity source ->
  ghost_ (M.slots_def source);
  match source with
  | M.Cons (a, b) -> let out = Cons (a, b) in ghost_ (corresponds_def source out; slots_def out); Some out
  | M.Closure (code, captures) ->
    (match I.encode capacity code with None -> None | Some number ->
      let out = Closure (number, captures) in ghost_ (corresponds_def source out; slots_def out); Some out)
