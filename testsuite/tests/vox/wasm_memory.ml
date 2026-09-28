module D = Hm_declarative
module B = Wasm_u32
module W = Hmc_word64
module S = Wasm_scalar
module C = Hmc_tagged_cell
module L = Hmc_linear_bytes
module Codec = Wasm_word_memory

type width = W32 | W64 [@@inductive]
let[@def] (width @ total) (value : S.value @ immutable) =
  match value with S.I32 _ -> W32 | S.I64 _ -> W64
let[@def] (size @ total) (width : width @ immutable) : {n : int | n = 4 || n = 8} = match width with W32 -> 4 | W64 -> 8
let[@def] (count @ total) (width : width @ immutable) =
  match width with W32 -> C.four D.Z | W64 -> C.eight D.Z
let[@def] (address @ total) (base : B.u32) (offset : B.u32) (width : width @ immutable) : B.u32 option =
  let effective = base + offset in
  if effective + size width <= 4294967296 then Some effective else None
let[@def] (decode @ total) (width : width @ immutable) (bytes : B.bytes @ immutable) =
  match width with
  | W32 -> (match Codec.decode_limb bytes with Some (value, B.End) -> Some (S.I32 value) | _ -> None)
  | W64 -> (match Codec.decode bytes with Some (value, B.End) -> Some (S.I64 value) | _ -> None)
let (encode @ total) : (value : S.value) @ immutable ->
    {bytes : B.bytes | decode (width value) bytes === Some value
      && C.length bytes === count (width value)} @ immutable = fun value ->
  ghost_ (width_def value; C.length_def B.End);
  match value with
  | S.I32 limb ->
    let bytes = Codec.encode_limb limb B.End in
    ghost_ (decode_def W32 bytes; count_def W32; C.limb_size bytes limb B.End ()); bytes
  | S.I64 word ->
    let bytes = Codec.encode word B.End in
    ghost_ (decode_def W64 bytes; count_def W64; C.word_size bytes word B.End ()); bytes
let[@def] (load @ total) (memory : B.bytes @ immutable) (base : B.u32) (offset : B.u32)
    (width : width @ immutable) =
  match address base offset width with
  | None -> None
  | Some address -> (match L.load memory address (count width) with
    | None -> None | Some bytes -> decode width bytes)
let[@def] (can_store @ total) (memory : B.bytes @ immutable) (base : B.u32) (offset : B.u32)
    (value : S.value @ immutable) =
  match address base offset (width value) with
  | None -> false
  | Some address -> L.fits_at memory address (encode value)
let (store @ total) : (memory : B.bytes) @ immutable -> (base : B.u32) -> (offset : B.u32) ->
    (value : S.value) @ immutable ->
    {out : B.bytes option | match out with
      | None -> not (can_store memory base offset value)
      | Some after -> can_store memory base offset value
        && load after base offset (width value) === Some value
        && C.length after === C.length memory
        && (match address base offset (width value) with None -> false
          | Some address -> L.updated memory address (encode value) after)} @ immutable =
  fun memory base offset value ->
    ghost_ (can_store_def memory base offset value);
    match address base offset (width value) with
    | None -> None
    | Some address ->
      let payload = encode value in
      (match L.store memory address payload with
      | None -> None
      | Some after -> ghost_ (load_def after base offset (width value)); Some after)
