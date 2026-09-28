module B = Wasm_u32
module W = Hmc_word64
module L = Hmc_linear_bytes
module P = Hmc_linear_preservation
module M = Wasm_memory
module S = Wasm_scalar
module Codec = Wasm_word_memory
let rec (fits @ total) : (payload : B.bytes) @ immutable -> (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    {u : unit | L.overlay payload before after} -> {u : unit | L.fits payload before} @ ghost =
  fun payload before after premise -> ghost_ (
    L.overlay_def payload before after; L.fits_def payload before;
    match payload, before, after with
    | B.Byte (_, p), B.Byte (_, b), B.Byte (_, a) -> fits p b a ()
    | _ -> ())
let rec (prefix @ total) : (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable -> (address : B.u32) ->
    (payload : B.bytes) @ immutable -> (old_suffix : B.bytes) @ immutable -> (new_suffix : B.bytes) @ immutable ->
    {u : unit | P.equal_prefix address before after && L.drop before address === Some old_suffix
      && L.drop after address === Some new_suffix && L.overlay payload old_suffix new_suffix} ->
    {u : unit | L.updated before address payload after} @ ghost =
  fun before after address payload old_suffix new_suffix premise -> ghost_ (
    P.equal_prefix_def address before after; L.drop_def before address; L.drop_def after address;
    L.updated_def before address payload after;
    if address = 0 then () else match before, after with
    | B.Byte (_, b), B.Byte (_, a) -> prefix b a (address - 1) payload old_suffix new_suffix ()
    | _ -> ())
let (correct @ total) : (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (base : B.u32) -> (offset : B.u32) -> (address : B.u32) ->
    (old_suffix : B.bytes) @ immutable -> (new_suffix : B.bytes) @ immutable ->
    (old : W.t) @ immutable -> (value : W.t) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | address = base + offset && address <= 4294967288
      && P.equal_prefix address before after && L.drop before address === Some old_suffix
      && L.drop after address === Some new_suffix
      && Codec.decode old_suffix === Some (old, tail) && Codec.decode new_suffix === Some (value, tail)} ->
    {u : unit | M.store before base offset (S.I64 value) === Some after} @ ghost =
  fun before after base offset address old_suffix new_suffix old value tail premise -> ghost_ (
    let payload = M.encode (S.I64 value) in
    M.width_def (S.I64 value); M.decode_def M.W64 payload;
    Wasm_word_overlay.word old_suffix payload new_suffix old value tail ();
    fits payload old_suffix new_suffix ();
    prefix before after address payload old_suffix new_suffix ();
    M.can_store_def before base offset (S.I64 value); M.address_def base offset M.W64; M.size_def M.W64;
    L.fits_at_def before address payload;
    match M.store before base offset (S.I64 value) with
    | None -> ()
    | Some actual -> Wasm_memory_unique.updated before address payload actual after ())
