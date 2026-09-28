module B = Wasm_u32
module W = Hmc_word64
module Codec = Wasm_word_memory
module L = Hmc_linear_bytes
let (limb @ total) : (before : B.bytes) @ immutable -> (payload : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (old : W.limb) -> (value : W.limb) -> (old_tail : B.bytes) @ immutable ->
    (payload_tail : B.bytes) @ immutable -> (new_tail : B.bytes) @ immutable ->
    {u : unit | Codec.decode_limb before === Some (old, old_tail)
      && Codec.decode_limb payload === Some (value, payload_tail)
      && Codec.decode_limb after === Some (value, new_tail)
      && L.overlay payload_tail old_tail new_tail} ->
    {u : unit | L.overlay payload before after} @ ghost =
  fun before payload after old value old_tail payload_tail new_tail premise -> ghost_ (
    Codec.decode_limb_def before; Codec.decode_limb_def payload; Codec.decode_limb_def after;
    match before, payload, after with
    | B.Byte (_, (B.Byte (_, (B.Byte (_, (B.Byte (_, _) as b3)) as b2)) as b1)),
      B.Byte (_, (B.Byte (_, (B.Byte (_, (B.Byte (_, _) as p3)) as p2)) as p1)),
      B.Byte (_, (B.Byte (_, (B.Byte (_, (B.Byte (_, _) as a3)) as a2)) as a1)) ->
        L.overlay_def payload before after; L.overlay_def p1 b1 a1;
        L.overlay_def p2 b2 a2; L.overlay_def p3 b3 a3
    | _ -> ())
let (word @ total) : (before : B.bytes) @ immutable -> (payload : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (old : W.t) @ immutable -> (value : W.t) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Codec.decode before === Some (old, tail)
      && Codec.decode payload === Some (value, B.End)
      && Codec.decode after === Some (value, tail)} ->
    {u : unit | L.overlay payload before after} @ ghost = fun before payload after old value tail premise -> ghost_ (
    Codec.decode_def before; Codec.decode_def payload; Codec.decode_def after;
    match Codec.decode_limb before, Codec.decode_limb payload, Codec.decode_limb after with
    | Some (_, b), Some (_, p), Some (_, a) ->
      L.overlay_def B.End tail tail;
      limb b p a old.W.hi value.W.hi tail B.End tail ();
      limb before payload after old.W.lo value.W.lo b p a ()
    | _ -> ())
