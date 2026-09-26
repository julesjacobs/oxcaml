module B = Wasm_u32
module W = Hmc_word64
module Codec = Wasm_word_memory
module P = Hmc_linear_preservation
let (limb @ total) : (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable -> (value : W.limb) ->
    (left : B.bytes) @ immutable -> (right : B.bytes) @ immutable -> (count : B.u32) -> (total : B.u32) ->
    {u : unit | Codec.decode_limb before === Some (value, left) && Codec.decode_limb after === Some (value, right)
      && total = count + 4 && P.equal_prefix count left right} ->
    {u : unit | P.equal_prefix total before after} @ ghost = fun before after value left right count total premise -> ghost_ (
    Codec.decode_limb_def before; Codec.decode_limb_def after;
    match before, after with
    | B.Byte (_, (B.Byte (_, (B.Byte (_, (B.Byte (_, _) as b3)) as b2)) as b1)),
      B.Byte (_, (B.Byte (_, (B.Byte (_, (B.Byte (_, _) as a3)) as a2)) as a1)) ->
      P.equal_prefix_def total before after; P.equal_prefix_def (total - 1) b1 a1;
      P.equal_prefix_def (total - 2) b2 a2; P.equal_prefix_def (total - 3) b3 a3
    | _ -> ())
let (word @ total) : (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable -> (value : W.t) @ immutable ->
    (left : B.bytes) @ immutable -> (right : B.bytes) @ immutable -> (count : B.u32) -> (total : B.u32) ->
    {u : unit | Codec.decode before === Some (value, left) && Codec.decode after === Some (value, right)
      && total = count + 8 && P.equal_prefix count left right} ->
    {u : unit | P.equal_prefix total before after} @ ghost = fun before after value left right count total premise -> ghost_ (
    Codec.decode_def before; Codec.decode_def after;
    match Codec.decode_limb before, Codec.decode_limb after with
    | Some (_, b), Some (_, a) ->
      limb b a value.W.hi left right count (total - 4) ();
      limb before after value.W.lo b a (total - 4) total ()
    | _ -> ())
let (limb_unique @ total) : (left : B.bytes) @ immutable -> (right : B.bytes) @ immutable -> (value : W.limb) ->
    (tail : B.bytes) @ immutable ->
    {u : unit | Codec.decode_limb left === Some (value, tail) && Codec.decode_limb right === Some (value, tail)} ->
    {u : unit | left === right} @ ghost = fun left right value tail premise -> ghost_ (
    Codec.decode_limb_def left; Codec.decode_limb_def right)
let (unique @ total) : (left : B.bytes) @ immutable -> (right : B.bytes) @ immutable -> (value : W.t) @ immutable ->
    (tail : B.bytes) @ immutable ->
    {u : unit | Codec.decode left === Some (value, tail) && Codec.decode right === Some (value, tail)} ->
    {u : unit | left === right} @ ghost = fun left right value tail premise -> ghost_ (
    Codec.decode_def left; Codec.decode_def right;
    match Codec.decode_limb left, Codec.decode_limb right with
    | Some (_, a), Some (_, b) -> limb_unique a b value.W.hi tail (); limb_unique left right value.W.lo a ()
    | _ -> ())
