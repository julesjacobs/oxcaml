module B = Wasm_u32
module W = Hmc_word64
module L = Hmc_linear_bytes
module P = Hmc_linear_preservation
module Codec = Wasm_word_memory
module Q = Wasm_word_sequence
type width = {n : B.u32 | n >= 8}
let[@def] (remaining @ total) (count : width) : B.u32 = count - 8
let (word @ total) : (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (value : W.t) @ immutable -> (tail : B.bytes) @ immutable -> (count : width) ->
    {u : unit | Codec.decode before === Some (value, tail) && P.equal_prefix count before after} ->
    {out : B.bytes | Codec.decode after === Some (value, out) && P.equal_prefix (remaining count) tail out} @ immutable =
  fun before after value tail count premise ->
    ghost_ (remaining_def count);
    ghost_ (P.equal_prefix_def (count - 0) before after);
    (match before, after with
    | B.Byte (_, b1), B.Byte (_, a1) ->
    ghost_ (P.equal_prefix_def (count - 1) b1 a1);
    (match b1, a1 with
    | B.Byte (_, b2), B.Byte (_, a2) ->
    ghost_ (P.equal_prefix_def (count - 2) b2 a2);
    (match b2, a2 with
    | B.Byte (_, b3), B.Byte (_, a3) ->
    ghost_ (P.equal_prefix_def (count - 3) b3 a3);
    (match b3, a3 with
    | B.Byte (_, b4), B.Byte (_, a4) ->
    ghost_ (P.equal_prefix_def (count - 4) b4 a4);
    (match b4, a4 with
    | B.Byte (_, b5), B.Byte (_, a5) ->
    ghost_ (P.equal_prefix_def (count - 5) b5 a5);
    (match b5, a5 with
    | B.Byte (_, b6), B.Byte (_, a6) ->
    ghost_ (P.equal_prefix_def (count - 6) b6 a6);
    (match b6, a6 with
    | B.Byte (_, b7), B.Byte (_, a7) ->
    ghost_ (P.equal_prefix_def (count - 7) b7 a7);
    (match b7, a7 with
    | B.Byte (_, b8), B.Byte (_, a8) ->
    ghost_ (Codec.decode_def before; Codec.decode_def after; Codec.decode_limb_def before; Codec.decode_limb_def after; Codec.decode_limb_def b4; Codec.decode_limb_def a4);
    a8
    | _ -> unreachable_ ())
    | _ -> unreachable_ ())
    | _ -> unreachable_ ())
    | _ -> unreachable_ ())
    | _ -> unreachable_ ())
    | _ -> unreachable_ ())
    | _ -> unreachable_ ())
    | _ -> unreachable_ ())
let rec (sequence @ total) : (words : Q.words) @ immutable -> (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (tail : B.bytes) @ immutable -> (count : B.u32) ->
    {u : unit | Q.decode words before === Some tail && Q.size words count && P.equal_prefix count before after} ->
    {out : B.bytes | Q.decode words after === Some out} @ immutable =
  fun words before after tail count premise ->
    ghost_ (Q.decode_def words before; Q.decode_def words after; Q.size_def words count);
    match words with
    | Q.End -> after
    | Q.Word (value, rest) -> (match Codec.decode before with
      | None -> unreachable_ ()
      | Some (actual, middle) ->
        ghost_ (W.equal_def value actual);
        let after_middle = word before after value middle count () in
        ghost_ (remaining_def count);
        let out = sequence rest middle after_middle tail (count - 8) () in
        ghost_ (W.equal_def value value); out)
