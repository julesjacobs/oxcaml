module B = Wasm_u32
module W = Hmc_word64
module Q = Wasm_word_sequence
module Codec = Wasm_word_memory
module M = Wasm_memory
module S = Wasm_scalar
let[@def] (zero @ total) (unit : unit) : B.u32 = 0
let (correct @ total) : (prefix : Q.words) @ immutable -> (rest : Q.words) @ immutable ->
    (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    (old : W.t) @ immutable -> (value : W.t) @ immutable -> (offset : B.u32) ->
    {u : unit | Q.size prefix offset && offset <= 4294967288
      && Q.decode (Q.append prefix (Q.Word (old, rest))) before === Some tail
      && Q.decode (Q.append prefix (Q.Word (value, rest))) after === Some tail} ->
    {u : unit | M.store before (zero ()) offset (S.I64 value) === Some after} @ ghost =
  fun prefix rest before after tail old value offset premise -> ghost_ (
    zero_def ();
    let left = Q.split prefix (Q.Word (old, rest)) before tail () in
    let right = Q.split prefix (Q.Word (value, rest)) after tail () in
    Q.prefix prefix before after left right offset ();
    Q.decode_def (Q.Word (old, rest)) left; Q.decode_def (Q.Word (value, rest)) right;
    match Codec.decode left, Codec.decode right with
    | Some (a, b), Some (c, d) ->
      W.equal_def old a; W.equal_def value c; Q.unique rest b d tail ();
      Wasm_word_update.correct before after 0 offset offset left right old value b ()
    | _ -> ())
let (at @ total) : (prefix : Q.words) @ immutable -> (rest : Q.words) @ immutable ->
    (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (before_frame : B.bytes) @ immutable -> (after_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    (old : W.t) @ immutable -> (value : W.t) @ immutable -> (base : B.u32) -> (offset : B.u32) -> (address : B.u32) ->
    {u : unit | Q.size prefix offset && address = base + offset && address <= 4294967288
      && Hmc_linear_preservation.equal_prefix base before after
      && Hmc_linear_bytes.drop before base === Some before_frame && Hmc_linear_bytes.drop after base === Some after_frame
      && Q.decode (Q.append prefix (Q.Word (old, rest))) before_frame === Some tail
      && Q.decode (Q.append prefix (Q.Word (value, rest))) after_frame === Some tail} ->
    {u : unit | M.store before base offset (S.I64 value) === Some after} @ ghost =
  fun prefix rest before after before_frame after_frame tail old value base offset address premise -> ghost_ (
    let left = Q.split prefix (Q.Word (old, rest)) before_frame tail () in
    let right = Q.split prefix (Q.Word (value, rest)) after_frame tail () in
    Q.prefix prefix before_frame after_frame left right offset ();
    Wasm_memory_splice.concat before after base offset address before_frame after_frame ();
    Wasm_cell.shift before base offset address before_frame (); Wasm_cell.shift after base offset address after_frame ();
    Q.decode_def (Q.Word (old, rest)) left; Q.decode_def (Q.Word (value, rest)) right;
    match Codec.decode left, Codec.decode right with
    | Some (a, b), Some (c, d) ->
      W.equal_def old a; W.equal_def value c; Q.unique rest b d tail ();
      Wasm_word_update.correct before after base offset address left right old value b ()
    | _ -> ())
