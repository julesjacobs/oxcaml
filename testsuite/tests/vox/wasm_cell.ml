module D = Hm_declarative
module B = Wasm_u32
module W = Hmc_word64
module C = Hmc_tagged_cell
module L = Hmc_linear_bytes
module Codec = Wasm_word_memory
module Prefix = Hmc_memory_prefix
module M = Wasm_memory
module S = Wasm_scalar

let rec (shift @ total) : (memory : B.bytes) @ immutable -> (base : B.u32) ->
    (offset : B.u32) -> (effective : B.u32) -> (suffix : B.bytes) @ immutable ->
    {u : unit | base + offset = effective && L.drop memory base === Some suffix} ->
    {u : unit | L.drop memory effective === L.drop suffix offset} @ ghost =
  fun memory base offset effective suffix premise -> ghost_ (
    L.drop_def memory base; L.drop_def memory effective;
    if base = 0 then () else match memory with
    | B.End -> ()
    | B.Byte (_, rest) -> shift rest (base - 1) offset (effective - 1) suffix ())

let[@def] (eight @ total) (unit : unit) : B.u32 = 8
let (word_suffix @ total) : (bytes : B.bytes) @ immutable -> (word : W.t) @ immutable ->
    (tail : B.bytes) @ immutable ->
    {u : unit | Codec.decode bytes === Some (word, tail)} ->
    {u : unit | L.drop bytes (eight ()) === Some tail} @ ghost =
  fun bytes word tail premise -> ghost_ (
    Codec.decode_def bytes; Codec.decode_limb_def bytes; M.size_def M.W64; eight_def ();
    (match Codec.decode_limb bytes with None -> () | Some (_, middle) -> Codec.decode_limb_def middle);
    match bytes with
    | (B.Byte (_, (B.Byte (_, (B.Byte (_, (B.Byte (_, (B.Byte (_, (B.Byte (_, (B.Byte (_, (B.Byte (_, tail) as b7)) as b6)) as b5)) as b4)) as b3)) as b2)) as b1)) as b0) ->
      Codec.decode_limb_def b4;
      L.drop_def b0 8; L.drop_def b1 7; L.drop_def b2 6; L.drop_def b3 5; L.drop_def b4 4; L.drop_def b5 3; L.drop_def b6 2; L.drop_def b7 1; L.drop_def tail 0
    | _ -> ())

let[@def] (tag @ total) (memory : B.bytes @ immutable) (base : B.u32) = M.load memory base 0 M.W64
let[@def] (payload @ total) (memory : B.bytes @ immutable) (base : B.u32) = M.load memory base 8 M.W64
let (cell @ total) : (memory : B.bytes) @ immutable -> (base : B.u32) ->
    (bytes : B.bytes) @ immutable -> (value : C.value) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | base <= 4294967280 && L.drop memory base === Some bytes
      && C.decode bytes === Some (value, tail)} ->
    {u : unit | tag memory base === Some (S.I64 (C.tag value))
      && payload memory base === Some (S.I64 (C.payload value))} @ ghost =
  fun memory base bytes value tail premise -> ghost_ (
    C.decode_def bytes; C.tag_def value; C.payload_def value;
    tag_def memory base; payload_def memory base;
    M.load_def memory base 0 M.W64; M.load_def memory base 8 M.W64;
    M.address_def base 0 M.W64; M.address_def base 8 M.W64;
    M.size_def M.W64; M.count_def M.W64;
    L.load_def memory base (C.eight D.Z); L.load_def memory (base + 8) (C.eight D.Z);
    match Codec.decode bytes with
    | None -> ()
    | Some (tag, middle) ->
      L.take_def D.Z middle;
      let tag_bytes = Prefix.word bytes D.Z tag middle B.End () in
      M.decode_def M.W64 tag_bytes;
      word_suffix bytes tag middle (); eight_def (); shift memory base 8 (base + 8) bytes ();
      match Codec.decode middle with
      | None -> ()
      | Some (payload, rest) ->
        L.take_def D.Z rest;
        let payload_bytes = Prefix.word middle D.Z payload rest B.End () in
        M.decode_def M.W64 payload_bytes;
        (* Expose limb bounds for Boolean payload canonicalization. *)
        let _lo : W.limb = payload.W.lo in
        let _hi : W.limb = payload.W.hi in
        ())
