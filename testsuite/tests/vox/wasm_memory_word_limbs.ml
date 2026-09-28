module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module V = Hmc_tagged_cell
module S = Wasm_scalar
module M = Wasm_memory
module L = Hmc_linear_bytes
module Codec = Wasm_word_memory
type offset = {n : B.u32 | n <= 4294967291}
let[@def] (high_offset @ total) (offset : offset) : B.u32 = offset + 4
let (correct @ total) : (memory : B.bytes) @ immutable -> (base : B.u32) -> (offset : offset) ->
    (word : W.t) @ immutable ->
    {u : unit | base + offset <= 4294967288 && offset <= 4294967291
      && M.load memory base offset M.W64 === Some (S.I64 word)} ->
    {u : unit | M.load memory base offset M.W32 === Some (S.I32 word.W.lo)
      && M.load memory base (high_offset offset) M.W32 === Some (S.I32 word.W.hi)} @ ghost =
  fun memory base offset word premise -> ghost_ (
    high_offset_def offset;
    M.load_def memory base offset M.W64; M.address_def base offset M.W64; M.size_def M.W64;
    M.count_def M.W64; V.eight_def D.Z; V.four_def D.Z; V.four_def (V.four D.Z);
    M.load_def memory base offset M.W32; M.address_def base offset M.W32; M.size_def M.W32; M.count_def M.W32;
    M.load_def memory base (offset + 4) M.W32; M.address_def base (offset + 4) M.W32;
    L.load_def memory (base + offset) (V.eight D.Z);
    L.load_def memory (base + offset) (V.four D.Z);
    L.load_def memory (base + offset + 4) (V.four D.Z);
    match L.drop memory (base + offset) with
    | None -> ()
    | Some b0 ->
      L.take_def (D.S (D.S (D.S (D.S (D.S (D.S (D.S (D.S (D.Z))))))))) b0;
      (match b0 with B.End -> () | B.Byte (a0, b1) ->
      L.take_def (D.S (D.S (D.S (D.S (D.S (D.S (D.S (D.Z)))))))) b1;
      (match b1 with B.End -> () | B.Byte (a1, b2) ->
      L.take_def (D.S (D.S (D.S (D.S (D.S (D.S (D.Z))))))) b2;
      (match b2 with B.End -> () | B.Byte (a2, b3) ->
      L.take_def (D.S (D.S (D.S (D.S (D.S (D.Z)))))) b3;
      (match b3 with B.End -> () | B.Byte (a3, b4) ->
      L.take_def (D.S (D.S (D.S (D.S (D.Z))))) b4;
      (match b4 with B.End -> () | B.Byte (a4, b5) ->
      L.take_def (D.S (D.S (D.S (D.Z)))) b5;
      (match b5 with B.End -> () | B.Byte (a5, b6) ->
      L.take_def (D.S (D.S (D.Z))) b6;
      (match b6 with B.End -> () | B.Byte (a6, b7) ->
      L.take_def (D.S (D.Z)) b7;
      (match b7 with B.End -> () | B.Byte (a7, b8) ->
      L.take_def D.Z b8;
      let payload = B.Byte (a0, B.Byte (a1, B.Byte (a2, B.Byte (a3, B.Byte (a4, B.Byte (a5, B.Byte (a6, B.Byte (a7, B.End)))))))) in
      let high = B.Byte (a4, B.Byte (a5, B.Byte (a6, B.Byte (a7, B.End)))) in
      M.decode_def M.W64 payload; Codec.decode_def payload; Codec.decode_limb_def payload; Codec.decode_limb_def high;
      Wasm_cell.shift memory (base + offset) 4 (base + offset + 4) b0 ();
      L.drop_def b0 4;
      L.drop_def b1 3;
      L.drop_def b2 2;
      L.drop_def b3 1;
      L.drop_def b4 0;
      L.take_def (D.S (D.S (D.S (D.S (D.Z))))) b0;
      L.take_def (D.S (D.S (D.S (D.Z)))) b1;
      L.take_def (D.S (D.S (D.Z))) b2;
      L.take_def (D.S (D.Z)) b3;
      L.take_def D.Z b4;
      L.take_def (D.S (D.S (D.S (D.S (D.Z))))) b4;
      L.take_def (D.S (D.S (D.S (D.Z)))) b5;
      L.take_def (D.S (D.S (D.Z))) b6;
      L.take_def (D.S (D.Z)) b7;
      L.take_def D.Z b8;
      let low = B.Byte (a0, B.Byte (a1, B.Byte (a2, B.Byte (a3, B.End)))) in
      M.decode_def M.W32 low; Codec.decode_limb_def low;
      M.decode_def M.W32 high; Codec.decode_limb_def high
      )))))))))
