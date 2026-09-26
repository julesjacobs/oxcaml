(* TEST
 has-z3;
 flags = "-extension refinement_types";
 all_modules = "wasm_u32.ml hmc_word64.ml wasm_word_memory.ml wasm_word_memory_demo.ml";
 { bytecode; }
*)
open Wasm_u32
module W = Hmc_word64
module M = Wasm_word_memory

let check word expected =
  let tail = Byte (165, Byte (90, End)) in
  let encoded = M.encode word tail in
  if encoded <> expected then failwith "wrong little-endian bytes";
  match M.decode encoded with
  | Some (decoded, rest) when W.equal decoded word && rest = tail -> ()
  | _ -> failwith "word or suffix was lost"

let rec zeros n tail = if n = 0 then tail else Byte (0, zeros (n - 1) tail)
let rec truncated n =
  if n < 8 then (
    if M.decode (zeros n End) <> None then failwith "accepted truncated word";
    truncated (n + 1))

let () =
  let tail = Byte (165, Byte (90, End)) in
  check {W.lo = 0; hi = 0} (zeros 8 tail);
  check {W.lo = 2309737967; hi = 19088743}
    (Byte (239, Byte (205, Byte (171, Byte (137,
      Byte (103, Byte (69, Byte (35, Byte (1, tail)))))))));
  check {W.lo = 0; hi = 2147483648}
    (zeros 7 (Byte (128, tail)));
  check {W.lo = 4294967295; hi = 4294967295}
    (Byte (255, Byte (255, Byte (255, Byte (255,
      Byte (255, Byte (255, Byte (255, Byte (255, tail)))))))));
  truncated 0
