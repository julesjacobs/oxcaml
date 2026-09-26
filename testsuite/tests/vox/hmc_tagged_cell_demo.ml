(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml wasm_u32.ml wasm_word_memory.ml hmc_tagged_cell.ml hmc_tagged_cell_demo.ml";
 { bytecode; }
 { native; }
*)
module D = Hm_declarative
module B = Wasm_u32
module W = Hmc_word64
module M = Wasm_word_memory
module C = Hmc_tagged_cell
let rec integer = function D.Z -> 0 | D.S n -> 1 + integer n
let rec list = function B.End -> [] | B.Byte (b, rest) -> b :: list rest
let rec take n = function B.End -> B.End | B.Byte (b, rest) -> if n = 0 then B.End else B.Byte (b, take (n - 1) rest)
let check : C.value @ immutable -> unit = fun value ->
  let suffix = B.Byte (255, B.Byte (0, B.End)) in
  let bytes = C.encode value suffix in
  if C.decode bytes <> Some (value, suffix) then failwith "tagged cell round trip";
  if integer (C.length bytes) <> 18 then failwith "tagged cell width";
  let bytes = C.encode value B.End in
  for count = 0 to 15 do
    if C.decode (take count bytes) <> None then failwith "accepted truncated cell"
  done
let reject : W.t @ immutable -> W.t @ immutable -> unit = fun tag payload ->
  if C.decode (M.encode tag (M.encode payload B.End)) <> None then failwith "accepted noncanonical cell"
let () =
  List.iter check [C.Boolean false; C.Boolean true; C.Nil;
    C.Word {W.lo = 0; hi = 0}; C.Word {W.lo = 4294967295; hi = 4294967295};
    C.Word {W.lo = 4294967295; hi = 2147483648};
    C.Cons_pointer 0; C.Cons_pointer 4294967295; C.Closure_pointer 0; C.Closure_pointer 4294967295];
  let bytes = C.encode (C.Word {W.lo = 4294967295; hi = 2147483648}) B.End in
  if list bytes <> [1;0;0;0;0;0;0;0;255;255;255;255;0;0;0;128] then failwith "word endianness";
  let bytes = C.encode (C.Closure_pointer 4294967295) B.End in
  if list bytes <> [4;0;0;0;0;0;0;0;255;255;255;255;0;0;0;0] then failwith "pointer endianness";
  reject {W.lo = 5; hi = 0} {W.lo = 0; hi = 0};
  reject {W.lo = 0; hi = 1} {W.lo = 0; hi = 0};
  reject {W.lo = 0; hi = 0} {W.lo = 2; hi = 0};
  reject {W.lo = 0; hi = 0} {W.lo = 1; hi = 1};
  reject {W.lo = 2; hi = 0} {W.lo = 1; hi = 0};
  reject {W.lo = 3; hi = 0} {W.lo = 0; hi = 1};
  reject {W.lo = 4; hi = 0} {W.lo = 0; hi = 1};
  print_endline "16-byte tagged cells preserve words, pointers, suffixes, and canonical tags"
