(* TEST
 has-z3;
 flags = "-extension refinement_types";
 all_modules = "wasm_u32.ml wasm_framing.ml wasm_section.ml wasm_module.ml wasm_module_demo.ml";
 { bytecode; }
*)
module B = Wasm_u32
module F = Wasm_framing
module S = Wasm_section
module M = Wasm_module
let rec raw = function
  | [] -> B.End
  | b :: rest -> if 0 <= b && b < 256 then B.Byte (b, raw rest)
    else failwith "fixture byte"
let require = function Some value -> value | None -> failwith "encoding rejected"
let rec truncate = function
  | B.End -> B.End
  | B.Byte (_, B.End) -> B.End
  | B.Byte (b, rest) -> B.Byte (b, truncate rest)
let () =
  let tail = raw [255; 19] in
  List.iter (fun payload ->
    let bytes = require (F.encode payload tail) in
    if F.decode bytes <> Some (payload, tail) then failwith "payload suffix";
    let section = {S.id = 10; payload} in
    if S.decode (require (S.encode section tail)) <> Some (section, tail) then
      failwith "section suffix") [B.End; raw [0]; raw [255; 0; 128]];
  if F.decode (raw [3; 1; 2]) <> None then failwith "short payload";
  if F.decode (raw [128]) <> None then failwith "short length";
  if F.decode (raw [255; 255; 255; 255; 16]) <> None then failwith "overflowing length";
  if S.decode (raw [12; 0]) <> None then failwith "unknown section id";
  if S.decode (raw [1]) <> None then failwith "missing section length";
  let empty = raw [0] in
  let module_ = {M.types = raw [1; 96; 0; 1; 127]; functions = raw [1; 0];
    table = empty; memory = empty; globals = empty;
    exports = raw [1; 3; 114; 117; 110; 0; 0]; elements = empty;
    code = raw [1; 4; 0; 65; 42; 11]; data = empty} in
  let bytes = require (M.encode module_ tail) in
  if M.decode bytes <> Some (module_, tail) then failwith "module suffix";
  if M.decode (truncate (require (M.encode module_ B.End))) <> None then failwith "truncated module";
  if M.decode (raw [0; 97; 115; 109; 2; 0; 0; 0]) <> None then failwith "wrong version";
  if M.decode (raw [0; 97; 115; 109; 1; 0; 0; 0; 3; 0]) <> None then failwith "wrong section order";
  if M.decode (raw [0; 97; 115; 109; 1; 0; 0; 0; 1; 0; 1; 0]) <> None then failwith "duplicate section";
  print_endline "Wasm framing and module envelope: exact payloads and suffixes, truncation, lengths, section order, and version passed"
