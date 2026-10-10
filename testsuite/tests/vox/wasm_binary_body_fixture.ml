module B = Wasm_u32
module F = Wasm_functions
module C = Wasm_code
module I = Wasm_instruction
module T = Wasm_control
module Body = Wasm_function_body
let rec bytes = function [] -> B.End | n :: rest -> if n < 0 || n > 255 then failwith "fixture byte" else B.Byte (n, bytes rest)
let check locals code =
  if not (Wasm_nesting.structured code) then failwith "fixture structure" else
  let body = {Body.locals; code} in
  let tail = bytes [42; 0; 255] in
  match Body.encode body tail () with
  | None -> failwith "body fixture encoding"
  | Some encoded -> if Body.decode encoded <> Some (body, tail) then failwith "body fixture roundtrip"
let rejected payload = if Body.decode_payload (bytes payload) <> None then failwith "malformed body accepted"
let fixtures () =
  let module Sections = Wasm_function_sections in
  let module Code = Wasm_code_section in
  let body = {Body.locals = F.Local64 F.No_locals; code = T.Instruction (I.I64_const {Hmc_word64.lo = 42; hi = 0}, T.Empty)} in
  (match Body.payload {Body.locals = F.No_locals; code = T.Empty} with
  | Some payload when Wasm_framing.size payload = Some 6 -> ()
  | _ -> failwith "empty body canonical size");
  (match Body.payload body with
  | Some payload when Wasm_framing.size payload = Some 19 -> ()
  | _ -> failwith "word body canonical size");
  let bodies = Code.Body (body, Code.Body ({Body.locals = F.No_locals; code = T.Empty}, Code.Empty)) in
  if not (Code.structured bodies) then failwith "body structure" else
  (match Code.encode bodies (bytes [42; 255]) () with
  | None -> failwith "code vector fixture encoding"
  | Some encoded -> if Code.decode encoded <> Some (bodies, bytes [42; 255]) then failwith "code vector roundtrip");
  List.iter (fun input -> if Code.decode (bytes input) <> None then failwith "invalid code vector accepted")
    [[]; [1]; [1; 0]; [1; 1; 0]; [2; 2; 0; 11]; [1; 3; 0; 11; 11]];
  let signatures = F.Signature (F.I64, F.Signature (F.Void, F.Signature (F.I32, F.No_signatures))) in
  let functions = F.Function ({F.result = F.I64; locals = body.Body.locals; code = body.Body.code},
    F.Function ({F.result = F.Void; locals = F.No_locals; code = T.Empty}, F.No_functions)) in
  (match Sections.encode signatures functions with None -> failwith "function sections encoding"
  | Some encoded -> if Sections.decode signatures encoded <> Some functions then failwith "function signature linking");
  if Sections.encode F.No_signatures functions <> None then failwith "missing signature accepted";
  if Sections.link signatures (F.Element (Some 3, F.No_elements)) (Code.Body (body, Code.Empty)) <> None then failwith "out of range signature accepted";
  if Sections.link signatures F.No_elements bodies <> None then failwith "missing function indices accepted";
  if Sections.link signatures (F.Element (Some 0, F.No_elements)) Code.Empty <> None then failwith "missing function bodies accepted";
  List.iter (fun table ->
    let suffix = bytes [42; 255] in
    (match Wasm_index_vector.encode table suffix with None -> failwith "index fixture encoding"
    | Some encoded -> if Wasm_index_vector.decode encoded <> Some (table, suffix) then failwith "index vector roundtrip");
    match Wasm_index_vector.encode_element table suffix with None -> failwith "element fixture encoding"
    | Some encoded -> if Wasm_index_vector.decode_element encoded <> Some (table, suffix) then failwith "element section roundtrip")
    [F.No_elements; F.Element (Some 0, F.Element (Some 128, F.Element (Some 4294967295, F.No_elements)))];
  if Wasm_index_vector.encode (F.Element (None, F.No_elements)) B.End <> None then failwith "null index accepted";
  List.iter (fun input -> if Wasm_index_vector.decode (bytes input) <> None then failwith "invalid index vector accepted")
    [[]; [1]; [1; 128]; [2; 0]; [1; 128; 128; 128; 128; 16]];
  List.iter (fun input -> if Wasm_index_vector.decode_element (bytes input) <> None then failwith "invalid element section accepted")
    [[]; [0]; [2; 0]; [1; 1]; [1; 0; 11]; [1; 0; 65; 128; 128; 128; 128; 0; 11; 1]];
  let module G = Wasm_globals in
  let module S = Wasm_scalar in
  let module Globals = Wasm_global_section in
  List.iter (fun globals ->
    let suffix = bytes [42; 255] in
    match Globals.encode globals suffix with
    | None -> failwith "global section fixture encoding"
    | Some encoded -> if Globals.decode encoded <> Some (globals, suffix) then failwith "global section fixture roundtrip")
    [{G.values = S.Empty; permissions = G.Empty};
     {G.values = S.Push (S.I32 4294967295, S.Push (S.I64 {Hmc_word64.lo = 0; hi = 2147483648}, S.Empty));
      permissions = G.Global (false, G.Global (true, G.Empty))}];
  if Globals.encode {G.values = S.Empty; permissions = G.Global (true, G.Empty)} B.End <> None
    then failwith "mismatched global permissions accepted";
  List.iter (fun input -> if Globals.decode (bytes input) <> None then failwith "invalid global vector accepted")
    [[]; [1]; [1; 127; 2]; [1; 125; 0]; [1; 127; 0; 65]; [1; 127; 0; 65; 128; 128; 128; 128; 0];
     [2; 127; 0; 65; 128; 128; 128; 128; 0; 11]];
  let exports = {Wasm_export_section.run = 4294967295; memory = 0; tag = 128; payload = 7} in
  let suffix = bytes [42; 255] in
  if Wasm_export_section.decode (Wasm_export_section.encode exports suffix) <> Some (exports, suffix)
    then failwith "export section roundtrip";
  List.iter (fun input -> if Wasm_export_section.decode (bytes input) <> None then failwith "invalid export section accepted")
    [[]; [3]; [4; 3; 114; 117; 110; 1; 0]; [4; 3; 114; 117; 110; 0]];
  List.iter (fun data ->
    let suffix = bytes [42; 255] in
    match Wasm_data_section.encode (bytes data) suffix with
    | None -> failwith "data section fixture encoding"
    | Some encoded -> if Wasm_data_section.decode encoded <> Some (bytes data, suffix) then failwith "data section fixture roundtrip")
    [[]; [0; 255; 127; 128; 0]];
  List.iter (fun input -> if Wasm_data_section.decode (bytes input) <> None then failwith "invalid data section accepted")
    [[0]; [1; 1]; [2; 0]; [1; 0; 11]; [1; 0; 65; 128; 128; 128; 128; 0; 11; 2; 42]];
  let limits = {Wasm_limits_section.minimum = 0; maximum = 65536} in
  let suffix = bytes [42; 255] in
  if Wasm_limits_section.decode_memory (Wasm_limits_section.encode_memory limits suffix ()) <> Some (limits, suffix)
    then failwith "memory limits roundtrip";
  if Wasm_limits_section.decode_table (Wasm_limits_section.encode_table limits suffix ()) <> Some (limits, suffix)
    then failwith "table limits roundtrip";
  List.iter (fun input -> if Wasm_limits_section.decode_memory (bytes input) <> None then failwith "invalid memory limits accepted")
    [[1; 1; 2; 1]; [1; 1; 0; 129; 128; 4]; [2; 1; 0; 1]; [1; 1; 0]];
  if Wasm_limits_section.decode_table (bytes [1; 111; 1; 0; 1]) <> None then failwith "invalid table type accepted";
  let signatures = F.Signature (F.Void, F.Signature (F.I32, F.Signature (F.I64, F.No_signatures))) in
  let suffix = bytes [42; 255] in
  (match Wasm_signature_section.encode signatures suffix with
  | None -> failwith "signature fixture encoding"
  | Some encoded -> if Wasm_signature_section.decode encoded <> Some (signatures, suffix) then failwith "signature fixture roundtrip");
  List.iter (fun input -> if Wasm_signature_section.decode (bytes input) <> None then failwith "invalid signature accepted")
    [[1; 96; 1; 127; 0]; [1; 96; 0; 2; 127; 127]; [1; 96; 0; 1; 125]; [2; 96; 0; 0]];
  check F.No_locals T.Empty;
  check (F.Local32 (F.Local64 (F.Local32 F.No_locals)))
    (T.Block (T.Loop (T.If (T.Instruction (I.Br 1, T.Empty), T.Instruction (I.I32_const 4294967295, T.Empty), T.Empty), T.Empty),
      T.Instruction (I.I64_const {Hmc_word64.lo = 4294967295; hi = 4294967295}, T.Empty)));
  check (F.Local64 F.No_locals)
    (T.Instruction (I.I64_load (3, 4294967295), T.Instruction (I.Call_indirect 127, T.Empty)));
  List.iter rejected [[0]; [0; 11; 11]; [0; 1]; [0; 255; 11]; [0; 2; 64; 11];
    [1; 1; 125; 11]; [1; 1]; [1; 2; 127; 11]; [1; 0; 127; 11];
    [0; 32; 128; 11]; [0; 4; 64; 5; 5; 11; 11]];
  if Wasm_local_declarations.decode (bytes [1; 1; 127; 42]) <> Some (F.Local32 F.No_locals, bytes [42])
    then failwith "local declaration suffix";
  let tokens = C.Next (I.Local_get 128, C.Next (I.I64_const {Hmc_word64.lo = 0; hi = 2147483648}, C.Empty)) in
  if Wasm_instruction_stream.decode (Wasm_instruction_stream.encode tokens) <> Some tokens
    then failwith "instruction stream fixture"
