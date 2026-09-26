(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml wasm_u32.ml wasm_word_memory.ml hmc_tagged_cell.ml hmc_linear_bytes.ml hmc_linear_bytes_demo.ml";
 { bytecode; }
*)
module D = Hm_declarative
module W = Hmc_word64
module B = Wasm_u32
module C = Hmc_tagged_cell
module M = Hmc_linear_bytes
let rec bytes (values : B.byte list) = match values with [] -> B.End | head :: rest -> B.Byte (head, bytes rest)
let rec list : B.bytes -> B.byte list = function B.End -> [] | B.Byte (head, rest) -> head :: list rest
let rec filled n = if n = 0 then B.End else B.Byte (17, filled (n - 1))
let rec drop n values = if n = 0 then values else match values with [] -> [] | _ :: rest -> drop (n - 1) rest
let rec splice address payload memory = if address = 0 then payload @ drop (List.length payload) memory else
  match memory with [] -> failwith "bad reference splice" | head :: rest -> head :: splice (address - 1) payload rest
let () =
  let original : B.byte list = [0; 1; 2; 3; 4; 5; 6; 7] in
  let memory = bytes original in
  let addresses : W.limb list = [0; 1; 2; 4; 7; 8; 9; 4294967295] in
  let payloads : B.byte list list = [[]; [255]; [42; 43]; [9; 9; 9; 9; 9; 9; 9; 9]; [1; 2; 3; 4; 5; 6; 7; 8; 9]] in
  List.iter (fun (address : W.limb) -> List.iter (fun payload ->
    let encoded = bytes payload in
    let expected = if address > List.length original || List.length payload > List.length original - address then None
      else Some (splice address payload original) in
    match M.store memory address encoded, expected with
    | None, None -> ()
    | Some after, Some expected when list after = expected ->
      if M.load after address (C.length encoded) <> Some encoded then failwith "linear store/load roundtrip"
    | _ -> failwith "linear store bounds or untouched bytes") payloads) addresses;
  let word = C.Word {W.lo = 4294967295; hi = 2147483648} in
  let encoded = C.encode word B.End in
  let memory = filled 35 in
  (match M.store memory 3 encoded with
  | None -> failwith "cell store rejected"
  | Some after ->
    (match M.load after 3 (C.length encoded) with
    | Some stored when C.decode stored = Some (word, B.End) -> ()
    | _ -> failwith "stored tagged cell decode"));
  if M.load memory 35 D.Z <> Some B.End || M.load memory 36 D.Z <> None then failwith "empty load endpoint";
  print_endline "bounded flat-byte writes, untouched regions, endpoint cases, and tagged-cell loads passed"
