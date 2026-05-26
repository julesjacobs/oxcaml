(* TEST
 macos;
 arch_arm64;
 flags += " -O3 -llvm-backend";
 native;
*)

let opaque_string s = Sys.opaque_identity s
let opaque_bytes b = Sys.opaque_identity b

let string_of_codes codes =
  let bytes = Bytes.create (List.length codes) in
  List.iteri (fun i code -> Bytes.set bytes i (Char.chr code)) codes;
  opaque_string (Bytes.unsafe_to_string bytes)

let string_copy s = opaque_string (Bytes.unsafe_to_string (Bytes.of_string s))

let reference_compare a b =
  let len_a = String.length a in
  let len_b = String.length b in
  let min_len = min len_a len_b in
  let rec loop i =
    if i = min_len
    then compare len_a len_b
    else
      let ca = Char.code (String.get a i) in
      let cb = Char.code (String.get b i) in
      if ca = cb then loop (i + 1) else compare ca cb
  in
  loop 0

let sign n = if n < 0 then -1 else if n > 0 then 1 else 0

let check_pair a b =
  let a = opaque_string a in
  let b = opaque_string b in
  let expected = sign (reference_compare a b) in
  let string_actual = sign (String.compare a b) in
  if string_actual <> expected
  then
    Printf.ksprintf failwith "String.compare mismatch: %S %S -> %d expected %d"
      a b string_actual expected;
  let bytes_actual =
    sign (Bytes.compare (opaque_bytes (Bytes.of_string a))
            (opaque_bytes (Bytes.of_string b)))
  in
  if bytes_actual <> expected
  then
    Printf.ksprintf failwith "Bytes.compare mismatch: %S %S -> %d expected %d"
      a b bytes_actual expected

let rec all_strings alphabet len =
  if len = 0
  then [string_of_codes []]
  else
    List.concat_map
      (fun tail ->
        List.map
          (fun code ->
            let bytes = Bytes.create len in
            Bytes.set bytes 0 (Char.chr code);
            Bytes.blit_string tail 0 bytes 1 (len - 1);
            opaque_string (Bytes.unsafe_to_string bytes))
          alphabet)
      (all_strings alphabet (len - 1))

let with_byte len pos code =
  let bytes = Bytes.make len 'a' in
  if pos < len then Bytes.set bytes pos (Char.chr code);
  opaque_string (Bytes.unsafe_to_string bytes)

let () =
  let tiny =
    List.concat_map (all_strings [0; 1; 127; 128; 255]) [0; 1; 2; 3; 4]
  in
  List.iter (fun a -> List.iter (fun b -> check_pair a b) tiny) tiny;
  let boundary_lengths = [7; 8; 9; 15; 16; 17; 31; 32; 33] in
  let diff_positions = [0; 7; 8; 14; 15; 16; 30; 31; 32] in
  List.iter
    (fun len ->
      let equal = with_byte len 0 (Char.code 'a') in
      check_pair equal (string_copy equal);
      List.iter
        (fun pos ->
          if pos < len
          then (
            check_pair (with_byte len pos 0) (with_byte len pos 255);
            check_pair (with_byte len pos 255) (with_byte len pos 0)))
        diff_positions)
    boundary_lengths;
  check_pair "" "\000";
  check_pair "\000" "";
  check_pair "abc" "abc\000";
  check_pair "abc\000" "abc";
  check_pair "\255" "\000";
  check_pair "\128" "\127";
  print_endline "OK"
