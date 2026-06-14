(* TEST
 macos;
 arch_arm64;
 flags += " -O3 -llvm-backend";
 native;
*)

let check_string a b expected =
  assert (String.equal a b = expected);
  assert (String.equal b a = expected)

let check_bytes a b expected =
  assert (Bytes.equal a b = expected);
  assert (Bytes.equal b a = expected)

let () =
  check_string "" "" true;
  check_string "" "x" false;
  check_string "abc" "abc" true;
  check_string "abc" "abd" false;
  check_string "abc" "abcd" false;
  check_string "abc\000def" "abc\000def" true;
  check_string "abc\000def" "abc\000deg" false;
  check_string "\255\254\253" "\255\254\253" true;
  check_string "\255\254\253" "\255\254\252" false;
  check_string "123456789012345" "123456789012345" true;
  check_string "123456789012345" "123456789012346" false;
  check_string "1234567890123456" "1234567890123456" true;
  check_string "1234567890123456" "1234567890123457" false;
  let b1 = Bytes.of_string "abc\000def" in
  let b2 = Bytes.of_string "abc\000def" in
  let b3 = Bytes.of_string "abc\000deg" in
  check_bytes b1 b2 true;
  check_bytes b1 b3 false;
  Bytes.set b2 3 'x';
  check_bytes b1 b2 false
