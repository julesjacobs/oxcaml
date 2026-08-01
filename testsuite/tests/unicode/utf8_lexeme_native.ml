(* TEST
 include ocamlcommon;
 bytecode;
 native;
*)

(* Companion to utf8_lexeme.ml, which is an [expect] test and so only ever runs
   the bytecode toplevel: [expect.opt] cannot be used there, because the native
   toplevel already contains the ocamlcommon modules and loading
   ocamlcommon.cmxa into it is a duplicate-module error.

   The safety of [Misc.Utf8_lexeme.get_known_pair] is a property of generated
   code rather than of the source: it scrutinises [Uchar.unsafe_to_char c1],
   which is [%identity], so a base character above U+00FF yields a "char"
   holding a value above 255. That misses every branch only because the case
   values span ['A','z'], a proper subrange of the char domain, which forces an
   unsigned range check ahead of the jump table. This test therefore exercises
   those inputs through a natively compiled path as well as a bytecode one.

   Inputs are written with escapes so this file stays pure ASCII. *)

let hex s =
  String.concat ""
    (List.init (String.length s)
       (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let show = function
  | Ok s -> "Ok " ^ hex s
  | Error s -> "Error " ^ hex s

let check name f input =
  Printf.printf "%-32s %s\n" name (show (f input))

let () =
  (* Base characters outside the ASCII range whose low byte collides with a
     pair base: U+0141 with 'A' (0x41) and U+0161 with 'a' (0x61). A truncating
     conversion would compose these into U+00C0 and U+00E1. *)
  check "normalize U+0141 U+0300"
    Misc.Utf8_lexeme.normalize "\u{0141}\u{0300}";
  check "normalize U+0161 U+0301"
    Misc.Utf8_lexeme.normalize "\u{0161}\u{0301}";
  check "uncapitalize U+0141 U+0300"
    Misc.Utf8_lexeme.uncapitalize "\u{0141}\u{0300}";

  (* Bases far outside the char domain: these probe the range check rather
     than truncation, since their low bytes are not pair bases. *)
  check "normalize U+0100 U+0300"
    Misc.Utf8_lexeme.normalize "\u{0100}\u{0300}";
  check "normalize U+898B U+0300"
    Misc.Utf8_lexeme.normalize "\u{898B}\u{0300}";
  check "normalize U+10FFFF U+0300"
    Misc.Utf8_lexeme.normalize "\u{10FFFF}\u{0300}";

  (* Compositions that must still fire, at both ends of the ['A','z'] span. *)
  check "normalize A U+0300" Misc.Utf8_lexeme.normalize "A\u{0300}";
  check "normalize z U+030C" Misc.Utf8_lexeme.normalize "z\u{030C}";
  check "normalize helloA U+0300"
    Misc.Utf8_lexeme.normalize "helloA\u{0300}";

  (* First-character mapping combined with a later composition. *)
  check "uncapitalize U+0160 eE U+0301"
    Misc.Utf8_lexeme.uncapitalize "\u{0160}eE\u{0301}";

  (* The [case] constructors now carry an int; check both round trips. *)
  check "uncapitalize U+1E9E x" Misc.Utf8_lexeme.uncapitalize "\u{1E9E}x";
  check "capitalize U+00FF x" Misc.Utf8_lexeme.capitalize "\u{00FF}x";

  (* ASCII fast paths and the invalid-UTF-8 error path. *)
  check "uncapitalize Hello" Misc.Utf8_lexeme.uncapitalize "Hello";
  check "capitalize hello" Misc.Utf8_lexeme.capitalize "hello";
  check "uncapitalize Hello backslash-255"
    Misc.Utf8_lexeme.uncapitalize "Hello\255";
  check "normalize U+FFFD" Misc.Utf8_lexeme.normalize "\u{FFFD}"
