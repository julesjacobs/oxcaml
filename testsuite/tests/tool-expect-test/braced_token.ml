(* TEST
 {expect.opt;}
*)

(* The TSL lexer treats { and } as tokens in their own right, so the
   brace-adjacent spelling above is valid and runs the native expect-test
   runner. dev-runners-needed's token grep must see it too (its boundary
   classes include braces for exactly this file's sake); dev-selftest
   checks that. *)

let probe_braced = Sys.backend_type
[%%expect{|
val probe_braced : Sys.backend_type = Sys.Native
|}]
