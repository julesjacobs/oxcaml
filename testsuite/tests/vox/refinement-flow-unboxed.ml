(* TEST
 flags = "-extension layouts_beta";
 expect;
*)

(* Vox refinement flow, unboxed companion: refinements over unboxed record
   fields and unboxed literal patterns.  RED pins the current rejections;
   GREEN flips the introductions and expectations like the boxed forms. *)

(* an unboxed record with one refined field: construction is a field
   obligation, the [.#] read strips *)
type product = #{ refined : int{ _ > 0 }; plain : int };;
[%%expect{|
type product = #{ refined : int{ _ > 0 }; plain : int; }
|}]

let pr = #{ refined = 5; plain = 1 };;
[%%expect{|
Line 1, characters 22-23:
1 | let pr = #{ refined = 5; plain = 1 };;
                          ^
Error: The constant "5" has type "int" but an expression was expected of type
         "int{ _ > 0 }"
|}]

let use_pr (p : product) = p.#refined + 1;;
[%%expect{|
Line 1, characters 27-37:
1 | let use_pr (p : product) = p.#refined + 1;;
                               ^^^^^^^^^^
Error: This expression has type "int{ _ > 0 }"
       but an expression was expected of type "int"
|}]

(* unboxed boolean literal pattern against a refined expectation: the
   pattern recurses at the payload *)
let (#true : (bool#){ true }) = #true;;
[%%expect{|
Line 1, characters 5-10:
1 | let (#true : (bool#){ true }) = #true;;
         ^^^^^
Error: This pattern matches values of type "bool#"
       but a pattern was expected which matches values of type "bool#{ true }"
|}]

(* unboxed unit pattern likewise *)
let (#() : (unit#){ true }) = #();;
[%%expect{|
Line 1, characters 5-8:
1 | let (#() : (unit#){ true }) = #();;
         ^^^
Error: This pattern matches values of type "unit#"
       but a pattern was expected which matches values of type "unit#{ true }"
|}]
