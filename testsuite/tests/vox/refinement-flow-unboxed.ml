(* TEST
 flags = "-extension layouts_beta -drefinements";
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
Line 1, characters 22-23: refinement obligation: int{ _ > 0 }
val pr : product = #{refined = 5; plain = 1}
|}]

let use_pr (p : product) = p.#refined + 1;;
[%%expect{|
val use_pr : product -> int = <fun>
|}]

(* unboxed boolean literal pattern against a refined expectation: the
   pattern recurses at the payload *)
let (#true : (bool#){ true }) = #true;;
[%%expect{|
Line 1, characters 32-37: refinement obligation: bool#{ true }
|}]

(* unboxed unit pattern likewise *)
let (#() : (unit#){ true }) = #();;
[%%expect{|
Line 1, characters 30-33: refinement obligation: unit#{ true }
|}]
