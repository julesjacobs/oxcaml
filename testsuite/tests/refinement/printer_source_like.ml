(* TEST
 expect;
*)

(* Regression: refinement predicates must print in source-like syntax
   ([_ > 0]), not the raw predicate AST ([(app[Stdlib!.>] _ 0)]).  Covers the
   printer used by [-i], type-at-cursor and error messages (via [out_type]).
   The raw AST syntax is retained only for the debug [text] field of the VC
   dump, which is pinned separately by [rexp_operations.ml]. *)

type pos = int{ _ > 0 }
[%%expect {|
type pos = int{ _ > 0 }
|}]

type cmp = int{ _ >= 0 }
[%%expect {|
type cmp = int{ _ >= 0 }
|}]

(* Boolean connectives, precedence, and negation. *)
type conj = int{ _ >= 0 && _ < 10 }
[%%expect {|
type conj = int{ _ >= 0 && _ < 10 }
|}]

type disj = int{ _ = 0 || _ = 1 }
[%%expect {|
type disj = int{ _ = 0 || _ = 1 }
|}]

type neg = int{ not (_ = 0) }
[%%expect {|
type neg = int{ not (_ = 0) }
|}]

(* Arithmetic precedence: [*] binds tighter than [+] binds tighter than [=]. *)
type arith = int{ (_ + 1) * 2 = 6 }
[%%expect {|
type arith = int{ (_ + 1) * 2 = 6 }
|}]

(* [mod], [/] and the bitwise operators render infix, not prefix. *)
type modulo = int{ _ mod 2 = 0 }
[%%expect {|
type modulo = int{ _ mod 2 = 0 }
|}]

type divide = int{ _ / 3 = 1 }
[%%expect {|
type divide = int{ _ / 3 = 1 }
|}]

type bitand = int{ _ land 1 = 1 }
[%%expect {|
type bitand = int{ _ land 1 = 1 }
|}]

type shift = int{ _ lsl 1 = 4 }
[%%expect {|
type shift = int{ _ lsl 1 = 4 }
|}]

(* If-then-else and boolean literals (as in branch-condition facts). *)
type cond = int{ if _ > 0 then true else false }
[%%expect {|
type cond = int{ if _ > 0 then true else false }
|}]

(* Unit literal [()] must render as [()], not [constructor[unit/7!.()]]. *)
type is_unit = unit{ _ = () }
[%%expect {|
type is_unit = unit{ _ = () }
|}]

(* Record field access renders [_.a], with no type-path stamp. *)
type r = { a : int; b : int }
[%%expect {|
type r = { a : int; b : int; }
|}]

type field_ref = r{ _.a > 0 }
[%%expect {|
type field_ref = r{ _.a > 0 }
|}]

(* A cross-module value reference keeps its module qualifier ([Lib.bound]). *)
module Lib = struct let bound = 0 end
[%%expect {|
module Lib : sig val bound : int end
|}]

type qualified = int{ _ >= Lib.bound }
[%%expect {|
type qualified = int{ _ >= Lib.bound }
|}]
