(* TEST include stdlib_stable; flags = "-w -220"; expect;
*)

(* SOUNDNESS CAMPAIGN — Family 6 (predicate discipline).

   A refinement predicate is elaborated at total/logical mode: it must denote a
   pure, terminating proposition. If an impure, effectful, or diverging
   computation could be laundered THROUGH a predicate, the verification pass
   would reason about a value the predicate does not actually pin down. So every
   partial operation inside a predicate must reject at totality. Comparison
   primitives are ADMITTED as total only while elaborating a predicate (the R1
   admission); that admission must not leak into ordinary program code. *)

let expects_total (f @ total) = f
[%%expect {|
val expects_total : 'a @ total -> 'a = <fun>
|}]

(* PD1: an impure call inside a predicate rejects at totality. *)
let pd1 (x : int{ read_int () = _ }) = x
[%%expect {|
Line 1, characters 18-26:
1 | let pd1 (x : int{ read_int () = _ }) = x
                      ^^^^^^^^
Error: The value "read_int" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 1, characters 13-35).
|}]

(* PD2: dereferencing a mentioned ref inside a predicate rejects. *)
let r = ref 0
let pd2 (x : int{ !r = _ }) = x
[%%expect {|
val r : int ref = {contents = 0}
Line 2, characters 18-19:
2 | let pd2 (x : int{ !r = _ }) = x
                      ^
Error: The value "(!)" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 2, characters 13-26).
|}]

(* PD3: an effectful call as a predicate sub-term rejects. *)
let pd3 (x : int{ (print_int _; true) }) = x
[%%expect {|
Line 1, characters 19-28:
1 | let pd3 (x : int{ (print_int _; true) }) = x
                       ^^^^^^^^^
Error: The value "print_int" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 1, characters 13-39).
|}]

(* PD4: comparison IS admitted as total inside a predicate — accepts. *)
let pd4 (x : int{ _ > 0 }) = x
[%%expect {|
val pd4 : int{ (app[Stdlib!.>] _ 0) } -> int = <fun>
|}]

(* PD5: the comparison admission does NOT leak into program code — the same
   [(>)] in an ordinary closure still makes it partial. *)
let pd5 =
  let f = fun (x : int) -> x > 0 in
  expects_total f
[%%expect {|
Line 3, characters 16-17:
3 |   expects_total f
                    ^
Error: This value is "partial"
         because it closes over the value "(>)" at line 2, characters 29-30
         which is "partial".
       However, the highlighted expression is expected to be "total".
|}]
