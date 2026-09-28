(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* A non-recursive toplevel binding annotated [total] stays total, as in a
   compiled structure, so later phrases can use it in predicates. Other
   toplevel bindings keep the legacy mode of the toplevel. *)

let (two @ total) () = 2;;
[%%expect{|
val two : unit -> int = <fun>
|}]

let g (x : unit) : {r : int | r = two ()} = two ();;
[%%expect{|
val g : unit -> {r : int | r = (two ())} = <fun>
|}]

(* Not annotated: legacy, so partial. *)
let three () = 3;;
[%%expect{|
val three : unit -> int = <fun>
|}]

let h (x : unit) : {r : int | r = three ()} = three ();;
[%%expect{|
Line 1, characters 34-39:
1 | let h (x : unit) : {r : int | r = three ()} = three ();;
                                      ^^^^^
Error: The value "three" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 1, characters 30-42).
|}]

let (four @ total) () = 2 let k (x : unit) : {r : int | r = four ()} = four ();;
[%%expect{|
val four : unit -> int = <fun>
val k : unit -> {r : int | r = (four ())} = <fun>
|}]

(* A partial value stays partial. *)
let counter = ref 0;;
let next () = incr counter; !counter;;
[%%expect{|
val counter : int ref = {contents = 0}
val next : unit -> int = <fun>
|}]

let bad (x : unit) : {r : int | r = next ()} = next ();;
[%%expect{|
Line 1, characters 36-40:
1 | let bad (x : unit) : {r : int | r = next ()} = next ();;
                                        ^^^^
Error: The value "next" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 1, characters 32-43).
|}]

let (bad_total @ total) () = next ();;
[%%expect{|
Line 1, characters 29-33:
1 | let (bad_total @ total) () = next ();;
                                 ^^^^
Error: The value "next" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 24-36
         which is expected to be "total".
|}]
