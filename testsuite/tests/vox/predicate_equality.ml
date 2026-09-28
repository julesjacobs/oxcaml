(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* [=] in a refinement is interpreted only at int, bool and Bigint.t; other
   types need the logical equality [===]. *)

let (same_list @ total) (x : int list) : {y : int list | y = x} = x;;
[%%expect{|
Line 1, characters 59-60:
1 | let (same_list @ total) (x : int list) : {y : int list | y = x} = x;;
                                                               ^
Error: In refinements and ghost code, "=" works only at int, bool and Bigint.t,
       not at "int list". Use "x === y" for logical equality.
|}]

let (same_string @ total) (x : string) : {y : string | not (y <> x)} = x;;
[%%expect{|
Line 1, characters 62-64:
1 | let (same_string @ total) (x : string) : {y : string | not (y <> x)} = x;;
                                                                  ^^
Error: In refinements and ghost code, "<>" works only at int,
       bool and Bigint.t, not at "string".
       Use "not (x === y)" for logical equality.
|}]

let (logical @ total) (x : int list) : {y : int list | y === x} = x;;
[%%expect{|
val logical : (x : int list) -> {y : int list | y === x} = <fun>
|}]

let (same_int @ total) (x : int) : {y : int | y = x} = x;;
[%%expect{|
val same_int : (x : int) -> {y : int | y = x} = <fun>
|}]

let (same_bool @ total) (x : bool) : {y : bool | y = x} = x;;
[%%expect{|
val same_bool : (x : bool) -> {y : bool | y = x} = <fun>
|}]

let (same_bigint @ total) (x : Bigint.t) : {y : Bigint.t | y = x} = x;;
[%%expect{|
val same_bigint : (x : Bigint.t) -> {y : Bigint.t | y = x} = <fun>
|}]

let (in_ghost_code @ total) (x : int list) = ghost_ (x = x);;
[%%expect{|
Line 1, characters 55-56:
1 | let (in_ghost_code @ total) (x : int list) = ghost_ (x = x);;
                                                           ^
Error: In refinements and ghost code, "=" works only at int, bool and Bigint.t,
       not at "int list". Use "x === y" for logical equality.
|}]
