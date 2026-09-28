(* TEST expect; *)
let use (f : (int -> int) @ total) x = f x;;
[%%expect{|
val use : (int -> int) @ total -> int -> int = <fun>
|}]
let bad (f : (int -> int @ total) @ partial) x = use f x;;
[%%expect{|
Line 1, characters 53-54:
1 | let bad (f : (int -> int @ total) @ partial) x = use f x;;
                                                         ^
Error: This value is "partial" but is expected to be "total".
Annotate the callback itself with "(f : (int -> int) @ total)".
A "total" annotation after the arrow constrains the result value.
|}]
let good (f : (int -> int) @ total) x = use f x;;
[%%expect{|
val good : (int -> int) @ total -> int -> int = <fun>
|}]
