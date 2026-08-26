(* TEST
 expect;
*)

let value_after_effect = (Format.printf "producing an integer@."; 42 : @ total);;
[%%expect{|
producing an integer
val value_after_effect : int = 42
|}]

module Pure = struct
  let (increment @ total) x = x + 1
  let (apply @ total) f x = f x
  let (allocate @ total) x = ref x
end;;
[%%expect{|
module Pure :
  sig
    val increment : int -> int
    val apply : ('a -> 'b) -> 'a -> 'b
    val allocate : 'a -> 'a ref
  end
|}]

let answer = Pure.apply (fun x -> Format.printf "argument called@."; x + 1) 41;;
[%%expect{|
argument called
val answer : int = 42
|}]

module Effectful = struct
  let (print @ total) () = print_endline "not a total function"
end;;
[%%expect{|
Line 2, characters 27-40:
2 |   let (print @ total) () = print_endline "not a total function"
                               ^^^^^^^^^^^^^
Error: The value "print_endline" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 2, characters 22-63
         which is expected to be "total".
|}]

module Captured = struct
  let partial x = 1 / x
  let (call @ total) x = partial x
end;;
[%%expect{|
Line 3, characters 25-32:
3 |   let (call @ total) x = partial x
                             ^^^^^^^
Error: The value "partial" is "partial"
         because it closes over the value "(/)" at line 2, characters 20-21
         which is "partial".
       However, the value "partial" highlighted is expected to be "total"
         because it is used inside the function at line 3, characters 21-34
         which is expected to be "total".
|}]

module Mutable_read = struct
  let (read @ total) r = !r
end;;
[%%expect{|
Line 2, characters 25-26:
2 |   let (read @ total) r = !r
                             ^
Error: The value "(!)" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 2, characters 21-27
         which is expected to be "total".
|}]
