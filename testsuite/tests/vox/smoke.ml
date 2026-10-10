(* TEST
 expect;
*)

let answer = List.fold_left ( + ) 0 [20; 22];;
[%%expect{|
val answer : int = 42
|}]
