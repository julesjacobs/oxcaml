(* TEST *)

let rec x = let _y = [| |] in ();;

type 'a recursive_list = Nil | Cons of 'a * 'a recursive_list
let rec x = let y = [| |] in Cons (y, x);;
