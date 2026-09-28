(* TEST *)

(* test evaluation order

   'y' is translated into a constant, and is therefore considered
   non-recursive. With the current letrec compilation method,
   it should be evaluated before x and z.
*)
type 'a recursive_list = Nil | Cons of 'a * 'a recursive_list
type tree = Tree of tree recursive_list

let test =
  let rec x = (print_endline "effect"; Tree (Cons (y, Cons (z, Nil))))
  and y = (print_endline "effect"; Tree Nil)
  and z = (print_endline "effect"; Tree (Cons (x, Nil)))
  in
  match (x, y, z) with
    | (Tree (Cons (y1, Cons (z1, Nil))), Tree Nil, Tree (Cons (x1, Nil))) ->
      assert (y1 == y);
      assert (z1 == z);
      assert (x1 == x)
    | _ ->
      assert false
