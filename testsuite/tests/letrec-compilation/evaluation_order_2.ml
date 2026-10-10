(* TEST *)

(* A variant of evaluation_order_1.ml where the side-effects
   are inside the blocks.
   Effect are not named to allow different evaluation orders (flambda
   and clambda differ on this point).
*)
type 'a recursive_list = Nil | Cons of 'a * 'a recursive_list
type tree = Tree of tree recursive_list

let test =
  let rec x = Tree (Cons ((print_endline "effect"; y), Cons (z, Nil)))
  and y = Tree (print_endline "effect"; Nil)
  and z = Tree (print_endline "effect"; Cons (x, Nil))
  in
  match (x, y, z) with
    | (Tree (Cons (y1, Cons (z1, Nil))), Tree Nil, Tree (Cons (x1, Nil))) ->
      assert (y1 == y);
      assert (z1 == z);
      assert (x1 == x)
    | _ ->
      assert false
