(* TEST *)

(* a test with lists, because cyclic lists are fun *)
type 'a recursive_list = Nil | Cons of 'a * 'a recursive_list

let next = function
  | Cons (0, Cons (1, Cons (2, Cons (3, Cons (4,
      Cons (5, Cons (6, Cons (7, Cons (8, Cons (9, tail)))))))))) -> tail
  | _ -> assert false

let test =
  let rec li =
    Cons (0, Cons (1, Cons (2, Cons (3, Cons (4,
      Cons (5, Cons (6, Cons (7, Cons (8, Cons (9, li))))))))))
  in
  assert (li == next (next li))
