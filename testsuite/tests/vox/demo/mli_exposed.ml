type ilist =
  | Nil
  | Cons of int * ilist

let two : ilist{ len _ = 2 } = Cons (1, Cons (2, Nil))
