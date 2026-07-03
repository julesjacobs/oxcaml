(* Implementation of lean_sig.mli.  Note: NO prelude block here -- the
   verifier finds [len] in the interface's .cmi (the implementation is
   checked against its own interface's specs). *)

type ilist =
  | Nil
  | Cons of int * ilist

let two : ilist{ len _ = 2 } = refine_ (Cons (1, Cons (2, Nil)))

let push (l : ilist{ len _ = 2 }) : ilist{ len _ = 3 } =
  refine_ (Cons (9, l))
