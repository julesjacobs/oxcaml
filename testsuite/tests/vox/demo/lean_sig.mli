(* A specced interface: the datatype, its measure (as an embedded
   prelude block, exported through the .cmi), and refinements using it.
   Clients need no -vox-prelude flag; neither does the implementation
   (it reads this interface's .cmi like any other import). *)

type ilist =
  | Nil
  | Cons of int * ilist

[%%vox.lean {lean|
@[grind, expose] public def len : Vox_Lean_sig_ilist -> Int
  | .Nil => 0
  | .Cons _ t => 1 + len t
|lean}]

val two : ilist{ len _ = 2 }

val push : ilist{ len _ = 2 } -> ilist{ len _ = 3 }
