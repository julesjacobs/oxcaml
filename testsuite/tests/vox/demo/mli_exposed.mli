(* Contrast to mli_totalspec.mli: this interface EXPOSES [len]'s
   equations, by writing them in an interface block as an
   [@[grind, expose]] def.  Clients may now UNFOLD [len] over the
   datatype's constructors -- more provable, less abstract. *)

type ilist =
  | Nil
  | Cons of int * ilist

[%%vox.lean {lean|
@[grind, expose] public def len : Vox_Mli_exposed_ilist -> Int
  | .Nil => 0
  | .Cons _ t => 1 + len t
|lean}]

val two : ilist{ len _ = 2 }
