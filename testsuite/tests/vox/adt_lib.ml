(* Auxiliary module for lean_adt.ml: a refined ADT value crosses the
   module boundary (constructor paths travel through the .cmi and are
   remapped by Subst on import). *)

type t =
  | K of int
  | L

let k3 : t{ _ = K 3 } = refine_ (K 3)
