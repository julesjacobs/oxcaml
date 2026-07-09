(* An .mli that EXPORTS a total_ spec function by NAME only.  [val
   total_ len] gives clients the name and its sort signature -- so
   their refinements may mention [len] and receive facts about it --
   but NOT its defining equations: the implementation's [total_ len]
   body stays private.  Clients reason from the exported contracts,
   never by unfolding [len].  (To expose the equations instead, write
   them in an interface [%%vox.lean] block; see mli_exposed.mli.) *)

type ilist =
  | Nil
  | Cons of int * ilist

val total_ len : ilist -> int

val two : ilist{ len _ = 2 }

val append : (a : ilist) -> (b : ilist) -> ilist{ len _ = len a + len b }
