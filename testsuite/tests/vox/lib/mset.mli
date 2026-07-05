(* A MUTABLE finite set behind a via-ABSTRACTED sealed interface: the
   representation is a mutable RustHorn-borrow carrier (Mset_lib.varr),
   but the .mli hides it entirely.  [t] is declared [refines (iset)],
   so a client binds [t] at the Lean set sort ISet and reasons in pure
   set vocabulary ([mem]/[ins]/[card]) -- the carrier, its abstraction
   [setof], and the whole borrow discipline never leave the unit.  The
   interface text never mentions [setof].  Operations mutate the set
   IN PLACE (the value is threaded [@ unique]); [insert]'s residual is
   the model insert [ins x s].  The .ml PROVES these specs honestly
   (ZERO assume_unchecked_): a via binder denotes the image, the
   implementation reaches the carrier through a [refine_] unpack, and
   the borrow's prophecy resolves at the image sort. *)

open Mset_lib

type t : value refines (iset)

val create : unit -> t{ card _ = 0 } @ unique

(* IN-PLACE insert: the residual is the same set, its model advanced to
   exactly [ins x s]. *)
val insert : (x : int) -> (s : t) @ unique -> t{ _ = ins x s } @ unique

(* Membership; the set comes back with its model intact. *)
val member :
  (x : int) -> (s : t) @ unique ->
  (bool{ _ = mem x s } * t{ _ = s }) @ unique
