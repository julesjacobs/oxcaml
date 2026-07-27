(* A mutable field the parse tree cannot see.

   [kid] is not in lexical scope as a field of [M.t] -- [M] is never opened.
   The label this pattern resolves to is chosen from the expected type, and
   the [kid] that is in scope belongs to [shadow], a different and immutable
   record.  So reading the parse tree and asking which labels named [kid] are
   in scope answers "one, immutable", and that answer is wrong.

   This is why the mutability of a field crossed by a descent is settled on
   the typed tree rather than on the parse tree.

   By the time it is settled the grant has been made, so what happens next is
   that it is given back: the recursive occurrences carry a mode variable
   that nothing has constrained, the totality is pushed out of it, and the
   binding is left partial.  The refusal is then the same one the plainly
   mutable twin gets -- asking for the binding to be total is what fails --
   and a program that never asks keeps compiling, as it did before a record
   pattern gave a sub-term relation at all.  Only when the grant has already
   been used inside the group is there nothing to give back, and then the
   crossing is an error of its own. *)
let (expects_total @ total) (f @ total) = f

module M : sig
  type t = { mutable kid : t option }
  val make : unit -> t
end = struct
  type t = { mutable kid : t option }
  let make () = { kid = None }
end

type shadow = { kid : int }

let shadow_kid (s : shadow) = s.kid

let rec depth (t : M.t) : int =
  match t with
  | { kid = None } -> 0
  | { kid = Some kid } -> 1 + depth kid

let total_use () = expects_total depth
