(* A mutable field the parse tree cannot see.

   [kid] is not in lexical scope as a field of [M.t] -- [M] is never opened.
   The label this pattern resolves to is chosen from the expected type, and
   the [kid] that is in scope belongs to [shadow], a different and immutable
   record.  So reading the parse tree and asking which labels named [kid] are
   in scope answers "one, immutable", and that answer is wrong.

   This is why the mutability of a field crossed by a descent is settled on
   the typed tree rather than on the parse tree, and why failing to settle it
   there is an error rather than a silent grant. *)
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
