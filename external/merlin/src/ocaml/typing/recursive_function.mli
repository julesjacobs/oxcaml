exception Invalid of Location.t * string

val parameters :
  Typedtree.expression ->
  (Ident.t * Typedtree.pattern) list * Typedtree.expression

val check_predicates : Ident.t -> Typedtree.expression -> unit

(** Reject escaped, partially applied, and predicate occurrences of the
    recursive identifier, and calls in unsupported delayed bodies. Ordinary
    closures are traversed: their recursive calls must satisfy the same descent
    obligations as calls in the enclosing body. Call before checking descent. *)
val check_uses : Ident.t -> Typedtree.expression -> unit
