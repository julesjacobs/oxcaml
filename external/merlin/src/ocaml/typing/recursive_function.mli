exception Invalid of Location.t * string

val parameters :
  Typedtree.expression ->
  (Ident.t * Typedtree.pattern) list * Typedtree.expression

val check_predicates : Ident.t -> Typedtree.expression -> unit

(** Reject escaped, delayed, partially applied, and predicate occurrences of
    the recursive identifier. Call before checking any descent obligations. *)
val check_uses : Ident.t -> Typedtree.expression -> unit
