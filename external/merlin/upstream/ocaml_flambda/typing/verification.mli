val install : (Typedtree.structure -> unit) -> unit

(** Run after delayed typing checks and before emitting artifacts.
    Without a verifier, reject static refinement introductions. *)
val run : Typedtree.structure -> unit

val install_termination :
  (self:Ident.t -> fn:Typedtree.expression ->
   measure:Typedtree.expression -> unit) -> unit

(** Runs before generalization; the default rejects unverified measures. *)
val check_termination :
  self:Ident.t -> fn:Typedtree.expression ->
  measure:Typedtree.expression -> unit
