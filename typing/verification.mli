(** The verifier is told whether the structure is a whole compilation unit
    (rather than, say, a toplevel phrase). *)
val install : (whole_unit:bool -> Typedtree.structure -> unit) -> unit

(** Run after delayed typing checks and before emitting artifacts.
    Without a verifier, reject static refinement introductions. [run] checks
    a toplevel phrase; [run_unit] checks a whole compilation unit. *)
val run : Typedtree.structure -> unit

val run_unit : Typedtree.structure -> unit

val install_termination :
  (self:Ident.t -> fn:Typedtree.expression ->
   measure:Typedtree.expression -> unit) -> unit

(** Runs before generalization; the default rejects unverified measures. *)
val check_termination :
  self:Ident.t -> fn:Typedtree.expression ->
  measure:Typedtree.expression -> unit
