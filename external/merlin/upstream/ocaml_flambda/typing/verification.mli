(** The verifier is told whether the structure is a whole compilation unit
    (rather than, say, a toplevel phrase). *)
val install :
  (whole_unit:bool -> interface:Typedtree.refinement_site option ->
   Typedtree.structure -> unit) -> unit

(** Run after delayed typing checks and before emitting artifacts.
    Without a verifier, reject static refinement introductions and
    refinement subsumption obligations. [run] checks a toplevel phrase;
    [run_unit] checks a whole compilation unit, with the obligations of its
    inclusion in its interface. Both discard the attached sites. *)
val run : Typedtree.structure -> unit

val run_unit : ?interface:Typedtree.refinement_site -> Typedtree.structure -> unit

(** Refinement subsumption sites of the structure being checked, attached to
    the descriptor of the [Tmod_constraint] or [Tmod_apply] that needs them.
    [reset_refinement_sites] starts a new phrase or unit. *)
val reset_refinement_sites : unit -> unit
val attach_refinement_site :
  Typedtree.module_expr_desc -> Typedtree.refinement_site -> unit

(** The site of this descriptor, if it has not been discharged yet; it is
    discharged now. *)
val find_refinement_site :
  Typedtree.module_expr_desc -> Typedtree.refinement_site option

(** The sites not discharged yet; they are discharged now. *)
val unconsumed_refinement_sites : unit -> Typedtree.refinement_site list
val has_refinement_sites : unit -> bool

val install_termination :
  (self:Ident.t -> fn:Typedtree.expression ->
   measure:Typedtree.expression -> unit) -> unit

(** Runs before generalization; the default rejects unverified measures. *)
val check_termination :
  self:Ident.t -> fn:Typedtree.expression ->
  measure:Typedtree.expression -> unit
