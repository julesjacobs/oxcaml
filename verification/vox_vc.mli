(** Generate each obligation with its path facts. The callback must either
    establish validity or raise; this function does not emit artifacts. *)
val generate :
  prove:(Location.t -> Vox_smt.query -> unit) -> Typedtree.structure -> unit

(** [Recursive_function.check_uses] must have validated [fn] first. *)
val check_termination :
  prove:(Location.t -> Vox_smt.query -> unit) ->
  self:Ident.t ->
  fn:Typedtree.expression ->
  measure:Typedtree.expression ->
  unit
