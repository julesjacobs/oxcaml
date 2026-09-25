(** Invalid or inconclusive logical queries may be retried individually.
    Infrastructure failures must propagate directly. A caller supplying a shared
    budget must raise a different exception when it is exhausted. *)
exception Unproved of Location.error

(** Generate each obligation with its path facts. The callback must either
    establish validity or raise; this function does not emit artifacts. *)
val generate :
  ?poll:(unit -> unit) ->
  prove:(Location.t -> Vox_smt.query -> unit) ->
  Typedtree.structure ->
  unit

(** [Recursive_function.check_uses] must have validated [fn] first. *)
val check_termination :
  poll:(unit -> unit) ->
  prove:(Location.t -> Vox_smt.query -> unit) ->
  self:Ident.t ->
  fn:Typedtree.expression ->
  measure:Typedtree.expression ->
  unit
