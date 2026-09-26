(** Invalid or inconclusive logical queries may be retried individually.
    Infrastructure failures must propagate directly. A caller supplying a shared
    budget must raise a different exception when it is exhausted. *)
exception Unproved of Location.error

(** Generate each obligation with its path facts. The callback must either
    establish validity or raise; this function does not emit artifacts. [batch]
    marks a combined query for several obligations; if it is not proved, each
    obligation is retried alone, so the callback may give a batch a smaller
    budget. *)
val generate :
  ?poll:(unit -> unit) ->
  prove:(batch:bool -> Location.t -> Vox_smt.query -> unit) ->
  Typedtree.structure ->
  unit

(** [Recursive_function.check_uses] must have validated [fn] first. *)
val check_termination :
  poll:(unit -> unit) ->
  prove:(batch:bool -> Location.t -> Vox_smt.query -> unit) ->
  self:Ident.t ->
  fn:Typedtree.expression ->
  measure:Typedtree.expression ->
  unit
