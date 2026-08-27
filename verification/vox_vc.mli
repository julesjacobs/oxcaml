(** Generate each obligation with its path facts. The callback must either
    establish validity or raise; this function does not emit artifacts. *)
val generate :
  prove:(Location.t -> Vox_smt.query -> unit) -> Typedtree.structure -> unit
