[@@@ocaml.warning "+a-40-41-42"]

(** Merge blocks and eliminate dead blocks. *)
val run :
  ?allow_terminator_value_propagation_before_regalloc:bool ->
  Cfg_with_layout.t ->
  Cfg_with_layout.t
