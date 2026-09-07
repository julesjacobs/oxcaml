(** Source-language encoding shared by program expressions and predicates.
    [None] leaves the caller to reject a proof or use an opaque result. *)

val sort : Env.t -> Types.type_expr -> Vox_smt.sort option

val primitive : Env.t -> Path.t -> (string * int) option

val value_constant : Env.t -> Types.type_expr -> Path.t -> Vox_smt.term option

val operation :
  Env.t ->
  function_type:Types.type_expr ->
  result_type:Types.type_expr ->
  string ->
  Vox_smt.term option list ->
  Vox_smt.term option

val signature :
  Env.t -> Types.type_expr -> int -> (Vox_smt.sort list * Vox_smt.sort) option

val constant : Typedtree.constant -> Vox_smt.term option

val rconstant : Parsetree.constant -> Vox_smt.term option

val constructor : Env.t -> Types.type_expr -> string -> Vox_smt.term option

val rconstructor : Env.t -> Types.type_expr -> Path.t -> Vox_smt.term option
