(** Source-language encoding shared by program expressions and predicates.
    [None] leaves the caller to reject a proof or use an opaque result. *)

type context

val create_context : unit -> context

val sort : context -> Env.t -> Types.type_expr -> Vox_smt.sort option

type data_kind =
  | Tuple_data of Vox_smt.Constructor.t
  | Record_data of Vox_smt.Constructor.t
  | Variant_data of (string * Vox_smt.Constructor.t) list

type data =
  { declaration : Vox_smt.datatype_declaration;
    kind : data_kind
  }

val data : context -> Env.t -> Types.type_expr -> data option

val same_nominal_data_type : Env.t -> Types.type_expr -> Types.type_expr -> bool

val declarations : context -> data -> Vox_smt.datatype_declaration list

val declarations_of_sort :
  context -> Vox_smt.sort -> Vox_smt.datatype_declaration list

val sort_has_iarray : context -> Vox_smt.sort -> bool

val is_iarray_sort : context -> Vox_smt.sort -> bool

val iarray :
  context ->
  Env.t ->
  Types.type_expr ->
  (Vox_smt.sort * Vox_smt.sort option) option

val primitive : Env.t -> Path.t -> (string * int) option

val value_constant :
  context -> Env.t -> Types.type_expr -> Path.t -> Vox_smt.term option

val operation :
  context ->
  Env.t ->
  function_type:Types.type_expr ->
  result_type:Types.type_expr ->
  string ->
  Vox_smt.term option list ->
  Vox_smt.term option

val signature :
  context ->
  Env.t ->
  Types.type_expr ->
  int ->
  (Vox_smt.sort list * Vox_smt.sort) option

val constant : Typedtree.constant -> Vox_smt.term option

val rconstant : Parsetree.constant -> Vox_smt.term option

val constructor :
  context -> Env.t -> Types.type_expr -> string -> Vox_smt.term option

val rconstructor :
  context -> Env.t -> Types.type_expr -> Path.t -> Vox_smt.term option
