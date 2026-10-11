(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Jules Jacobs, Jane Street                             *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

val type_declaration_ikind_gated :
  env:Env.t option -> path:Path.t -> Types.type_ikind

(** [declaration_logicality ~env ~path] reads the logicality axis off the kind
    of the type constructor [path]: whether it is logical when its parameters
    are ([Logical]), and for each parameter whether its logicality bears on the
    result. See Note [Logicality of recursive types] in ikind.ml. *)
val declaration_logicality :
  env:Env.t -> path:Path.t -> Jkind_axis.Logicality.t * bool list

(** As [declaration_logicality], but the base assumes logical abstract types.
    Their contribution must remain in the declaration's with-bounds. *)
val declaration_logicality_lower_bound :
  env:Env.t -> path:Path.t -> Jkind_axis.Logicality.t * bool list

(** Whether the type constructor [path] is logical when its parameters are.
    Total code may look inside a value only if its type constructor is one. *)
val declaration_is_logical : env:Env.t -> path:Path.t -> bool

val logicality_of_jkind :
  Env.t -> ('l * 'r) Types.jkind -> Jkind_axis.Logicality.t

val type_declaration_ikind_of_jkind :
  env:Env.t option ->
  params:Types.type_expr list ->
  Types.jkind_l ->
  Types.type_ikind

type mode_crossing_error

type subjkind_error =
  | Jkind_error of Jkind.Violation.t
  | Mode_crossing_error of mode_crossing_error

val subjkind_error_printing_env : subjkind_error -> Env.t option

(** Whether the error is (at least in part) on the logicality axis. *)
val subjkind_error_on_logicality : subjkind_error -> bool

val report_subjkind_error_with_offender :
  offender:(Format_doc.formatter -> unit) ->
  Env.t ->
  Format_doc.formatter ->
  subjkind_error ->
  unit

val report_subjkind_error_with_name :
  name:string -> Env.t -> Format_doc.formatter -> subjkind_error -> unit

val sub_jkind_l :
  ?allow_any_crossing:bool ->
  ?origin:string ->
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  context:Jkind.jkind_context ->
  Env.t ->
  Types.jkind_l ->
  Types.jkind_l ->
  (unit, subjkind_error) result

val check_type_expr_bound :
  ?origin:string ->
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  context:Jkind.jkind_context ->
  Env.t ->
  ty:Types.type_expr ->
  actual:Types.jkind_l ->
  bound:Types.jkind_l ->
  (unit, subjkind_error) result

val check_type_decl_bound :
  ?allow_any_crossing:bool ->
  ?origin:string ->
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  context:Jkind.jkind_context ->
  Env.t ->
  decl:Types.type_declaration ->
  actual:Types.jkind_l ->
  bound:Types.jkind_l ->
  (unit, subjkind_error) result

val crossing_of_jkind :
  context:Jkind.jkind_context ->
  Env.t ->
  ('l * 'r) Types.jkind ->
  Mode.Crossing.t

val crossing_of_type : Env.t -> Types.type_expr -> Mode.Crossing.t

val enforce_refinement_crossings : Types.jkind_l -> Types.jkind_l

val instance_poly_for_jkind' :
  (Types.type_expr list -> Types.type_expr -> Types.type_expr) ref

type sub_or_intersect = Jkind.sub_or_intersect

val sub_or_intersect :
  ?origin:string ->
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  context:Jkind.jkind_context ->
  Env.t ->
  (Allowance.allowed * 'r1) Types.jkind ->
  ('l2 * Allowance.allowed) Types.jkind ->
  sub_or_intersect

val sub_or_error :
  ?origin:string ->
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  context:Jkind.jkind_context ->
  Env.t ->
  (Allowance.allowed * 'r1) Types.jkind ->
  ('l2 * Allowance.allowed) Types.jkind ->
  (unit, Jkind.Violation.t) result

(** Apply path substitutions to a constructor ikind. *)
val substitute_decl_ikind_with_lookup :
  lookup_type:(Path.t -> Subst.Ikind_substitution.type_lookup_result) ->
  lookup_jkind:(Path.t -> Subst.Ikind_substitution.jkind_lookup_result) ->
  Types.type_ikind ->
  Types.type_ikind
