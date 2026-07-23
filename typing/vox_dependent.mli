open Types

val validate_specification_only_attribute : Parsetree.attributes -> unit
(** Validate and mark [@@vox.spec_only] on a value declaration. *)

val is_specification_only : Parsetree.attributes -> bool
(** Whether a value declaration is available only to refinement terms. *)

val logical_type : type_expr -> type_expr
(** Erase refinements while preserving the surrounding type graph. *)

val mentions : Ident.t -> type_expr -> bool
(** Whether a refinement anywhere in the type mentions the given bound value. *)

val mentions_identifier : Ident.t -> type_expr -> bool
(** Whether a refinement mentions the identifier as either a bound value or
    the head of a free value path. *)

val instantiate :
  binder:Ident.t ->
  with_:Refinement.t ->
  type_expr ->
  type_expr
(** Capture-avoiding, non-mutating opening of a dependent arrow codomain. *)

val rename :
  binder:Ident.t ->
  as_:Ident.t ->
  type_expr ->
  type_expr
(** Rename one dependent binder throughout a codomain while preserving the
    type metadata carried by each occurrence. *)

val validate_scopes : type_expr -> (unit, Ident.t) result
(** Check that every bound occurrence in a refinement is owned by its
    refinement descriptor or by exactly one enclosing dependent arrow. *)

val validate_signature : signature -> (unit, Ident.t) result
(** Apply [validate_scopes] to every type rooted in a signature. *)
