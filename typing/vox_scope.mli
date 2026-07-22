open Types

type scope_error =
  | Value of Ident.t
  | Module of Ident.t

val validate_scopes :
  ?program_scope:Ident.Set.t ->
  ?stable_scope:Ident.Set.t ->
  type_expr ->
  (unit, Ident.t) result
(** Check that every bound refinement identifier is owned by its refinement
    descriptor, by exactly one enclosing dependent arrow, or by the supplied
    lexical program scope. *)

val validate_scopes_with_modules :
  module_in_scope:(Ident.t -> bool) ->
  ?program_scope:Ident.Set.t ->
  ?stable_scope:Ident.Set.t ->
  type_expr ->
  (unit, scope_error) result
(** Additionally check that every qualified value path is rooted only in
    modules accepted by [module_in_scope]. *)

val validate_signature : signature -> (unit, Ident.t) result
(** Apply [validate_scopes] to every type rooted in a signature. *)
