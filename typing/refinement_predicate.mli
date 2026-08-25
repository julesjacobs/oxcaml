(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Copyright 2026 Jane Street Group LLC                                  *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Generic operations on refinement predicates
    ({!Types.refinement_expression}). *)

open Types

(** Rebuild a predicate.  [rename_bound] renames every binder introduced
    inside the predicate;
    [rename] maps externally-bound idents;
    [value_path] rewrites the paths of free idents. *)
val map :
  ?rename:Ident.t Ident.Map.t ->
  ?rename_bound:(Ident.t -> Ident.t) ->
  ?value_path:(Path.t -> Path.t) ->
  ?constructor_path:(Path.t -> Path.t) ->
  ?type_path:(Path.t -> Path.t) ->
  ?type_expr:(type_expr -> type_expr) ->
  ?location:(Location.t -> Location.t) ->
  refinement_expression -> refinement_expression

(** Fold over every persistent type annotation in a predicate. *)
val fold_types :
  ('a -> type_expr -> 'a) -> 'a -> refinement_expression -> 'a

(** Syntactic alpha-equivalence.  [pairs] gives the pairing of
    externally-bound idents. *)
val equal :
  pairs:(Ident.t * Ident.t) list ->
  refinement_expression -> refinement_expression -> bool

(** Back to surface syntax, for printing.  [var_name] chooses the printed
    name of a bound ident; [value_ident] renders a free ident from its
    resolved (possibly substituted) path. *)
val untype :
  var_name:(Ident.t -> string) ->
  value_ident:(Path.t -> Longident.t Location.loc) ->
  constructor_ident:(Path.t -> Longident.t Location.loc) ->
  label_ident:(Path.t -> string -> Longident.t Location.loc) ->
  refinement_expression -> Parsetree.expression

(** Find an occurrence of one of the given bound identifiers. *)
val find_ident : Ident.Set.t -> refinement_expression -> Ident.t option

(** Identifiers bound inside the predicate. *)
val bound_idents : refinement_expression -> Ident.Set.t

(** The first free value, constructor, or record-owner type path in the
    predicate for which [f] answers, if any. *)
val find_dependency_path :
  (Path.t -> 'a option) -> refinement_expression -> 'a option
