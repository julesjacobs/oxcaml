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

(** Generic operations on refinement predicates ({!Types.refinement_expression}).

    A predicate is a typed mirror: source shape, with per-node types and
    identities selected by Typecore.  The pieces of it that live in the
    type graph proper are its interior types — the stored node types
    ([rexp_type]) and the written constraint types ([Rexp_constraint]),
    including nested refinements within either.  The traversals below
    visit exactly those; the callers decide what to do with them. *)

open Types

(** Fold over the interior types of a predicate: every stored [rexp_type]
    and every written constraint type. *)
val fold_types :
  ('a -> type_expr -> 'a) -> 'a -> refinement_expression -> 'a

val iter_types : (type_expr -> unit) -> refinement_expression -> unit

(** Like {!fold_types}, but over the *written* constraint types only —
    see {!iter_written_types}. *)
val fold_written_types :
  ('a -> type_expr -> 'a) -> 'a -> refinement_expression -> 'a

(** Like {!iter_types}, but over the *written* constraint types only,
    skipping the stored node types.  For consumers that reason about the
    type's structure (well-foundedness, cycle reporting): a stored node
    type may share nodes with the enclosing type (an own-domain binder's
    instance reaches back into the domain), and such metadata cycles are
    not type-structure cycles. *)
val iter_written_types :
  (type_expr -> unit) -> refinement_expression -> unit

(** Rebuild a predicate.  [type_expr] is applied to written constraint types;
    [stored_type_expr] is applied to stored node types and defaults to
    [type_expr].  Both receive the rename map in force at that node, so a
    predicate-local binder occurring in a nested refinement inside a stored
    type freshens with its binder.
    If [freshen] is set, every binder ident introduced inside the predicate
    is renamed to a fresh stamp ([Subst] freshens binder stamps on import;
    [Btype] does not).  [rename] maps externally-bound idents (arrow
    binders); [value_path] rewrites the paths of free idents; [type_path]
    rewrites constructor paths and field parent paths. *)
val map :
  ?rename:Ident.t Ident.Map.t ->
  ?freshen:bool ->
  ?value_path:(Path.t -> Path.t) ->
  ?type_path:(Path.t -> Path.t) ->
  ?stored_type_expr:(Ident.t Ident.Map.t -> type_expr -> type_expr) ->
  type_expr:(Ident.t Ident.Map.t -> type_expr -> type_expr) ->
  refinement_expression -> refinement_expression

(** Syntactic alpha-equivalence over shape, written constraint types and
    identity keys; stored node types are ignored.  [type_eq] compares
    written interior types and receives the binder pairing in force at the
    comparison point — the [pairs] argument extended with the local
    binders in scope — so that predicates nested inside those types can
    consult it (see [Ctype.arrow_binder_pairs]).  [pairs] gives the
    pairing of externally-bound idents (the arrow binders of the two
    types being compared). *)
val equal :
  type_eq:(pairs:(Ident.t * Ident.t) list -> type_expr -> type_expr -> bool) ->
  pairs:(Ident.t * Ident.t) list ->
  refinement_expression -> refinement_expression -> bool

(** Back to surface syntax, for printing.  [var_name] chooses the printed
    name of a bound ident; [value_ident] renders a free ident from its
    resolved (possibly substituted) path; [field_ident] renders a field
    from its parent record type path and label name; [core_type] renders
    an interior type.  Holes print as [_] via [Pexp_hole]. *)
val untype :
  var_name:(Ident.t -> string) ->
  value_ident:(Path.t -> Longident.t Location.loc) ->
  constructor_ident:(Path.t -> Longident.t Location.loc) ->
  field_ident:(Path.t -> string -> Longident.t Location.loc) ->
  core_type:(type_expr -> Parsetree.core_type) ->
  refinement_expression -> Parsetree.expression

(** Does the predicate mention the given bound ident? *)
val mentions_ident : Ident.t -> refinement_expression -> bool

(** Reclassify free mentions ([Rexp_ident (Pident id, _)]) of the given
    idents as bound mentions ([Rexp_var id]).  A refinement nested inside
    a predicate's constraint type is typed by its own reentry, which sees
    the enclosing predicate's [let]/[fun]/[match] binders as ordinary
    environment values; the enclosing mirror build, which knows its
    binders, promotes them so that alpha-equivalence and substitution
    treat them as the bound occurrences they are. *)
val promote_locals :
  Ident.Set.t -> refinement_expression -> refinement_expression

(** The first free value path, constructor path or field parent path in
    the predicate for which [f] answers, if any.  Interior types are not
    scanned here; the caller scans the type graph. *)
val find_value_path :
  (Path.t -> 'a option) -> refinement_expression -> 'a option
