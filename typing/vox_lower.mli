(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Jules Jacobs, Jane Street                             *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Lowering refinement obligations into the solver term language: one
    sorted VC IR, two front ends — subjects ({!Typedtree.expression},
    fully typed) and predicates ({!Types.refinement_expression}, resolved
    but untyped) — and one emitter into {!Vox_logic.Term}.  This module
    also owns the operator table, the symbol allocator, signature assembly
    and per-obligation canonicalisation
    (design-docs/vc-generation.md, "Lowering: one sorted language"). *)

module Ir : sig
  (** The sorted VC IR: {!Vox_logic.Term}'s shape with a sort on every
      node, plus the forms that instantiation and normalisation remove
      before emission.  Every fact, goal, subject and predicate is built
      here and crosses the one {!val:emit} below, so each construct has
      exactly one meaning. *)
  type t =
    { desc : desc
    ; sort : Vox_logic.Sort.t
    ; loc : Location.t
    }

  and desc =
    | Var of string  (** a symbol allocated by {!Symbols} *)
    | Const of Vox_logic.Literal.t
    | App of Vox_logic.Op.t * t list
    | Call of string * t list
    | Ite of t * t * t
    | Construct of string * t list
    | Select of string * int * t
    | Test of string * t
    | Hole
        (** [_], the value under obligation; removed by
            {!val:substitute_hole} before emission. *)
    | Let of string * t * t
        (** Predicate-local [let]; removed by substitution.  Binders bind
            allocated symbol names, unique by construction, so
            substitution is capture-free. *)
    | Lambda of string list * t
        (** Predicate-local [fun]; removed by beta reduction where
            applied, a located rejection where not.  [Rexp_match] has no
            IR form: the predicate front end lowers it to
            [Ite]/[Test]/[Select] directly. *)
end

module Symbols : sig
  (** The symbol allocator and table: every symbol the lowering mints or
      resolves — variables, uninterpreted functions, uninterpreted sorts,
      constructors and selectors — with its sort and any function or
      datatype declaration it pulls in.  A symbol's key is its resolved
      identity (stamped ident, or dotted module path) plus, for functions,
      the ground sort signature of the use.  The table is bookkeeping, not
      soundness: it never decides what is true, only what must be
      declared. *)
  type t

  val create : unit -> t

  (** The symbol for a value path: [Ident.unique_name] for locals (the
      stamp keeps shadowed locals distinct), the dotted spelling for
      module paths.  Records the variable for signature assembly. *)
  val value : t -> Path.t -> sort:Vox_logic.Sort.t -> string

  (** The symbol for an uninterpreted function at one ground
      instantiation; the key includes the sort signature, so a polymorphic
      function used at two ground instantiations yields two declarations,
      mangled in {!Vox_logic.Signature.instantiate}'s [name<key,...>]
      shape. *)
  val func :
    t -> Path.t -> params:Vox_logic.Sort.t list ->
    result:Vox_logic.Sort.t -> string

  (** A fresh opaque constant, [result/<counter>]: tier-1 abstraction of a
      call that fails the stability gate, a per-read mutable subject, or a
      guarded shift fallback. *)
  val fresh_opaque : t -> sort:Vox_logic.Sort.t -> string

  (** Close over the table: the declared variables and functions, plus the
      datatype declarations reachable from any mentioned sort, run through
      {!Vox_logic.Signature.instantiate}. *)
  val to_signature : t -> Vox_logic.Signature.t
end

(** Map an OCaml type onto a sort: [bool -> Bool], [int -> Bitvec 63],
    [Bigint.t -> Int], concrete datatypes -> [Datatype], abstract types ->
    [Uninterpreted].  A sort the language cannot represent (functions,
    first-class modules, objects, unsolved variables) is a located
    rejection (tier 2), never a silent drop. *)
val sort_of_type : Env.t -> Types.type_expr -> Vox_logic.Sort.t

(** Subject front end: typedtree -> IR.  Shallow and total on the
    supported forms; sorts are read off [exp_type], never reconstructed.
    A value-sorted form it does not support abstracts to a fresh opaque
    constant (tier 1), memoized per node. *)
val lower_subject : Symbols.t -> Typedtree.expression -> Ir.t

(** Predicate front end: rexp -> IR.  A located sort checker — rexp is
    untyped and nothing upstream or downstream checks predicate sorts —
    and a normaliser to the quantifier-free fragment.  [hole_sort] is the
    refined type's payload sort; free paths resolve in [env] (the
    obligation site's environment).  A sort clash or residual binder form
    is a located error at the obligation's site. *)
val lower_predicate :
  Symbols.t ->
  env:Env.t ->
  hole_sort:Vox_logic.Sort.t ->
  Types.refinement_expression ->
  Ir.t

(** Instantiation: the predicate's IR with the subject's IR term
    substituted for {!Ir.Hole}.  Capture-free by construction (unique
    binder names). *)
val substitute_hole : Ir.t -> hole:Ir.t -> Ir.t

(** The one emitter: IR -> {!Vox_logic.Term}, trivial by construction —
    the IR is sorted, hole-free and binder-free by the time it emits.  A
    residual [Hole], [Let] or [Lambda] is an internal defect, never a
    located user error. *)
val emit : Ir.t -> Vox_logic.Term.t

(** Renumber an obligation's symbols deterministically in
    first-occurrence order — a function of the source text alone —
    consistently across terms and signature, so printing baselines do not
    churn when unrelated edits shift [Ident] stamps.  Both backends
    receive the canonicalised bytes. *)
val canonicalise : Vox_logic.Obligation.t -> Vox_logic.Obligation.t
