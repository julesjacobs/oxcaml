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
end

(** A located rejection: the subject or predicate cannot be stated in the
    term language (tier 2 — function-sorted, first-class module, object,
    unsolved variable, an unclosable datatype), or meets a construct this
    translation does not yet support.  Obligations fail closed: the walk
    turns this into a located user error.  Facts fail open: a fact source
    catches it and declines. *)
exception Unsupported of { loc : Location.t; reason : string }

(** A located predicate sort error: rexp is untyped and nothing upstream
    or downstream checks predicate sorts, so [int{ 1 + true }] compiles and
    dies here, as a sort error the user can read.  Obligations fail closed
    (a located user error); fact sources decline fail-open. *)
exception Ill_sorted of { loc : Location.t; message : string }

(** A predicate mentions a free value that is a mutable variable, or whose
    declared type does not cross logicality (mutable parts): no such
    predicate has one denotation, so this rejection is fail-closed even
    for facts — a fact with no single denotation is not conservative, it
    is wrong. *)
exception Reads_mutable_state of { loc : Location.t }

(** What the subject front end resolved while lowering, reported to the
    caller as it happens: the fact sources that ride on lowering
    (value descriptions at occurrences, apply codomains, immutable field
    labels, per-read mutable subjects) hang off these, keeping the fact
    rules in {!Vox_verify} without a second traversal.  The callback fires
    on every lowering of the node, memoized or not: each obligation
    snapshot decides for itself what a resolution deposits. *)
type resolved =
  | Resolved_ident of Path.t * Types.value_description
  | Resolved_apply of Typedtree.expression
      (** an application lowered as a stable [Call] or abstracted opaque *)
  | Resolved_field of Typedtree.expression * Data_types.label_description
      (** an immutable field read; the label carries the declared type *)
  | Resolved_mutvar of Ident.t
      (** a [Texp_mutvar] read, lowered to its per-read opaque constant *)

(** Map an OCaml type onto a sort: [bool -> Bool], [int -> Bitvec 63],
    [Bigint.t -> Int], concrete datatypes -> [Datatype] (registering their
    declarations for signature assembly), abstract types (and records with
    mutable fields, whose datatype extensionality would equate two
    states) -> [Uninterpreted].  A sort the language cannot represent
    (functions, first-class modules, objects, unsolved variables, open or
    unboxed definitions) is a located rejection (tier 2), never a silent
    drop. *)
val sort_of_type :
  Symbols.t -> loc:Location.t -> Env.t -> Types.type_expr ->
  Vox_logic.Sort.t

(** Close an obligation's signature over exactly the symbols its terms
    (facts first, then the goal) mention, in first-occurrence order, plus
    the datatype declarations reachable from any mentioned sort, run
    through {!Vox_logic.Signature.instantiate}.  An [instantiate]
    rejection (non-regular recursion, function-valued fields) raises
    {!Unsupported} at [loc]. *)
val to_signature :
  Symbols.t -> loc:Location.t -> terms:Ir.t list -> Vox_logic.Signature.t

(** Subject front end: typedtree -> IR.  Shallow and total on the
    supported forms; sorts are read off [exp_type], never reconstructed.
    A value-sorted form it does not support abstracts to a fresh opaque
    constant (tier 1), memoized per node.  [is_total_local] supplements
    the stability gate's occurrence-totality read for local binders,
    whose [@ total] annotation caps the checking mode without pinning the
    binder's mode variable — the walker supplies it from the binding's
    recorded [Texp_mode] annotation. *)
val lower_subject :
  Symbols.t -> ?on_resolved:(resolved -> Ir.t -> unit) ->
  ?is_total_local:(Ident.t -> bool) ->
  Typedtree.expression -> Ir.t

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
