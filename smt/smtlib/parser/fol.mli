(** Typed first-order formula IR (front-end quantified pipeline, stages 1-2). A quantified
    assertion is parsed into this IR {e before} any {!Oxsmt_core.Term.t} is built, so the
    standard clausification transforms — NNF/polarity, binder rename-apart, and (stage 2)
    Skolemization + definitional clausification — run on the formula skeleton rather than
    on hash-consed terms. Term construction is deferred to the leaf atoms, whose payload
    ['a] the module treats opaquely; the parser instantiates ['a] with a deferred reader
    that turns a leaf s-expression into a {!Oxsmt_core.Term.t} once binder images exist.

    The whole module is PURE (no {!Oxsmt_core.Context}, no minting) and parametric in the
    leaf type, so NNF/rename-apart are unit-testable in isolation from the solver. It is
    dark behind [OXSMT_QUANT_PIPELINE] at the parser boundary — building an IR value has
    no effect until a driver lowers it (stage 2), so the default-OFF path is
    byte-identical.

    {b Binder identity is the [id] field, never the source [name]} (design basis: "binder
    identity must not depend on source names"). Every binder gets a globally fresh [id]
    from {!fresh_binder}; a leaf atom references the binders in scope by their [id]s. This
    makes rename-apart meaningful even across duplicated sub-formulas: NNF duplicates a
    quantified sub-formula when it expands an [Iff]/[Xor]/[Ite] (each side appears once
    per polarity), and the two copies would otherwise share binder [id]s; {!rename_apart}
    freshens every binder occurrence so no [id] is shared between independent quantifiers
    — a prerequisite for sound Skolemization (a shared [id] across a universal and a
    dualized existential copy would mint one Skolem symbol for two distinct binders). *)

open Oxsmt_core

(** A typed quantifier binder. [id] is the identity (globally unique after
    {!rename_apart}); [name] is the source name, kept for diagnostics/auditing ONLY and
    never used for binding resolution; [sort] is the binder's sort. *)
type binder =
  { id : int
  ; name : string
  ; sort : Sort.t
  }

(** A first-order formula over leaf atoms of type ['a]. Boolean connectives and
    quantifiers are structural; a leaf [Atom] is an opaque, quantifier-free Bool
    sub-formula (the parser builds one per maximal quantifier-free subterm). [Ite] is a
    {e Boolean} if-then-else (all three arguments Bool); [Iff]/[Xor] are Boolean
    equality/xor. Theory if-then-else and theory equality live inside leaf atoms, never
    here. *)
type 'a t =
  | True
  | False
  | Atom of 'a
  | Not of 'a t
  | And of 'a t list
  | Or of 'a t list
  | Implies of 'a t * 'a t
  | Iff of 'a t * 'a t
  | Xor of 'a t * 'a t
  | Ite of 'a t * 'a t * 'a t
  | Forall of binder list * 'a t
  | Exists of binder list * 'a t

(** [fresh_binder ~name ~sort] allocates a binder with a globally fresh [id]. The counter
    is process-global and monotonic; distinct calls never collide, so binder identity is
    unique by construction (and rename-apart is a re-freshening after duplication). *)
val fresh_binder : name:string -> sort:Sort.t -> binder

(** [map_atoms f phi] rewrites every leaf atom with [f], preserving structure and binders.
    Used to lower an ['a t] whose leaves are parse-time readers into a ['b t] whose leaves
    are built terms. *)
val map_atoms : ('a -> 'b) -> 'a t -> 'b t

(** [iter_atoms f phi] applies [f] to every leaf atom, left to right. *)
val iter_atoms : ('a -> unit) -> 'a t -> unit

(** [nnf phi] is the negation normal form of [phi]: [Implies]/[Iff]/[Xor]/[Ite] are
    eliminated, negations are pushed to the leaves (so [Not] wraps only an [Atom]), and
    quantifiers are dualized under negation ([not (forall x. p)] becomes [exists x. not p]
    and dually). The result is logically {e equivalent} to [phi]. [Iff]/[Xor]/[Ite]
    expansion duplicates each operand once per polarity, so a quantified operand appears
    in two copies sharing binder [id]s — always follow [nnf] with {!rename_apart} before
    Skolemizing. *)
val nnf : 'a t -> 'a t

(** [is_nnf phi] holds iff [phi] contains no [Implies]/[Iff]/[Xor]/[Ite] and every [Not]
    wraps an [Atom]. A post-condition check for {!nnf} (and a test predicate). *)
val is_nnf : 'a t -> bool

(** [rename_apart ~rename_atom phi] returns [phi] with every binder given a globally fresh
    [id], so no [id] is shared between two quantifier occurrences (even ones duplicated by
    {!nnf}). [rename_atom remap a] must return [a] with each referenced binder [id] [i]
    replaced by [remap i] (identity for [id]s not bound above the atom); a leaf that
    carries no binder references may ignore [remap]. Binding structure and the formula
    shape are preserved; only [id]s change. *)
val rename_apart : rename_atom:((int -> int) -> 'a -> 'a) -> 'a t -> 'a t

(** [binder_ids phi] is every binder [id] introduced by a [Forall]/[Exists] in [phi], in
    pre-order, {e with multiplicity}. [List.length (binder_ids phi)] equal to the size of
    its de-duplicated set witnesses rename-apart (no shared [id]s). *)
val binder_ids : 'a t -> int list

(** [to_string leaf phi] renders [phi] as an s-expression-ish string ([leaf] renders an
    atom), binders shown by source [name]. For test readability and audit dumps; not a
    parser input. *)
val to_string : ('a -> string) -> 'a t -> string
