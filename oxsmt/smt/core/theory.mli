(** The frozen THEORY plugin signature (ADR-0005) — the seam the CDCL(T) engine (M1/M4)
    drives and that EUF (M2) and LIA (M3) implement, through a thin adapter over their
    [Term]/[Context]-facing engines. Nelson–Oppen combination (M4) is itself a THEORY:
    [Combine (A) (B)] presents one THEORY to the engine (functor packaging à la Alt-Ergo
    [CC(X)]; engine-observable semantics are Z3's model-based combination).

    All shared vocabulary ([Atom]/[Lit]/[Explanation]/[Model]) lives in [core] because the
    module DAG forbids [theories → solver]. Frozen at the M1 THEORY freeze (ADR-0005
    Tranche A). Pure signature — no implementation module.

    {b Determinism (INVARIANTS.md I6, ADR-0005 C1–C8)} and the soundness contracts
    CONTRACT-EX (precedence-valid explanations), CONTRACT-SPLIT (a [Split] is a clausified
    disjunction over ≥2 distinct atoms), CONTRACT-MODEL, and CONTRACT-POISON (an
    {e engine} obligation: any exception escaping a THEORY op bricks the instance and
    degrades the query to [unknown], I8) are stated in the ADR; they are discipline on the
    caller/implementer, not encoded in this signature. *)

(** The effort of a {!THEORY.check}. *)
type effort =
  | Propagate
  (** cheap, in-search: theory propagation + fast inconsistency; never returns [Sat] or
      [Split]. *)
  | Final
  (** the SAT core has a full boolean model: the theory must be complete (LIA
      branch-and-bound for integrality; model-based Nelson–Oppen). *)

(** The result of a {!THEORY.check}. *)
type check_result =
  | Sat (** [Final] only: the theory certifies this assignment T-satisfiable. *)
  | Propagations of Lit.t list
  (** consistent so far; these literals are T-implied (lazy explanation via
      {!THEORY.explain}), in deterministic order (C1). *)
  | Conflict of Explanation.t (** the asserted set is T-inconsistent. *)
  | Split of Term.t list
  (** [Final] only: clausify each term to a literal and assert their {b disjunction} as
      one clause (ADR-0005 CONTRACT-SPLIT) — a B&B branch, an N-O ℤ-trichotomy, or an
      E-matching lemma. Must force a choice among ≥2 distinct atoms. *)

module type THEORY = sig
  type t

  (** [create ctx env] is an empty theory state bound to the session [Context] (ADR-0003
      D6): every term the theory builds mid-solve (a [Split] disjunct) goes through [ctx],
      sharing its tag stream and hash-consing (I6). *)
  val create : Context.t -> Env.t -> t

  (** The sole point a theory receives a [Term.t] (ADR-0005 CONTRACT-REG-1/2). Called as
      the clausifier internalizes each theory atom and for atoms minted from a {!Split}.
      The theory walks the term for subterms, indexes them by [Term] tag, and builds its
      structure (EUF: e-graph; LIA: bound/row). Idempotent (C7). *)
  val register_atom : t -> Atom.t -> Term.t -> unit

  (** Assert a signed literal (its atom is registered). Cheap incremental state update, no
      output; consistency/propagation are deferred to {!check}. Asserted in the current
      frame (see {!push}/{!pop}). *)
  val assert_lit : t -> Lit.t -> unit

  (** Theory reasoning over the currently-asserted literals; see {!check_result}.
      Propagations are returned in deterministic order (C1). [Sat]/[Split] are legal only
      at [Final]. *)
  val check : t -> effort -> check_result

  (** The premises + rule tag justifying a literal THIS theory propagated (a
      {!Propagations} element). Lazy but always available (§7), and precedence-valid
      (ADR-0005 CONTRACT-EX: every premise was assigned strictly before [lit] on the
      trail). Deterministic (C2). *)
  val explain : t -> Lit.t -> Explanation.t

  (** [push t] opens a backtrack frame; [pop t n] discards the last [n], restoring state
      to that checkpoint. A frame is opened at each SAT decision level and each user
      assertion frame, so assert-after-check and incremental push/pop are first-class
      (ADR-0005 D6). *)
  val push : t -> unit

  val pop : t -> int -> unit

  (** A candidate model, valid whenever the last {!check} was consistent — a complete,
      integer-valued, N-O-agreed model after [Final]→[Sat] (ADR-0005 CONTRACT-MODEL). Used
      by [Combine] for model-based combination (§6) and by the §8 sat evaluator. *)
  val model : t -> Model.t
end
