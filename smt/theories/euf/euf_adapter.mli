(** The ADR-0005 [THEORY] adapter for the EUF engine ({!Oxsmt_euf.Euf}) — the thin layer
    that presents the proof-producing congruence-closure engine to the CDCL(T) engine (M4)
    as one frozen {!Oxsmt_core.Theory.THEORY} plugin. The engine stays
    [Term]/[Context]-facing and freeze-agnostic; this adapter maps the frozen
    {!Oxsmt_core.Atom}/{!Oxsmt_core.Lit} currency onto the engine's opaque premise token
    (['p] instantiated internally) and translates engine explanations into the frozen
    {!Oxsmt_core.Explanation} currency (premise set + [Rule_tag.Euf_congruence]).

    {b Which atoms this owns (ADR-0005, {!Oxsmt_core.Theory_view}).} A non-Bool
    [Equality (a, b)] (positive ⇒ [a = b], negative ⇒ [a ≠ b]) and a Bool-codomain
    [Predicate]/[Bool_lit] atom. A predicate/bool atom [p] has no native equality form, so
    the adapter encodes it against reserved [true]/[false] constants and a standing
    [true ≠ false] disequality (textbook CC-for-predicates): positive ⇒ [p = true],
    negative ⇒ [p = false]. That axiom is justified by an internal token, {b never} a real
    [Lit], and is filtered out of every {!Oxsmt_core.Explanation} — so a returned premise
    set contains only literals the engine actually asserted. A non-owned atom (e.g. a LIA
    [Le_zero]) is {b accepted} by [register_atom] as [K_foreign]: its subterm closure is
    registered in the e-graph so model-based combination sees terms shared with owned
    atoms. A foreign atom is never asserted, watched, propagated, or explained; only
    [assert_lit] on a non-owned atom raises [Invalid_argument].

    {b Soundness discipline (DESIGN §7, I4).} The underlying engine self-checks every
    explanation it produces (independent naive-closure replay). This adapter adds no new
    reasoning; it only relabels premise tokens.

    {b Determinism (I6, ADR-0005 C1–C8).} All internal maps ([Atom]/[Term]-keyed) are used
    for lookup only, never iterated into an observable output; propagation and explanation
    order come straight from the deterministic engine. Owning task: TASKS.md M4-adapters
    (EUF). *)

include Oxsmt_core.Theory.THEORY

(** [internalize_term t term] internalises [term] and its full subterm closure into the
    e-graph WITHOUT binding it to any atom — no watch, no assertion, no propagation
    target, only e-nodes so congruence closes over it and [model] values it. The
    Nelson–Oppen combinator (internalization ADR §3) uses this to make EUF see a boundary
    term that surfaces only inside the OTHER theory's atom (e.g. [f x] occurring only in a
    LIA order atom). Additive to the child-facing interface; the frozen
    {!Oxsmt_core.Theory.THEORY} seam the engine drives is unchanged. Idempotent (C7);
    undone by [pop] of the frame that introduced it, exactly like {!register_atom}'s
    internalisation. *)
val internalize_term : t -> Oxsmt_core.Term.t -> unit

val check_fabric : t -> Oxsmt_core.Theory.effort -> Oxsmt_core.Fabric.check_result
val explain_fabric : t -> Oxsmt_core.Lit.t -> Oxsmt_core.Fabric.Explanation.t
val fabric_are_equal : t -> Oxsmt_core.Term.t -> Oxsmt_core.Term.t -> bool

(** The congruence child never fixes a term to an arithmetic value (that is the arithmetic
    child's role); it satisfies the shared [FABRIC_CHILD] surface with a constant [None].
    The fix-trigger only ever queries the arithmetic child. *)
val fixed_bounds : t -> Oxsmt_core.Term.t -> Oxsmt_core.Fabric.fixed_bounds option

(** The congruence child is never the fabric's fixed-value witness — constant [false]. *)
val fabric_verify
  :  t
  -> Oxsmt_core.Term.t
  -> string
  -> Oxsmt_core.Fabric.justification
  -> Oxsmt_core.Fabric.justification
  -> bool

val assert_fabric_eq
  :  t
  -> edge_id:Oxsmt_core.Fabric.edge_id
  -> Oxsmt_core.Term.t
  -> Oxsmt_core.Term.t
  -> unit

(** {2 Read-only e-graph query API (ADR-0012 L2 / R6, tranche 2 E-matching).}

    Thin, {b non-registering} forwards to the underlying engine's query accessors (see
    {!Oxsmt_euf.Euf}), so the lemma tier's matcher can read the congruence closure without
    growing it. The combinator exposes the congruence child's state so {!Oxsmt_interface}
    can call these. *)

val app_terms_by_symbol : t -> Oxsmt_core.Symbol.t -> Oxsmt_core.Term.t list
val find_class_opt : t -> Oxsmt_core.Term.t -> int option
val equal_if_registered : t -> Oxsmt_core.Term.t -> Oxsmt_core.Term.t -> bool
val class_members : t -> Oxsmt_core.Term.t -> Oxsmt_core.Term.t list
