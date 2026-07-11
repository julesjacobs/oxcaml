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
    set contains only literals the engine actually asserted. A [Le_zero] atom belongs to
    LIA and is rejected ([Invalid_argument]).

    {b Soundness discipline (DESIGN §7, I4).} The underlying engine self-checks every
    explanation it produces (independent naive-closure replay). This adapter adds no new
    reasoning; it only relabels premise tokens.

    {b Determinism (I6, ADR-0005 C1–C8).} All internal maps ([Atom]/[Term]-keyed) are used
    for lookup only, never iterated into an observable output; propagation and explanation
    order come straight from the deterministic engine. Owning task: TASKS.md M4-adapters
    (EUF). *)

include Oxsmt_core.Theory.THEORY
