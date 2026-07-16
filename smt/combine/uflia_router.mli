(** The concrete {!Combine.ROUTER} for QF_UFLIA: child [A] = EUF, child [B] = LIA
    (DESIGN.md §6). Structural routing off the frozen {!Oxsmt_core.Theory_view} dispatch —
    the same [App]-vs-[Arith]/[Le] signal EUF and LIA already split on (ADR-0003, decision
    two) — so it introduces no new classification surface.

    Ownership ([owner], register-time union):
    - [Le_zero] (order atom) → LIA.
    - [Predicate] (Bool-codomain [App]) → EUF.
    - non-Bool [Equality] over Int sides → [Both]: registered into both, so both track the
      shared Int variables and a split-decided equality can reach either.
    - [Equality] over uninterpreted-sort sides → EUF; [Bool_lit] → EUF (a folded constant,
      never a live combination driver).

    Assertion routing ([assert_to], polarity-aware) refines the [Both] case: a POSITIVE
    Int equality asserts to both (LIA takes it as two bounds, EUF merges the classes), but
    a NEGATIVE Int equality asserts to EUF ONLY — LIA's decision procedure raises
    [Unsupported] on a disequality (lia.mli), so routing [x ≠ y] to LIA would degrade
    every Int-disequality query to [unknown]. EUF handles disequality natively. For a
    shared-VARIABLE disequality this stays complete: if LIA's candidate model later equates
    [x] and [y] while EUF holds them apart, that shared-pair disagreement forces the
    ℤ-trichotomy split, whose [<]/[>] branches (ordinary [Le] atoms) carry the ordering to
    LIA. It is INCOMPLETE (OFF-path, pre-existing) for a variable-vs-CONSTANT disequality
    [x <> c]: [Combine.find_disagreement] ranges over the both-valued INTERFACE members, so a
    bare [x] used only inside EUF (not a shared interface member) paired with a literal [c]
    (not an interface node) is never formed, LIA never hears [x <> c], and its model may set
    [x = c] — a spurious point R1 rejects (→ unknown). The dark flag [OXSMT_LIA_MODEL_REPAIR]
    (combine.ml [repair_split]) closes this at Final; OFF (default) the narrowing stays
    incomplete-but-sound.

    The equality split is the ℤ-trichotomy [x = y ∨ x < y ∨ x > y] (three DISTINCT atoms,
    valid over ℤ), never the [A ∨ ¬A] tautology the SAT core would drop (ADR-0005 §1a /
    B1).

    {b Shared-sort invariant (load-bearing — why the trichotomy suffices).} Nelson–Oppen
    shares equalities over {e all} shared sorts, and the ℤ-trichotomy is an Int-only
    split. In QF_UFLIA no sort-U shared equality ever needs splitting, so this is
    complete over shared sorts (orthogonal to the OFF-path var-vs-constant interface gap
    noted above, which [OXSMT_LIA_MODEL_REPAIR] closes): the combinator's disagreement
    search ranges only over Int-sorted terms (it
    compares the terms BOTH children's candidate models value, filtered to sort Int), so
    {!equality_split} is only ever asked about Int pairs. The deeper reason it {e can}
    restrict to Int: LIA's language is Int-only, so a term is constrained by LIA (hence
    model-valued by it, hence a comparison candidate) only if it is Int-sorted; an
    uninterpreted-sorted term is private to EUF, and EUF is convex, so any uninterpreted-
    sort equality it entails is settled by EUF's own congruence — never by a cross-theory
    arrangement split. (A future non-convex theory over another sort would supply its own
    [ROUTER] with a sort-appropriate genuinely-constraining split; the generic
    {!Combine.Combine} imposes only the CONTRACT-SPLIT ≥2-distinct-atoms contract, not
    Int.) {!equality_split} guards this invariant: a non-Int pair (unreachable here)
    raises rather than building an ill-sorted order atom, degrading to [unknown] via
    CONTRACT-POISON — sound, never a wrong verdict. *)

include Combine.ROUTER
