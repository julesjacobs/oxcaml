(** Auto-trigger inference (ADR-0012 L3, tranche 3): pick a trigger for a lemma that has
    no explicit [:pattern]. Purely a completeness heuristic — a trigger only decides which
    valid instances the matcher generates, never a verdict (§3), so this is
    soundness-inert (an unreachable qvar just means the lemma does not fire → a live lemma
    degrades to a sound [unknown], never a dropped forall). *)

open Oxsmt_core

(** [infer ~qvars body] is the inferred trigger set for a lemma with binders [qvars] and
    Bool body [body], in the [triggers] shape ({!Lemma.t.triggers}: outer = alternatives,
    inner = one conjunctive multi-trigger). The recipe: the SMALLEST uninterpreted-
    function-headed ([App], arity >= 1) subterms of [body] that together cover every qvar,
    chosen greedily smallest-first (deeper, more specific patterns fire on fewer terms,
    curbing runaway matching), tag-tiebroken for determinism (I6). Arithmetic/order/
    equality/boolean nodes are never trigger roots (matcher fragment), though inference
    recurses THROUGH them to reach UF applications nested inside.

    Returns a single conjunctive multi-trigger [[ t1; ...; tk ]] wrapped as one
    alternative, or [[]] (no trigger) when some qvar occurs only outside any UF
    application (nothing can bind it) or when [qvars] is empty (a zero-qvar lemma is a
    ground fact the matcher fires without a trigger). Every returned pattern is a UF
    application, so it passes {!Lemma}/{!Matcher}'s trigger-fragment contract by
    construction.

    [inert_head hd] marks head symbol [hd] as INERT — provably unable to ever seed
    matching, so a trigger on it can never fire. Inert candidates sort AFTER non-inert
    ones (ahead of the size/tag keys), so a non-inert trigger is chosen whenever one
    covers the qvars, while the size/tag order among non-inert candidates is left
    untouched. The only current inert case is a Skolem function minted for a nested
    existential (chunk 2b): it occurs in one lemma body and can only be created by that
    lemma firing, which needs it — a deadlock. (A ground-occurrence-count key was tried
    and regressed the corpus: it also demoted user heads with no INITIAL ground occurrence
    that are legitimately created during search.) Purely a completeness heuristic — it
    changes only which valid instances fire, never a verdict. The default [fun _ -> false]
    marks nothing inert, so the order is byte-identical to the size/tag recipe (existing
    callers are unaffected). *)
val infer
  :  ?inert_head:(Symbol.t -> bool)
  -> qvars:Qvar.t array
  -> Term.t
  -> Term.t list list
