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

    [ground_occurrences hd] is how many times head symbol [hd] occurs in a ground term of
    the current problem (see {!ground_head_counts}); candidates are ordered by it
    DESCENDING before the size/tag keys, so a head that can actually match a ground term
    is preferred and a head with no ground occurrence (e.g. a Skolem function minted for a
    nested existential, which never appears in a ground term) sorts last. Purely a
    completeness heuristic — it changes only which valid instances fire, never a verdict.
    The default [fun _ -> 0] makes the key inert, so the order is byte-identical to the
    size/tag recipe (existing callers are unaffected). *)
val infer
  :  ?ground_occurrences:(Symbol.t -> int)
  -> qvars:Qvar.t array
  -> Term.t
  -> Term.t list list

(** [ground_head_counts terms] returns [fun hd -> count], the number of
    [App]-with-arguments nodes headed by [hd] across [terms] (typically a query's ground
    assertions). Intended as the [~ground_occurrences] argument to {!infer}. *)
val ground_head_counts : Term.t list -> Symbol.t -> int
