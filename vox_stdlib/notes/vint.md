# Vint — language/compiler needs (blueprint §5 format)

Module: reflected `int` ops (min/max/abs). Interface-only `[%%vox.lean]`
block; `.ml` is pure OCaml bodies bound by `[@@vox.reflect]`. The assumed-axiom
ledger is empty, but this is NOT zero trust: the reflect body↔def
correspondence is a trusted (audited, not machine-checked) surface — see the
first note below.

Law inventory (11, all proved `public theorem`, zero axioms): vi_min_comm,
vi_min_idem, vi_min_le_left, vi_min_le_right, vi_min_cases; vi_max_comm,
vi_max_idem, vi_max_ge_left, vi_max_ge_right, vi_max_cases; vi_abs_nonneg.
The two `*_cases` laws (`vi_max a b = a ∨ vi_max a b = b`) were added in the
Phase-C fix wave to discharge clamp-style COMBINED bounds — see the combining
-bound note below for why the originally-requested conditional form could not
be used.

### Vint · "zero trust" overclaims — the reflect body↔def correspondence is trusted, not checked
- **site:** vox_stdlib/Vint.mli:1, vox_stdlib/Vint.ml:1 (header comments, reworded)
- **milestone/gap:** new (soundness-review honesty item; no op change)
- **what I tried:** original headers said "ZERO-TRUST" / "so nothing is assumed on the Lean side".
- **error:** the soundness reviewer probed a DIVERGENT reflect body (`imin = a+b` bound to `vi_min`): it silently verifies and then proves runtime-false facts. `[@@vox.reflect]` does not check that the OCaml body implements the Lean def — that correspondence is a TRUST SURFACE.
- **workaround used:** reworded both headers to "the assumed-axiom ledger is empty, which is NOT zero trust: the trust surface is the inspectable body↔def correspondence, audited character-for-character (not machine-checked)". The shipped bodies do match; only the label was dishonest.
- **removed by:** a compiler check that the reflected OCaml body is definitionally the reflected Lean def (would move reflect from trusted to checked) — the Phase-3 declaration-at-a-distance reflect work is the natural place.
- **severity:** SHOULD-FIX (honesty; the code is correct, the claim was too strong).

### Vint · vi_abs_nonneg is runtime-false at min_int — honest only under the unbounded-Int model
- **site:** vox_stdlib/Vint.mli (`vi_abs_nonneg : 0 <= vi_abs x`), vox_stdlib/Vint.ml (`iabs`)
- **milestone/gap:** new (bounded-int modelling gap; being studied separately)
- **what I tried:** ship `vi_abs_nonneg` as a general law `0 <= vi_abs x`.
- **error:** at runtime `iabs min_int` wraps (`-min_int` overflows back to `min_int < 0`), so the law is FALSE for machine ints; the reviewer proved `iabs x >= 0` for all x from the shipped law. vox's model types OCaml `int` as unbounded `Int`, which hides the wrap.
- **workaround used:** caveat documented in both file headers and here; no code change (the law is correct under the model vox actually uses). The systemic fix (bounded-int / machine-int model) is a separate study.
- **removed by:** a bounded/machine-int model for OCaml `int` in vox (then `vi_abs_nonneg` would correctly require `x <> min_int`, or `iabs` would be specced with the wrap).
- **severity:** SHOULD-FIX (latent: any client relying on `vi_abs_nonneg` at an unclamped input is proving something machine-false).

### Vint · @[grind] on a non-equational theorem is silently inert without a grind_pattern
- **site:** vox_stdlib/Vint.mli (the five bound/nonneg laws: vi_min_le_left, vi_min_le_right, vi_max_ge_left, vi_max_ge_right, vi_abs_nonneg)
- **milestone/gap:** M3 (adjacent) / new
- **what I tried:** ship `@[grind] public theorem vi_min_le_left (x y) : vi_min x y <= x := by grind [vi_min]` with no `grind_pattern`, exactly the way the equality laws (comm/idem) are shipped.
- **error:** at the CLIENT (smoke_vint), goal `vi_min x y <= x`, `Hypotheses: <none>`, `vox: verification failed -- NOT PROVED ... (lean: error: grind failed)`. The reported goal is *literally the shipped theorem's statement*, yet grind never fires it.
- **workaround used:** attach `grind_pattern vi_min_le_left => vi_min x y` (and one per bound law). Equality-conclusion theorems (`vi_min_comm`, `vi_min_idem`) fire from bare `@[grind]` and need no pattern; only the inequality/`<=`-conclusion theorems do. `by grind [vi_min]` still proves each theorem *in the .mli*; the gap is purely trigger-generation for downstream use.
- **removed by:** `@[grind]` deriving a usable E-matching trigger from an inequality conclusion (the head application `vi_min x y`) the way it does for an equality's LHS — or a lint that an attributed theorem produced no trigger (the M3 "silently inert" family, generalized from axioms to `<=`-theorems).
- **severity:** MAJOR-ERGONOMIC (silent: the law compiles, is `@[grind]`, and simply never fires; only a forcing client reveals it).

### Vint · no model-dup and no M1 tax (positive evidence — reflect + inline exception)
- **site:** vox_stdlib/Vint.mli block; vox_stdlib/Vint.ml (no block at all)
- **milestone/gap:** model-dup / M1 (both ABSENT here)
- **what I tried:** the pre-seeded expectation (§5) is one model-dup note and one M1 note per module. Neither occurs for a reflect module: the model theory lives in exactly ONE block (the `.mli`), and the laws are inline `public theorem`s under the §4 inline exception, so no `.mli`-axiom/`.ml`-theorem statement is typed twice.
- **error:** n/a.
- **workaround used:** n/a — this is the payoff of the reflect shape. Recorded so Phase C does not expect a model-dup/M1 entry from Vint.
- **removed by:** n/a.
- **severity:** COSMETIC (informational).

### Vint · zero-trust requires NOT exposing the model defs (dead-law vs the reflectbits template)
- **site:** vox_stdlib/Vint.mli:14-16 (`public def vi_min/vi_max/vi_abs`, deliberately no `expose`)
- **milestone/gap:** new (design guidance, not a compiler bug)
- **what I tried:** the graduation template `demo/reflectbits.mli` marks its def `@[grind, expose] public def bmin`. Copied verbatim (exposed) first.
- **error:** no compile error — but with the defs exposed, a client's grind UNFOLDS `vi_min` and proves every algebra goal directly from the `if..then..else`, so every shipped law becomes derivable-by-unfolding, i.e. DEAD by the §6.7 removal test (removing any law leaves its smoke goal passing).
- **workaround used:** drop `expose` (keep `public`): clients may still name `vi_min` in refinements (the reflect result `{ _ = vi_min x y }` type-checks) but grind treats it opaquely and must use the shipped `@[grind]` laws. Confirmed: with defs unexposed, `vi_min_comm`/`vi_min_idem`/`vi_max_comm`/`vi_max_idem`/`vi_abs_nonneg` are each strictly load-bearing (removal breaks the smoke goal).
- **removed by:** nothing needed — this is the correct setting for a laws-are-the-interface reflect module. Flagged so the template's `expose` is not cargo-culted into a zero-trust stdlib module.
- **severity:** MINOR (guidance).

### Vint · conditional combining bound (`vi_max a b <= c` given `a<=c ∧ b<=c`) cannot fire — free arithmetic variable is uncoverable by any grind pattern
- **site:** vox_stdlib/Vint.mli (the combining-bound laws requested by the Phase-C clamp reviewer)
- **milestone/gap:** new (generalizes the M3 "silently inert attributed theorem" family)
- **what I tried:** the reviewer's clamp `imax lo (imin hi x)` couldn't prove its upper half `<= hi`. Natural fix = ship `vi_max_le (a b c) (a<=c) (b<=c) : vi_max a b <= c` and `vi_le_min` dually, per the team lead's request. Tried every pattern form: bare `@[grind]`; `@[grind →]`; `grind_pattern … => vi_max a b`; conclusion pattern `… => vi_max a b <= c`; multipatterns `… => vi_max a b, a <= c` and `… => vi_max a b, a <= c, b <= c`.
- **error:** conclusion pattern (`vi_max a b <= c`) is REJECTED at seal (Lean: pattern must cover all vars — but after grind negates the goal the `<=` atom is not in the E-graph anyway). Every other form SEALS but is silently inert: even the minimal direct goal `vi_max a b <= c` under `a<=c, b<=c` fails `NOT PROVED / grind failed`. Root cause: the extra variable `c` occurs ONLY inside `<=` comparisons, and grind's E-matcher indexes function-APPLICATION terms (`vi_max a b`, `all_lt t b`), not `≤`/`<` atoms — so no trigger can bind `c`. This is exactly why `lib/bst.mli`'s `not_mem_lt` binds its extra var through the def-application `all_lt t b`, never through its `b <= x` hypothesis.
- **workaround used:** ship the DISJUNCTION/cases law instead — `vi_max_cases : vi_max a b = a ∨ vi_max a b = b` with pattern `vi_max a b` (no free var, fires cleanly). grind then derives ANY bound (upper for clamp, and lower) by case analysis; it strictly SUBSUMES the requested conditional `vi_max_le`/`vi_le_min` (a client goal `_ <= c` under `a<=c, b<=c` discharges by cases). Verified: both clamp orientations (`imax lo (imin hi x)` and `imin hi (imax lo x)`, each with the combined `[lo,hi]` bound) now pass; both cases laws are strictly load-bearing (removal breaks the clamp).
- **removed by:** either (a) grind indexing/allowing `≤`/`<` atoms as pattern triggers so conditional arithmetic bounds can fire, or (b) documenting the cases-law idiom as the canonical way to expose an op's bounding algebra (probably the better answer — the cases law is more general than the conditional bound).
- **severity:** MAJOR-ERGONOMIC (the intuitive law shape is un-shippable; only a probe sweep + the bst precedent reveal the working idiom).

### Vint · dead-law removal test yields false positives on inter-derivable algebra
- **site:** vox_stdlib/clients/smoke_vint.ml (bound laws)
- **milestone/gap:** new (reviewer-methodology note for Phase C item 7)
- **what I tried:** the §6.7 "delete the law, confirm the goal fails" test as a dead-law detector on the bound laws.
- **error:** false positive — deleting `vi_min_le_right` leaves its smoke goal PASSING, because `vi_min_comm` + `vi_min_le_left` ⊢ `vi_min x y <= y`. But BOTH `le_left` and `le_right` are §3-MANDATED, so §3's own required min set contains mutually-derivable members; the removal test would fail §3 itself. Same for the (added) symmetric max bounds under `vi_max_comm`.
- **workaround used:** read item 7 as "every law has a consuming goal (no orphan)", which holds — smoke_vint has one goal per law. `comm`/`idem` are the strictly-independent core; the paired bounds are deliberate ergonomic completeness mirroring §3.
- **removed by:** n/a — check-semantics clarification, not a language change.
- **severity:** COSMETIC (informational).
