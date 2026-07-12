# Gate encoder notes (for M5 certificate-replay and future maintainers)

This file records what the Lean oracle actually does, established by experiment
before the encoder was written (experiments live in `../logs/lean-experiments`).
Read this before touching `encoder.ml`. Lean 4.31.0, core only (no mathlib).

## grind capability findings (experiments exp1–exp8)

- **EUF congruence** (`a = b → f a = f b`): grind closes it. Uninterpreted
  sorts are plain `Type` binders, uninterpreted functions are plain function
  binders. No setup needed.
- **LIA bounds / Farkas** (`x ≥ 0 → x ≤ 0 → x = 0`, `2*x = 4 → x = 2`): closed.
  grind has a `cutsat` procedure for linear integer arithmetic.
- **Mixed EUF+LIA**: closed (`x = y+1 → y = 0 → g x = g 1`).
- **distinct**: encoded as pairwise `≠`; closed.
- **multiplication by a constant** (`2*x`): fine (linear). grind even closes
  some genuinely nonlinear goals via its `ring`/`cutsat` machinery, but the gate
  reader rejects nonlinear `*` as Unsupported anyway (QF_UFLIA is linear).
- **ite**: `if c then _ else _` needs `Decidable c`. SMT ite conditions can be
  equalities over uninterpreted sorts, which are NOT decidable in Lean. Fix:
  the unsat encoding emits `open Classical`, which puts `Classical.propDecidable`
  in scope so every `Prop` is `Decidable` and `if` elaborates. grind still
  closes goals with classical `ite` (exp8). This is sound: grind reasons
  classically regardless.
- **Booleans**: SMT Bool modelled as Lean `Prop`. `and/or/not/=>` map to
  `∧ ∨ ¬ →`. A Bool constant is a `Prop` binder; an assert of it is a hypothesis.
- **Bool-sorted `=` (iff)** and **`distinct` over Bool** (exp9, M0-gate-iff): SMT
  `=` over Bool is pairwise iff; the reader's `normalize` pass rewrites Bool
  `Eq` to a dedicated `Iff` node (chains `(= p q r)` become the pairwise `And`
  of iffs, same as other relations). Lean encoding: `Iff` → `↔`; grind closes
  iff goals, iff-chains, and iff mixed with theory atoms directly (no setup).
  `distinct` over Bool is pairwise `≠` (Ne on Prop) — grind closes it (three
  distinct Bools ⇒ unsat by pigeonhole). SAT direction: a Bool model value is
  `True`/`False` (Prop) and `↔`/`≠` over concrete Props close by `decide`.
  Iff/distinct-Bool are the only nodes touched; everything else is unchanged.

## Outcome detection

- grind **success** → exit 0, theorem accepted → **CERTIFIED**.
- grind **failure** → exit 1 with `` `grind` failed `` on stderr, and (for LIA)
  a `[cutsat] Assignment satisfying linear constraints` block. We do NOT parse
  that block for the verdict; we only use it as a hint. Classified
  **INCONCLUSIVE** unless a witness refutes (below).
- Lean **elaboration error** (type mismatch, unknown identifier, parse) → exit 1
  with a different message → **ENCODE_ERROR** (a bug in our encoder, loud).
- We give each query its own `.lean` file and its own process. Exit 0 with no
  diagnostics = certified; anything else is classified from stderr.

## REFUTED (kernel-checked, no diagnostic parsing)

grind does **not** synthesize existential witnesses (exp4: `∃ x, x ≥ 0 ∧ x ≤ 5`
fails), so REFUTED cannot come from asking grind to prove satisfiability. Instead
REFUTED is always a **kernel-checked** proof of the opposite claim. The witness
refutation deliberately uses `decide` **only** — never the compiler-trusted
`native_decide` that the *certify* path may escalate to — so a compiler /
`Lean.ofReduceBool` miscompile can never manufacture a false ship-stopper
(ADR-0006 Decision 3, review AP4):

- claimed **unsat** but a witness model is supplied → run the **sat encoder** on
  that model; if `decide` closes `⋀ assertions`, the query is satisfiable, so the
  unsat claim is REFUTED (ship-stopping).
- claimed **sat** → after the model `decide` check, also try the **unsat**
  encoding (`⋀ assertions → False` by grind); if grind proves `False`, the query
  is actually unsat, so the sat claim is REFUTED.

So both directions run a *primary* attempt and a *refutation* attempt; REFUTED is
never a heuristic. INCONCLUSIVE is the only soft outcome. There is no path from a
satisfiable query to CERTIFIED-unsat, which is the property honeypots audit.

## SAT / model encoding

- `decide` closes ground goals (exp6). Uninterpreted sorts become `Fin n` where
  `n` is the model-supplied cardinality; sort/const/function definitions use
  `abbrev` (NOT `def`: a `def` abbreviation hides the `OfNat`/`Decidable`
  instances and `decide` fails — exp6).
- Uninterpreted functions become total `abbrev` lambdas: nested
  `if arg = case then val else … else default`. Every function needs a default.
- `native_decide` closes larger arithmetic (exp6) but adds the compiler to the
  trusted base; the runner tries `decide` first and falls back to
  `native_decide` only if `decide` fails.

## Timeout

grind self-terminates quickly even on hard nonlinear goals (exp7, ~0.6s), but the
runner still imposes a wall-clock cap (default 30s) by spawning lean under its own
watchdog (`Unix.create_process` + polled `waitpid` + `kill`). The `/usr/bin/timeout`
binary exists but the runner does not depend on it.

## Cache-key injectivity (a review REJECT — fixed)

The cache is a soundness component: if two semantically-different queries hash to
the same key, one query silently inherits the other's kernel-checked verdict. The
first canonical form concatenated raw symbol names with space/newline/paren
separators; because a `|quoted symbol|` may contain any byte except `|`
(including those separators), an unsat query and a satisfiable query were made to
produce identical canonical bytes → same key → the sat query got CERTIFIED off
the unsat query's proof (exhibits in `tests/gate/collision/qA,qB.smt2`). Fix:
`canonical.ml` now serialises a tagged tree with a self-delimiting netstring
encoding — each node is `A<len>:<bytes>` (atom) or `L<count>:<subnodes>` (list),
so payload bytes are read by length and no separator can be forged; the encoding
is invertible, hence injective (argument in the file header). `gate selftest`
embeds qA/qB and asserts their canonical strings and cache keys differ, plus a
`ser` self-delimiting unit. Bump `encoding_version` on any canonical-form change
too — old keys are otherwise stale (it is folded into the key).

## Open questions / deferred

- **Cache canonicalization does not rename symbols in v1** (see `canonical.ml`).
  Consistent renaming of uninterpreted symbols is verdict-preserving and would
  raise the hit rate for isomorphic-but-differently-named queries, but it is a
  collision-bug risk in the trust-critical path, so it is deferred. The dominant
  benefit (never re-running Lean on a byte-identical or reformatted file) is
  already captured by operand/assertion sorting + canonical printing.
## encoding_version and iff (M0-gate-iff)

`encoding_version` stays `enc-v1` after adding iff support. The bump rule is:
bump iff previously-supported queries emit *different* Lean. Iff is purely NEW
coverage — Bool-sorted `=` used to be rejected UNSUPPORTED (never certified,
never cached), so no stale entry can exist for it. Every previously-supported
query contains no Bool `=`, so `normalize` leaves it unchanged and the encoder
and canonical form emit byte-identical output (only an `Iff` branch was added
alongside the untouched `Eq` branch). Hence old cache entries remain valid and a
bump would needlessly discard them.

## Reader hardening (codex G1–G4, enc-v2)

A cross-model (gpt-5.6) review found four holes in the gate's own SMT-LIB
reader/lexer — the TCB — that the same-model reviews had cleared. Fixed on
task/gate3; details + retroactive re-certification in logs/gate3-recertification.md.

- **G1 (quoted tokens):** `sexp.ml` now distinguishes token KIND — `Atom`
  (unquoted: may be numeral/keyword/operator), `Quoted` (`|...|`: ALWAYS a plain
  symbol), `Str` (`"..."`: inert data). Previously a quoted `|0|` lexed as the
  numeral 0, so `(distinct |0| 0)` collapsed to `(distinct 0 0)` → grind certified
  `0≠0`=False (a false unsat). Now `|0|` is the symbol "0"; the reader never
  numeral-/keyword-interprets a `Quoted`.
- **G2 (string literals):** the lexer reads `"..."` (with `""`→`"`) as one inert
  `Str` token; its bytes are never re-tokenized as commands, so a
  `:source "(assert false)"` cannot inject `false` into the theorem.
- **G3 (single-query model):** `check-sat` was a no-op, so asserts anywhere in the
  file accumulated into one theorem. The reader now rejects a second `check-sat`
  and any `assert` after a `check-sat` as UNSUPPORTED (loud) — no silent union.
- **G4 (div/mod):** grind does NOT reason about Lean's Euclidean `Int.ediv`/
  `Int.emod` — verified by experiment (exp10): it treats them as opaque and cutsat
  ignores the div/mod ↔ dividend link, so `(mod x 3)=5` etc. are NOT closed.
  Emitting `ediv`/`emod` would only ever yield INCONCLUSIVE. So the reader
  recognises `div`/`mod` and classifies them a distinct, LOUD UNSUPPORTED (not a
  silent MALFORMED-green bypass). Real support needs euclidean elimination (fresh
  q,r + `x = c·q+r ∧ 0≤r<|c|` side constraints, as smt/preprocess does) — a
  separate TCB feature RECOMMENDED before M4 LIA, when div/mod cases arrive.

**encoding_version enc-v1 → enc-v2 (unconditional):** the emitted Lean is
unchanged, but every verdict cached under enc-v1 was computed through the broken
reader, so the bump forcibly invalidates the whole certified cache and forces
re-certification through the fixed reader. Re-cert found zero regressions (our
printer-emitted corpus never exercised the holes).

## Gate accounting invariant (author directive, #119)

Every input query exits `gate run` in EXACTLY ONE class: **certified** /
**inconclusive**(-with-reason) / **quarantined**(-with-reason), plus the two RED
terminal classes **refuted** (soundness breach) and **encode_error** (encoder
bug). The digest prints the sum identity — `inputs = certified + inconclusive +
quarantined + refuted + encode_error` — and the gate is RED if it does not close
(a query silently dropped). MALFORMED / UNSUPPORTED / NO_STATUS are *quarantine*
reasons: visibly counted AND listed per-file with their reason, never dropped.
This generalises G4 (and the honeypot floor) — silent bypass is now structurally
impossible (an unaccounted or reader-rejected-but-claimed query is loud), not
patched per-bug. Quarantine is not RED (it is a coverage gap, not a soundness
failure); only refuted/encode_error/honeypot-breach/accounting-mismatch are RED.

## Ruling: quarantine-is-green is an accepted asymmetry (master, codex round-2 (c))

Recorded as a decision, not an accident. Quarantine (MALFORMED / UNSUPPORTED /
NO_STATUS) leaves the gate GREEN. This is the same accepted asymmetry as
INCONCLUSIVE: **nothing can be CERTIFIED through quarantine**, so no unsound
verdict can be laundered — the only cost is oracle COVERAGE, and that cost stays
LOUD via the per-reason accounting counts + the per-file QUARANTINED listing (a
query the gate can't judge is never silent). Making quarantine RED would instead
punish legitimately-out-of-subset corpus inputs and pressure toward test-gaming.
The REAL close of the div/mod (and abs) coverage gap is `gate-divmod-elim` (filed
follow-up, scheduled before M4 LIA cases exercise div/mod) — encoder-side
euclidean/ite elimination that turns those quarantines into CERTIFIED/REFUTED.

Coverage-metric flow: quarantine counts appear in the gate digest today; they
should also flow to STATUS.md as a coverage metric. If `status_gen` does not yet
pick them up, that is the `status_gen` task's concern (#133), NOT fixed here.

## Reader-vs-execution divergence (codex round-3, MERGE-BLOCKING)

Round-2's exactly-one-check-sat guard was incomplete. Two commands the reader
matched-and-ignored diverged from SMT-LIB *execution* semantics, and both let a
file assemble a query a conformant solver would never execute — the G1 laundering
class (effective query ≠ what the file states):

- **`(check-sat X)` junk args.** `(check-sat)` takes NO arguments. The old arm
  `List (Atom "check-sat" :: _)` matched a junk-arg check-sat and set `checked`,
  so `(set-logic QF_LIA)(set-info :status unsat)(assert false)(check-sat X)` —
  which contains ZERO valid check-sat commands — folded `false` into a theorem and
  the gate CERTIFIED a false unsat. Fix: match exactly `List [Atom "check-sat"]`;
  `(check-sat ...)` is a hard REJECT (MALFORMED), flowing into the accounting
  quarantine bucket like any other malformed input.

- **Commands after `(exit)`.** SMT-LIB execution TERMINATES at `(exit)`. The old
  arm ignored exit and kept folding, so
  `(set-logic QF_LIA)(set-info :status unsat)(exit)(assert false)(check-sat)`
  assembled a query AFTER the query had ended and the gate CERTIFIED a false
  unsat. Fix: `(exit)` arms an `exited` flag; any later command is a hard REJECT
  (MALFORMED) — NOT a silent truncation of the ignored tail, which is the same
  divergence class. `(exit)` as the final command is fine. `(exit ...)` with args
  is likewise rejected (exit takes no args).

Both fixes fail closed. Discrimination is empirically verified: built the gate at
the pre-round-3 reader (gate3 `eb3350e`) in a scratch worktree — it CERTIFIES both
inputs (false unsat); the fixed reader rejects both MALFORMED. Permanent
regressions: `honeypot_checksat_args_trap` and `honeypot_post_exit_trap` (both
`.expect MALFORMED`, floor 9→11) plus two Lean-free reader-reject assertions in
`gate selftest` (`check-sat-args reject`, `post-exit reject`), all on codex's
verbatim triggers. The 5-term accounting identity still closes (21 = 21 certified).

Calibration (ADR-0007 §3, appended to logs/gate-review.md): round-2's "no-op
audit" checked classification OUTCOMES but not COMMAND HANDLERS. Extended checklist
item — every command the reader matches-and-ignores must be justified against
SMT-LIB *execution* semantics: what would a conformant solver DO here, and does
ignoring it change the effective query the gate certifies?

## div/mod/abs support — euclidean elimination (gate-divmod, enc-v3)

Closes the div/mod (+abs) quarantine gap the round-3 NOTES flagged as the real fix
(previously div/mod/abs were loud UNSUPPORTED). Two directions, because grind and
decide have opposite strengths on Euclidean division:

- **UNSAT (grind)**: [Encoder.encode_unsat] runs [Elim.eliminate], which replaces
  each [(div x d)]/[(mod x d)] (nonzero integer-literal d) with fresh nullary
  [.oxsmt.q.n]/[.oxsmt.r.n] plus `x = d*q + r ∧ 0 <= r < |d|`, returning q/r. grind
  then sees pure linear arithmetic (verified exp: grind proves the eliminated
  `(mod x 4) >= 4 → False`, `4*(div x 4) >= x+1 → False`, mod-periodicity). This is
  EXACTLY smt/preprocess's [div_mod_elimination] rewrite (preprocess.ml:118) — same
  `rhs = d*q + r`, `0 <= r`, `r < |d|`. Divergences, both sound: the gate uses
  arbitrary-precision numeral strings (no min_int |d| overflow, unlike preprocess's
  native-int ceiling), and does NOT memoise one witness per (dividend, divisor)
  (per-occurrence witnesses are equivalent — euclidean q,r are unique given x,d).
- **SAT (decide)**: [Encoder.enc] emits [Int.ediv]/[Int.emod] directly; decide
  computes them on the model-substituted closed term. SMT-LIB div/mod are Euclidean =
  Lean [Int.ediv]/[Int.emod] (verified exp incl. negatives: `emod (-1) 3 = 2`,
  `ediv (-1) 3 = -1`, the euclidean identity, and `0 <= emod < |d|`). This is why the
  refute honeypot ((mod x 3)=1 mislabeled unsat, witness x:=1) goes REFUTED not
  INCONCLUSIVE — the sat path proves the assertion under the model by decide.

- **abs**: desugared in the reader to `ite(x>=0, x, -x)`, EXACTLY smt/core's
  [Context.abs] (context.mli:51). Both directions already handle ite (grind via
  Classical / decide), so no fresh var and no dual path.

**Divisor restriction = fail closed (matches the solver's theory).** [Elim.check_divisors]
is the single authoritative preflight (run in [certify_file] before either encoding):
a ZERO divisor (SMT-LIB leaves div/mod-by-zero unconstrained; preprocess rejects it —
note Lean's `Int.emod x 0 = x` would otherwise let the sat path "compute" a value the
solver never sanctions) and a NON-LITERAL / variable divisor (v1 is linear) both stay
UNSUPPORTED. Known coverage gap (not a soundness issue): the solver's core folds a
constant divisor like `(+ 1 2)` to a literal and accepts it; the gate does NOT
constant-fold, so it quarantines such a divisor. The gate is strictly MORE conservative
— it never certifies a divisor the solver rejects — so the asymmetry is a completeness
gap, filed for a future fold-then-check refinement, never a false certification.

**Fresh-name capture (#127 for the gate).** [Elim]'s fresh symbols live in the reserved
[.oxsmt.] namespace; [Reader] now rejects any user declaration OR let-binding in that
namespace ([is_reserved_name]), so a crafted `(declare-const |.oxsmt.q.0| Int)` cannot
alias a euclidean witness. This is the gate's independent instance of the #127 guard.

**Accounting.** Eliminated-and-certified div/mod cases move quarantined → certified; the
5-term identity is unchanged (they are in the certified count), and the digest prints a
distinct `divmod-eliminated: N case(s) certified` line so the movement is visible.

**Soundness spot-check.** A satisfiable div/mod query mislabeled unsat with NO witness
model returns INCONCLUSIVE (grind correctly fails to prove False), never CERTIFIED.

## Cache-entry integrity + the honest residual (tokenizer-gate round 3)

The migrated cache reader (`cache.ml`) validates an entry's *identity* — schema (exactly
the written fields, no missing/extra/duplicate) and `key`/`query-hash`/`claim`/`encoding-version`/
`grind-config` against the requested key. But identity binds the entry to its INPUTS, not to
its RESULT: a codex round-3 pass showed that flipping `(outcome REFUTED)` → `(outcome
CERTIFIED)` in an otherwise-valid entry was trusted, dropping a ship-stopper while the gate
stayed GREEN. This is exactly the §10 test-gaming vector: deleting a REFUTED entry self-heals
(the case re-certifies and is re-refuted), but a *flip* would stick.

**Fix (proportionate):** `store` writes an `integrity` field = SHA-256 over the content-field
values; `lookup` recomputes it and rejects a mismatch as `Unreadable` (→ Lean re-runs). This
binds the outcome (and detail) to the entry, so a flipped/corrupted result whose digest was
not recomputed fails validation. Kills accidental corruption and NAIVE tampering. Selftests:
store a REFUTED entry, flip only the on-disk outcome tag → `Unreadable`; and the mirrored
CERTIFIED→REFUTED direction.

**Residual, stated honestly — this is NOT a full authenticity guarantee.** With no secret
available in the TCB, a keyless digest cannot stop a *determined same-UID adversary* who edits
a field AND recomputes the digest. An in-file MAC would need an embedded "secret", which is
security theater (a reviewer should reject it) — the secret would sit in the same repo/dir the
adversary already controls. So the integrity field is a corruption/naive-tamper guard, not a
cryptographic authenticator. The systemic backstops for the residual are:

1. **Documented trust assumption.** The cache directory is TRUSTED local state, on the same
   footing as the source tree and the build outputs: an actor who can rewrite cache entries
   under our UID can equally rewrite `encoder.ml` or the Lean sources. The gate's soundness
   argument assumes an untampered local checkout; the cache is inside that boundary.
2. **Nightly cache-audit intent (documented intent only — no scheduler exists yet).** Alongside
   the existing nightly-intent rows (mutation testing, the honeypot floor), add a nightly
   **cache audit**: sample N random CERTIFIED cache hits, re-run the Lean oracle on them with
   the cache disabled, and ALARM on any hit whose fresh verdict differs from the cached one.
   This catches a determined tamper (and any latent cache/oracle drift) out-of-band, without
   putting a fake secret in the repo. Not implemented here (like the other nightly rows, this
   records intent; wiring it needs the nightly scheduler that does not yet exist).
