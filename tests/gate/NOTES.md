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
REFUTED is always a **kernel-checked** proof of the opposite claim:

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
- Bool-sorted equality (`(= p q)` with p,q Bool) is currently rejected as
  Unsupported; add `↔` encoding if a benchmark needs it.
