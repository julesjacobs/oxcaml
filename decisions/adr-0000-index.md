# Decision log — index

Append-only. One line per ADR; read full entries on demand. Re-opening a logged
decision requires the adversarial-review ritual (DESIGN.md §10, §11).

- ADR-0001 — Host repo: develop in a fresh local git repo on branch `oxsmt`,
  not a clone of the oxcaml fork. [accepted]
- ADR-0002 — Toolchain: stock OCaml 5.4.0 (portable subset, no OxCaml-only
  features yet); Lean 4.31.0 via nix is the oracle. [accepted]
- ADR-0003 — Term/sort representation: field layout + constructor set for the
  frozen core. [accepted (frozen)]
- ADR-0004 — Task branch naming: `oxsmt/task/<name>` collides with the `oxsmt`
  trunk ref in git; amended to `task/<name>`. [accepted]
- ADR-0005 — THEORY plugin interface: the frozen plugin signature (EUF/LIA/N-O),
  `Atom`/`Lit`/`Explanation` currency, `check(effort)`/lazy `explain`, `Split`
  disjunctions, engine `CONTRACT-POISON`. [accepted (Tranche-A frozen)];
  freeze schedule in `adr-0005-freeze-plan.md`.
- ADR-0006 — Unsat certificates + Lean replay (M5 pulled forward): off-core
  `smt/certificate/` module, payload-free `Rule_tag`, `sat.mli` DRAT hooks bundled
  into the ADR-0005 Tranche-C freeze. [accepted (design; implementation post-M4)]
- ADR-0007 — Cross-model review is blocking for TCB-path merges (gate reader,
  shipped printer, `check_frozen`/`FROZEN.sha256` mechanics, canonicalization/
  cache-key code); a codex (gpt-5.6) pass in addition to same-model adversarial
  review, after a cross-model retrospective caught four gate-reader holes
  same-model review had cleared. [accepted]
- ADR-0008 — Shared SMT-LIB lexer (`smt/lexical`): one boundary-correct lexer
  (SMT-LIB §3.1, headline invariant "token kind is never lost") replacing three
  divergent hand-rolled lexers; the shipped printer, the smtlib parser, and the
  gate reader all link it. Breaks the gate's zero-`smt/`-deps posture at the
  lexical layer only (encoder + Lean kernel independence intact), backstopped by
  the fuzzer cross-impl differential, Lean elaboration, and pre-labeled
  benchmarks. [accepted]
- ADR-0009 — Async review pipelining: PR-branch model; reviews and test runs run
  independently against pinned shas, parallel and speculative; the integrator
  pre-rebases/pre-tests queued branches; trivial-rebase exception (formatting-only
  or disjoint hunks → carry verdicts forward + fast suite; semantic overlap /
  touched reviewed hunks / conflict resolutions → full re-test + scoped
  re-review). The DESIGN §11 linear-trunk / every-commit-green-at-its-landed-sha
  invariant is unchanged. [accepted]
- ADR-0010 — Internalization-based theory combination: rebuild `smt/combine` so
  each boundary term NODE is its own proxy (ownership by head symbol; no fresh
  `.oxsmt.*` proxy symbols, no defining equations, no preprocess purify pass). A
  total structural interface walk makes sharedness total by construction — killing
  the W1/R1 wrong-SAT family (too-small approximation) and the fresh-sum
  non-termination (too-large one); Bool-under-UF splits into native-constant /
  leaf-bridge / degrade-to-`Incomplete` cases (§3.6). The CDCL(T) seam and the
  frozen `THEORY` interface are KEPT (no e-graph hub); EUF/LIA adapters unchanged.
  Supersedes the v1–v3 explicit-purification drafts. Full text (with errata: §5a
  mechanism, §3.6 surfaced-vs-buried + option-(a) infeasibility, §6 fixture trio,
  ite precondition) in `adr-0010-internalization.md`. [accepted]
- ADR-0011 — Uninterpreted-function-table models (the QF_UF model gap): read out a
  verified `sat` function/predicate TABLE model (default + cases, first-match) from
  the combined `Cdclt` output — freeze-fork (b), unfrozen `Cdclt` currency, no
  frozen-surface change. Every promoted `sat` gated through `Model_check` (uniform
  gating); QF_UF first cut (mixed-sort QF_UFLIA Int-keyed tables deferred, §10 stub).
  The QF_UF solved-rate lever (7,269/7,503 files need tables). Implemented + landed
  on `task/uf-models` (trunk `e41b126672`); full text in `adr-0011-uf-models.md`.
  [accepted — this land is its acceptance]
- ADR-0012 — Lemma tier (stage-2 quantifier instantiation): L1 normative (trigger-based
  e-matching instantiation, placeholders + scoped `env.mli` unfreeze, unforgeable
  `.oxsmt.*` capability, `assert_lemma` binder-builder, nested-∀ rejected); L2–L6
  interface sketches. Design-only, gated behind M4 close + the freeze rituals; full
  text in `adr-0012-lemma-tier.md`. [ratified Rev 3.2 — design-only]
- ADR-0013 — Certificates + Lean replay, end-to-end (M5, elaborates ADR-0006): the
  resolution-skeleton + reflected-checker replay path — emission points (`on_learned`),
  the four in-search `R_unsat` terminal exits (§4.0, incl. E3 `Theory_prop` materialization
  + E4 Final-effort `T_lemma`), EUF explicit proof terms, LIA `omega` M5 bootstrap → reflected
  Farkas multiplier checker target (D3 erratum), the A5 accounting identity. Rev 5 folds the
  codex Rev-4 final leg (3 HIGH + 1 MEDIUM). Design-only; full text in
  `adr-0013-certificates-replay.md`. The D3 erratum was applied to `adr-0006-certificates.md`
  as its Revision 5 at this promotion. [ratified Rev 5 — design-only]

