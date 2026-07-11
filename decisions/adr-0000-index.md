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
<!-- ADR-0010 (internalization-based theory combination — each boundary node is
     its own proxy; supersedes the explicit-purification drafts) is drafted
     (logs/adr-purification-draft.md, v4) but NOT yet promoted; it promotes with
     the stage-2 combination-rebuild landing. -->

