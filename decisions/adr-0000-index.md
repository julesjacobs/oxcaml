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
