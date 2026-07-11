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
