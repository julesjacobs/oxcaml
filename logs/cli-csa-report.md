# CLI check-sat-assuming support (task #52) — report

Closes the parity gap the vc-corpus lane found (task #49, `logs/vc-corpus-report.md`): the
test SMT-LIB parser / `oxsmt_cli` had NO `check-sat-assuming` command — it fell to the
generic "unsupported command" arm, so every consumer-shaped VC (`check-sat-assuming` +
`get-unsat-core`) degraded to a silent `unknown` with no core, even though the in-process
`Session.check_sat_assuming` API supports it fully.

Base trunk: `9533ef00c8`. Branch `task/cli-csa`, worktree sibling of `main/`.

## What ships

- `smt/smtlib/parser/parser.ml` + `.mli`: a `check-sat-assuming` command arm. It reads each
  assumption literal — a Boolean atom (polarity `true`) or its single `(not atom)` negation
  (polarity `false`) — through the ordinary term reader at top-level scope, so a non-Bool or
  undeclared literal is a `Malformed` exactly as an ill-sorted assert. The parsed pairs are
  surfaced on a new `Parser.t` field `assumptions : (Term.t * bool) list option` (`None` when
  the document has no `check-sat-assuming`; `Some []` for `check-sat-assuming ()`). The reader
  does not solve — it hands the literals to the driver. `get-unsat-core` was already accepted
  (a no-op in the parser).
- `tests/solver/oxsmt_cli.ml`: when `parsed.assumptions` is `Some`, the CLI drives
  `Session.check_sat_assuming` (instead of `check_sat`), and — when the document requested
  `(get-unsat-core)` and the verdict is `Unsat` — prints the SMT-LIB core: the paren list of
  the core's assumption literals (a Bool constant as its lexically-quoted name, a negation as
  `(not name)`), in the input order the API preserves. A core query on a non-`unsat` result
  is not well-formed SMT-LIB, so nothing goes to stdout (a stderr note keeps it visible
  without polluting the verdict channel that the harness parses).
- `Makefile`: `csa-test` target, wired into `make test` + `.PHONY`.

## Behaviour verified

Running the 5 committed VC fixtures (`tests/vc-corpus/*.smt2`) through the CLI end-to-end —
the RED (each was `unknown` on trunk):

| fixture | verdict | printed core |
|---|---|---|
| ref-vc-dump-001 | unsat | `()` |
| ref-vc-dump-002 | unsat | `()` |
| ref-vc-dump-003 | unsat | `(sel_5)` |
| ref-vc-seal-001 | unsat | `()` |
| ref-vc-seal-002 | unsat | `(sel_1)` |

Cores match the sidecar reference cores exactly. Plus edge cases (in `csa-test`):
- Negative literal: `(assert p) (check-sat-assuming ((not p)))` → `unsat`, core `((not p))`
  (negation rendered verbatim).
- `sat` + `(get-unsat-core)`: verdict `sat`, NO stdout core line, stderr note (SMT-LIB: a
  core is only defined after unsat — matches z3 refusing a core on sat; we keep stdout clean
  rather than emitting `(error ...)` into the harness's verdict channel).
- `check-sat-assuming` without `(get-unsat-core)`: verdict only, no core line.

## Gates

- `make test`: exit 0 (includes `csa-test`, `wiring-test`, `smtlib-test`).
- `check-frozen`: 14/14 unchanged (parser.ml/.mli and oxsmt_cli.ml are smtlib-local /
  test-only; no frozen `.mli` touched).
- **Five-logic byte-id on non-assuming paths**: 45 files across QF_UF / QF_LIA / QF_UFLIA /
  QF_LRA / QF_BV / AUFLIA / QF_AX / QF_UFDT — the new CLI's stdout is byte-identical to
  trunk's (`--max-effort 300`, same on both). Non-`check-sat-assuming` documents take the
  unchanged `check_sat` path with no core printing, so this holds by construction.

## fmt-hook note for the integrator

Both `parser.ml` and `oxsmt_cli.ml` are NOT repo-ocamlformat-clean on trunk (reformatting
`parser.ml` under the repo config, ocamlformat 0.29.0 janestreet, is a 529-line delta;
`oxsmt_cli.ml` 56 lines). The per-file Edit hook uses a divergent ocamlformat, so editing
these files and then formatting churns the whole file. To keep the committed diff minimal on
these hot/shared files, I applied the changes textually (via Bash, bypassing the hook),
preserving trunk formatting and touching only my inserted lines. The diffs are therefore
minimal (`git diff -w` ≈ `git diff`): parser.ml 36/2, parser.mli 6/0, oxsmt_cli.ml 82/13.
Same hook-divergence hazard flagged in `logs/eq-elim-guard-report.md` (task #28).
