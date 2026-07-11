# Mutant registry

Seeded faults for mutation testing (DESIGN.md §10). Each mutant is a deliberate
bug the tiered suite MUST catch; running them proves the suite can actually go
red. A surviving mutant is a real oracle gap — a signal to strengthen the test,
never to weaken the mutant.

Run via `make mutants` (full) or `make mutants MODULE=<core|sat|preprocess|gate|smtlib>`.
The runner (`tools/mutants/run.sh`) applies each patch in a throwaway git
worktree off HEAD, runs the mutant's declared suite, and requires a red exit.

## Layout: one mutant = two files

- `<name>.patch` — a `git apply`-able unified diff against the CURRENT trunk
  content of one source file (the fault).
- `<name>.meta` — `key=value` sidecar:
  - `module` — one of core / sat / preprocess / gate / smtlib (the `MODULE` filter).
  - `suite` — the make target expected to kill it (`core-test`, `sat-test`,
    `preprocess-test`, `gate`, `smtlib-test`).
  - `target` — the source file the patch mutates.
  - `description` — what the fault is and why it is unsound.
  - `review` — the review / invariant / ADR that motivates it.
  - `expected` — the intended kill class: `KILLED-TEST` (preferred — an oracle
    catches it) or `KILLED-BUILD` (only the compiler catches it). Documentation
    only; the runner reports the actual class, it does not enforce this field. A
    mutant meant to survive is not a mutant.

## Outcomes

The runner builds each mutant FIRST (`make build` = `dune build @@default`,
which compiles the mutated library and every suite executable), then runs the
declared suite. That split classifies a kill by WHO caught it:

- **KILLED-TEST** — the build succeeded and the suite then went red. A test
  ORACLE caught the fault. This is the meaningful kill: it certifies the suite.
- **KILLED-BUILD** — the mutant did not compile, so the type-checker caught it
  before any oracle ran. Still a kill, but it certifies the COMPILER, not the
  registry's oracle. Prefer a *compiling* variant (one that type-checks but is
  semantically wrong) so an actual oracle is exercised — e.g. empty a list or
  flip a constant rather than delete a binding (which usually fails to compile).
  The runner prints an advisory when any KILLED-BUILD is present, so the table
  makes registry quality auditable at a glance.
- **SURVIVED** — the suite stayed green on a clean build. A real oracle gap:
  strengthen the suite (add the missing check), do NOT weaken or delete the
  mutant. Halts feature work on that module per DESIGN §10. `make mutants` exits 1.
- **PATCH-FAILED** — `git apply` rejected the patch: the target file drifted since
  the patch was written. Refresh the patch (see below). Exits 2.
- **LINT-REJECT** — the patch touches a frozen `.mli` (see `FROZEN.sha256`), which
  is never a legal mutation target. Exits 2.

Exit code: 1 if any SURVIVED, 2 if any PATCH-FAILED/LINT-REJECT, else 0 (all
killed, whether at TEST or BUILD).

## Adding a mutant

1. Identify a fault a review / invariant says a suite catches (cite it in `review`).
2. Author the patch against current trunk, cleanly:
   edit the target file, run `make fmt` (so only the semantic change remains — the
   editor's formatter can otherwise reflow unrelated lines), then
   `git diff -- <file> > registry/<name>.patch` and `git checkout -- <file>`.
3. Write `<name>.meta` with the fields above. Aim for `expected=KILLED-TEST`:
   the fault should COMPILE (so an oracle, not the type-checker, catches it).
4. `make mutants MODULE=<module>` and confirm KILLED-TEST. A KILLED-BUILD means
   the compiler caught it first — rework the fault into a compiling variant if an
   oracle should be the one to catch it. If it SURVIVES, you found a gap — file
   it; do not soften the mutant to make it pass.

## Refreshing a drifted patch (PATCH-FAILED)

The target moved. Re-apply the same semantic fault by hand to the current file,
then regenerate the patch exactly as in step 2 above. The `.meta` `description`
tells you what the fault is.
