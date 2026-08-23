# OxCaml agent guide

## Initial setup

A fresh worktree needs one complete build and test installation:
```sh
make dev-configure   # or, by hand: autoconf27 && ./configure --prefix="$PWD/_install"
make dev
```
This takes about 5 minutes. Subsequent `make dev` invocations are much faster
because they use an incremental background watcher.

`configure` is not tracked in git, so every new worktree needs generating it, and
`configure.ac` requires autoconf >= 2.71 — newer than the `autoconf` on many
systems, where the one to use is `autoconf27`. `make dev-configure` finds a
suitable autoconf and configures with `--prefix="$PWD/_install"`; do that by hand
only if you need different flags.

## Development loop

The loop is **synchronous**: every command runs one direct dune build and
returns when it is done. There is no background watcher and no RPC (the
watcher this replaced wedged under load; see
`design-docs/dev-loop-sync.md` for the investigation and measurements).
Timings on a warm tree: `make dev` no-op ~1.5s; after editing a compiler
module ~5s; `make dev-test DIR=vox` on an already-built tree ~5s.

```sh
# edit compiler code, then build (this IS the error loop: ~1.5-5s):
make dev
# edit compiler code or a test, then build & run a test:
make dev-test TEST=typing-local/regression_class_type.ml
# edit again, test an entire dir:
make dev-test DIR=typing-local
# review a test's new output before accepting it:
make dev-diff TEST=path/to/test.ml
# promote current outputs as expect goldens
make dev-promote TEST=path/to/test.ml
# compile separate files
make dev-ocamlc ARGS='-c file.ml'
make dev-ocamlopt ARGS='file.ml -o file.exe'
# run the full compiler test suite
make dev-test-all
```

### Which build is needed for what

The tree builds three different ways; the loop only ever needs the first
two, and the third only twice per worktree lifetime.

| build | command | takes | produces | needed when |
|---|---|---|---|---|
| boot workspace (no bootstrap: the host/opam compiler builds this tree's compiler directly) | `make dev` | seconds | `_build/dev-dune/default/main_native.exe`, `main.bc`, ocamltest, tools | every compiler edit; this is the loop |
| runtime+stdlib workspace (the boot-built compiler builds the runtime and stdlib) | `make dev-refresh-stdlib` (automatic when needed) | ~2-4 min | `_build/runtime_stdlib_install/...` — the stdlib the dev tests compile against | after editing `runtime/`, `stdlib/`, `otherlibs/` (auto-detected by `dev-runtime`), or after a compiler change that alters marshaled `.cmi` shapes (auto-detected by the stale-stdlib probe, which now refreshes automatically) |
| main workspace (full bootstrap: stage-2 compiler, all libraries, installed tree) | `make install_for_test` | ~15 min | `_install`, `_runtest` | once at worktree setup (`dev-setup` does it), and it is what `make dev-test-all` runs against. **Never needed in the inner loop** — `dev-test` runs the boot compiler against the runtime_stdlib output |

Corollary: a change to `typing/types.ml` that alters marshaled shapes costs
one automatic ~3-minute stdlib refresh, not a 15-minute reinstall. If an
agent or a doc tells you to run `install_for_test` after a Types change,
it is describing the pre-synchronous loop; don't.

Promote with `make dev-promote`, **never** by copying a `.corrected` file. The
expect harness runs twice, plain and `-principal`; the second pass writes
`<test>.ml.corrected.corrected`, so copying `<test>.ml.corrected` silently drops
the principal-block updates and the parallel suite then fails tests that serial
spot checks show green. `make dev-diff` always shows the artifact that supersedes.

For one-file experiments the dev compiler can be called directly, which is faster
and more scriptable than `make dev-ocamlc`:
```sh
_build/dev-dune/default/main_native.exe -nostdlib -I _build/dev/runtest/stdlib
```
Note that this bypasses `make dev`'s checks, including the stale-stdlib check
below.

### When things go wrong

- **The compiler segfaults, or tests crash for no reason, after a compiler
  change.** A change to marshaled `.cmi` shapes leaves the previously built
  stdlib unreadable. `make dev` detects this with a one-line probe and
  refreshes the stdlib automatically (~3 min); set `DEV_NO_AUTO_STDLIB=1`
  to make it an error instead.
- **A worktree that used the old watcher-based loop.** `make dev-stop`
  stops a leftover watcher process; everything else needs no migration.
  `NOWATCH=1` is accepted and ignored, so older scripts keep working.
- **An `expect.opt` test whose result looks too good.** `dev-test` syncs the
  expect-test runners a selection needs with dune on every run, so a runner
  cannot be invoked against sources it was not built from; after a compiler
  change the sync is a real rebuild (~3 min for expectnat, a 110M link),
  otherwise it is a ~1s no-op. Which runners a selection needs can be checked
  directly:
  ```sh
  make dev-runners-needed DIR=codegen     # prints: expectnat
  ```
  History, for results produced by older trees: before PR #6794 the native
  runner was never refreshed; after it and until this sync landed, the refresh
  was gated on an mtime comparison against the dev compiler, which never fires
  for changes to modules only the runners link (the toplevels, and the whole
  native backend for expectnat) -- so `codegen/*` and toplevel-affecting
  results from those trees should not be trusted either: a compiler change
  could be followed by a passing test that had exercised the previous
  compiler.
- **A test reported as `skipped` under `make dev-test` that runs elsewhere.** The
  dev test root's `ocamlc.byte` is the boot `main.bc`, built by the host compiler,
  so the in-tree `ocamlrun` cannot execute it. Rather than failing, those actions
  skip with a reason naming the compiler they therefore do not cover. The tests
  are real and do run under `make dev-test-all`, which uses `_runtest` and the
  installed bytecode compiler — so a change to bytecode-compiler behaviour needs
  the full suite, not the fast loop. See
  `design-docs/dev-loop-improvements-final.md`.

## Release builds

Benchmark and memtrace only with the compiler produced by `make install`, never
the development boot compiler.

## Review loop

When the user asks you to review loop, that means that you launch several claude and codex agents to review the changes. Don't take their suggestions at face value: carefully triage which issues are real and important, and which aren't, with respect to the original goal. We want to avoid overcomplicating the code as a result of the review loop. The agents themselves should focus on:
* Whether this is the simplest way to do it - AI coding can result in overcomplicated code and diffs that are not optimally small. It is very important to keep the code small and elegant for long term code health. Key question: is this the simplest, most elegant way to do it?
* Is the architecture right? Is everything in the right place? Is there duplicated functionality? Is there a way to do it with less code?
* What are the alternative ways of doing it? Is there a better way?
* Whether there are bugs, ideally proved by a failing test case / repro. Triage whether you agree that the behavior is actually a bug or not.
* Whether there is important missing test coverage, ideally proved by a mutation that currently makes no test fail - it's also important to curtail test growth: the test suite should be compact, and each test should add separate value.
* In general: the key question is whether an expert human software engineer would critique the changeset as-is.

It is very important that review agents have their own worktree to build and run experiments and develop tests.
Once you have the review reports, try to ground claims in experimental reality as much as possible. Reproduce suggested issues. Try out suggested refactorings. Only accept if grounded reality shows that it is a real issue or real improvement.

Try to get the deliverable in the best shape of the highest quality. Use of subagents is almost free compared to human software engineering time, so use them well. The biggest danger is not bugs: the biggest danger is is wrong design decisions. Significant agent effort should be spent on evaluating whether the design decisions are optimal, because otherwise we end up in a morass of slop and progress will slowly grind to a halt. We want to keep the project maximally healthy. If you need human help, don't hesitate to ask for it.

## Worktree structure

Set up a directory named after the change/branch, and in that directory make a worktree called dev, and a subfolder called review where the worktrees of review agents live.

## Design docs and specs

Each change has a design doc, specified by the human. These live in the repo itself, in the design-docs folder. If that folder doesn't exist yet, make it. The design doc should be named after the branch.

If, during development, you come across a decision point where the design doc is ambiguous or simply doesn't specify which route to take, then first decide (1) is there an arguably best route (2) does the decision actually matter much. Only if there is no clear best route, note at the end of the design doc in concise style which route you took and why, and which alternatives you considered, and also notify me so that I can help check if that's the right decision.

## Friction

If you notice friction or other problems that could be fixed by going up a meta level and fixing the setup or tooling or appraoch, try to fix that!