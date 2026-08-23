# A synchronous dev loop

The dev loop's watcher is removed: every `make dev*` command now runs one
direct, synchronous dune build and returns when it is done.

## Why

The watcher (a worktree-local `dune build -w` supervised by
`tools/dev-watcher.py`, driven over dune RPC) existed to keep dune's rule
database warm so `make dev` could return in seconds. In practice, on a
box running several worktrees' builds at once, the RPC path wedged
repeatedly: the watcher stayed alive and answered pings but the
`dune rpc build` never started, stalling `make dev`/`make dev-test` for
minutes until killed (observed ~5 times in one working session, across
two worktrees, both loop vintages). The timeout-and-fallback hardening
bounded the damage but kept the complexity.

Measurement shows the watcher was solving a problem dune no longer has.
On this tree (warm `_build`, direct synchronous dune):

| operation | time |
|---|---|
| `dune build main_native.exe`, no changes | 0.98s |
| full dev target set (compiler, bytecode, ocamltest, tools), no changes | 1.96s |
| `make dev` (adds setup/stdlib checks), no changes | 1.5s |
| `make dev` after editing `typing/typecore.ml` | 5.6s |
| `make dev-test DIR=vox`, tree already built | 4.3s |

A synchronous loop at these numbers needs no warm process, cannot wedge,
needs no RPC timeout tuning, no `NOWATCH` escape hatch, no
status/log/heartbeat plumbing, and no recovery self-tests.

## What changed

- `dev-check` runs the direct build unconditionally; the
  watcher/RPC branch, `dev-start`, `dev-status`, `dev-log`,
  `DEV_IDLE_TIMEOUT`, `DEV_RPC_TIMEOUT` and `DEV_HEARTBEAT` are gone.
  `NOWATCH` is accepted and ignored for compatibility.
- `dev-errors` is the direct build (with a 1.5-5s build, the error loop
  is the build); it no longer needs a running watcher to answer.
- `dev-stop` remains as a legacy-migration no-op: it stops a watcher left
  by the previous loop, if any.
- The stale-stdlib probe (`dev-stdlib-check`) now refreshes the stdlib
  automatically when it detects a marshaled-`.cmi`-shape change
  (`make dev-refresh-stdlib`, ~2-4 min) and re-probes, instead of
  stopping with advice. `DEV_NO_AUTO_STDLIB=1` restores the error.
- `dev-selftest` drops the watcher-recovery test and keeps the
  runner-selection checks.
- `dev-test` syncs the expect-test runners a selection needs with a dune
  build on every run, instead of gating the refresh on an mtime
  comparison against the dev compiler. The runners link libraries the
  dev compiler does not (the toplevels; the whole native backend for
  `expectnat`), so a change there never advanced `main_native.exe`'s
  mtime and the old gate silently accepted a runner built from the
  previous sources — a green expect result against the previous
  compiler. Dune's dependency tracking of the main workspace is the
  only sound answer to "does the runner match the sources", and asking
  costs ~1s when nothing changed. A vintage stamp compiled into the
  runner was rejected: it would duplicate the link-time dependency set
  by hand and drift the next time a runner gains a library.
- `tools/dev-watcher.py` remains for its non-watcher utilities
  (`prepare-test-root`, `diff`), which the loop still uses; its
  supervise/build subcommands are dead and can be deleted in a later
  cleanup together with `tools/dev-watcher-test.sh`.

## The build-mode map (the actual investigation)

Three ways to build live in this repository, and confusing them is what
made the loop feel slow:

1. **Boot workspace** (`duneconf/boot.ws`, built into `_build/dev-dune`):
   the host/opam compiler builds this tree's compiler sources directly —
   no bootstrap. Produces `main_native.exe` (the dev compiler),
   `main.bc`, `ocamltest`, and the tools. Seconds, incremental. This is
   the loop.
2. **Runtime+stdlib workspace** (`duneconf/runtime_stdlib.ws`): the
   boot-built compiler builds `runtime/`, `stdlib/`, `otherlibs/` into
   `_build/runtime_stdlib_install`. ~2-4 minutes. `dev-test` compiles
   test programs against exactly this; `dev-runtime` refreshes it when
   its sources change, and the stale-stdlib probe refreshes it when the
   compiler's `.cmi` shapes change.
3. **Main workspace** (`duneconf/main.ws`): the full bootstrap — the
   boot compiler plus the runtime/stdlib output build the stage
   compiler, every library and tool, and install into `_install` /
   `_runtest`. ~15 minutes. Needed exactly twice per worktree lifetime:
   once at setup (`dev-setup`), and implicitly by `make dev-test-all`,
   which runs the full suite against `_runtest` (that is also where
   bytecode-compiler actions run for real; the fast loop skips them,
   see dev-loop-improvements-final.md).

`dev-test` was already wired (by the previous dev-loop piece) to run the
boot compiler against the runtime_stdlib output via `prepare-test-root`
symlinks — the 15-minute `install_for_test` was never part of the inner
loop's requirements. This piece makes that fact impossible to miss: the
stdlib refresh is automatic, and the map above is in AGENTS.md.

## Not done, deliberately

- Deleting the dead watcher machinery from `tools/dev-watcher.py` (kept
  this change reviewable; the utilities interleave with it).
- Touching `hacking` / `hacking-emacs-*` (upstream-style polling
  targets, separate from the dev loop).
- Any change to `dev-test-all` semantics.
