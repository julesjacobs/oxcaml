# oxsmt build entry points. smt/ is a standalone stdlib-only subproject; the
# dev loop never builds or runs the compiler (DESIGN.md §1, §3).
#
# The bare `dune` on PATH is a Jane Street dispatch wrapper that fails outside
# jane workspaces, so we pin the opam toolchain explicitly.

OPAM_BIN := /usr/local/home/jujacobs/.opam/5.4.0/bin
DUNE := $(OPAM_BIN)/dune
export PATH := $(OPAM_BIN):$(PATH)

# Harness knobs (override on the command line, e.g. `make test SOLVER=path/to/real`).
# SOLVER defaults to the built stub until the real solver lands (DESIGN.md §8).
# LOGS/STATS resolve relative to the make invocation dir (the project root).
SOLVER   ?= _build/default/tests/harness/stub_solver.exe
LOGS     ?= ../logs
STATS    ?= $(LOGS)/stats
CASES    ?= tests/cases
FIXTURES ?= tests/harness/fixtures
HARNESS_ARGS := --solver $(SOLVER) --dir $(CASES) --dir $(FIXTURES) \
                --logs $(LOGS) --stats $(STATS)

.PHONY: build fmt test core-test bench gate promote

## build — compile everything under smt/ (stdlib-only). Fast dev loop.
build:
	$(DUNE) build @@default

## core-test — smt/core unit + property self-test (stdlib-only, deterministic).
##   Separate from `test` (the .smt2 harness): this is the in-tree TCB check for
##   the term layer (ADR-0003). Nonzero exit on any failed check.
core-test:
	$(DUNE) exec smt/core/test/core_test.exe

## fmt — format all sources in place with ocamlformat.
fmt:
	$(DUNE) fmt

## test — harness self-test, then the .smt2 golden/expect regression.
##   Runs the pure unit self-test (proves red-detection works) first, then
##   diffs produced-vs-golden over tests/cases + fixtures. Digest to stdout,
##   full detail under $(LOGS)/harness, exact stats under $(STATS). Nonzero on
##   any diff or missing golden. Override SOLVER to test the real solver.
test:
	$(DUNE) build tests/harness/run_harness.exe tests/harness/stub_solver.exe
	$(DUNE) exec tests/harness/harness_test.exe
	$(DUNE) exec tests/harness/run_harness.exe -- $(HARNESS_ARGS)

## bench — run the performance/adversarial corpus, emit digest to ../logs.
bench:
	@echo "not yet implemented (see TASKS.md: M0-harness)" && exit 1

## gate — Lean 4 certification (external oracle). Runs honeypots first (aborts
## red if any is certified), then the tests/cases corpus, using the
## content-addressed cache in ../cache. Digest to stdout; full log to ../logs.
gate:
	$(DUNE) build tests/gate/gate.exe
	_build/default/tests/gate/gate.exe selftest
	_build/default/tests/gate/gate.exe run

## promote — accept current solver output as golden (the promote workflow).
##   Rewrites the .smt2.expected sidecars for missing/mismatched goldens and
##   prints a per-file diffstat so the promoting agent sees what it accepts.
##   Label mismatches and solver errors are never masked — they still fail.
promote:
	$(DUNE) build tests/harness/run_harness.exe tests/harness/stub_solver.exe
	$(DUNE) exec tests/harness/run_harness.exe -- $(HARNESS_ARGS) --promote
