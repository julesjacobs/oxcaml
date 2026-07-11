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

# SAT bench corpus. Defaults to the uf50/uuf50 families (the M1 verdict-agreement
# target, solved in seconds); GLOBs at runtime and tolerates absence. Override to
# run elsewhere, e.g. `make sat-bench SAT_CORPUS=../corpora/SAT` for the whole
# tree (which includes intentionally-hard families like pigeon-hole).
SAT_CORPUS ?= ../corpora/SAT/uf50-218 ../corpora/SAT/uuf50-218

.PHONY: build fmt test core-test sat-test sat-bench bench gate promote check-frozen spine status status-fresh

## build — compile everything under smt/ (stdlib-only). Fast dev loop.
build:
	$(DUNE) build @@default

## core-test — smt/core unit + property self-test (stdlib-only, deterministic).
##   Separate from `test` (the .smt2 harness): this is the in-tree TCB check for
##   the term layer (ADR-0003). Nonzero exit on any failed check.
core-test:
	$(DUNE) exec smt/core/test/core_test.exe

## sat-test — CDCL SAT core (smt/solver) unit + property self-test (stdlib-only,
##   deterministic). Exact learned-clause/backjump/antecedent checks on textbook
##   conflicts, assumption semantics, incremental add-after-solve, every sat model
##   self-checked by evaluation, and thousands of random CNFs cross-checked against
##   an independent DPLL oracle. Nonzero exit on any failed check (TASKS.md M1-sat).
sat-test:
	$(DUNE) exec smt/solver/test/sat_test.exe

## sat-bench — run the SAT core over a DIMACS corpus ($(SAT_CORPUS)). GLOBs
##   **/*.cnf at runtime, label-checks uf*/uuf* families, self-checks every sat
##   model, and tolerates an absent corpus with a clear message. Digest to stdout,
##   full per-file log under $(LOGS). Deterministic (slowest ranking by conflicts).
sat-bench:
	@mkdir -p $(LOGS)
	$(DUNE) build smt/solver/test/sat_bench.exe
	_build/default/smt/solver/test/sat_bench.exe $(SAT_CORPUS) --log $(LOGS)/sat-bench.log

## fmt — format all sources in place with ocamlformat.
fmt:
	$(DUNE) fmt

## check-frozen — enforce the frozen-interface hashes (DESIGN.md §10, §11).
##   Recomputes sha256 of the five frozen core .mlis and diffs FROZEN.sha256;
##   red (with the unfreeze instructions) on any drift. Runs first in `test`.
check-frozen:
	tools/check_frozen.sh check

## spine — regenerate SPINE.md, the master's concatenated view of the frozen
##   core .mlis (DESIGN.md §11). Commit the result; it is a generated file.
spine:
	tools/check_frozen.sh spine

## test — frozen-interface guard, harness self-test, then the .smt2 golden/expect
##   regression. check-frozen runs FIRST so a drifted frozen .mli fails every
##   suite run. Then the pure unit self-test (proves red-detection works), then
##   diffs produced-vs-golden over tests/cases + fixtures. Digest to stdout,
##   full detail under $(LOGS)/harness, exact stats under $(STATS). Nonzero on
##   any diff or missing golden. Override SOLVER to test the real solver.
test: check-frozen
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

## status — regenerate STATUS.md by AGGREGATING existing artifacts (DESIGN §8.4, §11).
##   Reads TASKS.md, git, the latest gate log, the most recent existing stats
##   JSONL, committed line budgets, and the last captured harness digest. Does
##   NOT run the harness — so the committed STATUS.md is byte-stable given the
##   same (repo, logs): back-to-back `make status` produce identical output. The
##   only per-run-varying line is "generated at <HEAD>" (git HEAD, never
##   wall-clock); per-goal wall_ms lives only in the uncommitted stats sidecar,
##   never in the committed file. Prints a ~5-line digest; overwrites STATUS.md.
status:
	$(DUNE) build tools/status_gen/status_gen.exe
	$(DUNE) exec tools/status_gen/status_gen.exe -- \
	  --repo . --logs $(LOGS) --stats $(STATS) --tasks TASKS.md \
	  --budgets tools/line_budgets.txt \
	  --harness-digest $(LOGS)/harness/last-digest.txt --out STATUS.md

## status-fresh — refresh inputs, then regenerate (the nightly path). Runs the
##   fast harness once (writing a new stats JSONL + capturing its digest) so the
##   pass/fail line and stats reflect the current tree, then aggregates via
##   `status`. Not on the plain-`make status` path precisely because it mutates
##   inputs (new stats file) — keeping generation and refresh separate is what
##   makes the committed artifact's diff meaningful.
status-fresh:
	$(DUNE) build tests/harness/run_harness.exe tests/harness/stub_solver.exe
	@mkdir -p $(LOGS)/harness
	-$(DUNE) exec tests/harness/run_harness.exe -- $(HARNESS_ARGS) > $(LOGS)/harness/last-digest.txt 2>&1
	$(MAKE) status LOGS=$(LOGS) STATS=$(STATS)

## promote — accept current solver output as golden (the promote workflow).
##   Rewrites the .smt2.expected sidecars for missing/mismatched goldens and
##   prints a per-file diffstat so the promoting agent sees what it accepts.
##   Label mismatches and solver errors are never masked — they still fail.
promote:
	$(DUNE) build tests/harness/run_harness.exe tests/harness/stub_solver.exe
	$(DUNE) exec tests/harness/run_harness.exe -- $(HARNESS_ARGS) --promote
