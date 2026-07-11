# oxsmt build entry points. smt/ is a standalone stdlib-only subproject; the
# dev loop never builds or runs the compiler (DESIGN.md §1, §3).
#
# The bare `dune` on PATH is a Jane Street dispatch wrapper that fails outside
# jane workspaces, so we pin the opam toolchain explicitly.

OPAM_BIN := /usr/local/home/jujacobs/.opam/5.4.0/bin
DUNE := $(OPAM_BIN)/dune
export PATH := $(OPAM_BIN):$(PATH)

.PHONY: build fmt test bench gate promote

## build — compile everything under smt/ (stdlib-only). Fast dev loop.
build:
	$(DUNE) build @@default

## fmt — format all sources in place with ocamlformat.
fmt:
	$(DUNE) fmt

## test — run the .smt2 golden/expect regression harness.
test:
	@echo "not yet implemented (see TASKS.md: M0-harness)" && exit 1

## bench — run the performance/adversarial corpus, emit digest to ../logs.
bench:
	@echo "not yet implemented (see TASKS.md: M0-harness)" && exit 1

## gate — Lean 4 certification of cache misses (external oracle).
gate:
	@echo "not yet implemented (see TASKS.md: M0-gate)" && exit 1

## promote — accept golden diffs via the promote workflow.
promote:
	@echo "not yet implemented (see TASKS.md: M0-harness)" && exit 1
