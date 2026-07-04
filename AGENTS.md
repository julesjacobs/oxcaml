# AGENTS.md

This file provides guidance to AI agents when working with code in this repository.

# OxCaml Compiler Development Guide

Do not stage or commit your changes unless prompted to.
Always check that your changes build with both (after configuration, see below):
1. `make -s boot-compiler` - Quick build check
2. `make -s test` - Full test suite (required before declaring success)

You are working on the OxCaml compiler, a performance-focused fork of OCaml with Jane Street extensions, including the Flambda 2 optimizer and CFG backend.

## Key Architecture

**Directory Structure:**
- `middle_end/flambda2/` - Flambda 2 optimizer implementation
- `backend/cfg/` - Control Flow Graph backend
- `driver/` - Compiler driver, including `oxcaml_*` files for OxCaml-specific options
- `jane/` - Jane Street specific extensions and documentation
- `testsuite/tests/` - Upstream OCaml test suite
- `oxcaml/tests/` - OxCaml-specific tests

**Important Files:**
- `driver/oxcaml_flags.ml` - OxCaml compiler flags definitions
- `driver/oxcaml_args.ml` - Command-line argument handling
- Files ending in `.in` require configuration via `./configure`

## Build Commands
```bash
make -s boot-compiler         # Quick build (recommended for development)
make -s                       # Full build
make -s install               # Install the compiler to $(pwd)/_install
make -s fmt                   # Auto-format code (always run before committing)
```

## Test Commands
```bash
make -s test-one TEST=test-dir/path.ml      # Run a single test testsuite/tests/test-dir/path.ml
make -s test-one DIR=test-dir               # Run all tests in testsuite/tests/test-dir
make -s promote-one TEST=test-dir/path.ml   # Update expected test output
make -s test                                # Run all tests
```

## Configuration Commands
```bash
autoconf                  # Generate configure script
./configure               # Configure the compiler
```

If the execution of `autoconf` fails because the version is too old, try with `autoconf27` instead.

Configuration is needed after changing `.in` files or the autoconf script.

## Development Guidelines
- Always verify changes build with `make -s boot-compiler`
- Run `make -s fmt` to ensure code formatting
- Keep lines under 80 characters
- Don't add excessive comments unless prompted
- Don't disable warnings or tests unless prompted
- Use pattern-matching and functional programming idioms
- Avoid `assert false` and other unreachable code
- Rebuild the project often while using the LSP using `make -s boot-compiler`. When
  you don't rebuild, the LSP may give you stale information from a previous build

## vox: verification workflow (measured 2026-07-03)

- Iterate on vox code by invoking the built compiler DIRECTLY -- a
  verified module is about 1 second of Lean end to end:
  `_build/_bootinstall/bin/ocamlc.opt -vox-solver-path <lean> -c file.ml`
  (locate `<lean>` the way `testsuite/tests/vox/has-lean.sh` does:
  `$VOX_LEAN`, PATH, or its pinned copy).
- `make -s test-one` costs ~17s even on an UNCHANGED tree (its
  install_for_test step re-checks the full dune install graph and
  re-rsyncs the testsuite into _runtest), and MINUTES after compiler
  changes or a pull.  Use it for final validation, not iteration, and
  never concurrently with a background build or suite -- contention
  turns seconds into minutes.
- When only TEST FILES changed, skip the expensive step: copy the
  edited test over its _runtest counterpart and run the no-rebuild
  target (~3s):
    cp testsuite/tests/vox/foo.ml _runtest/testsuite/tests/vox/foo.ml
    make -s test-one-no-rebuild TEST=vox/foo.ml
  (_runtest is a HARDLINKED copy, so in-place edits propagate by
  themselves, but rename-style writers -- sed -i, most editors, git
  -- silently break the link; the cp makes the recipe unconditional
  and also covers brand-new files.)
- Cost model: honest module ~1s; a failing GROUND goal is sub-second
  even with a large prelude; a failing QUANTIFIED goal under
  quantified hypotheses costs ~6s of genuine search (not
  heartbeat-bound).  So write expected-failure test goals as small
  ground facts (`p = a + 2`, `1 = 2`), not quantified claims.
- After `make promote-one` on an expect test, EYEBALL the promoted
  expectations: confirm each rejection happens at the INTENDED layer
  (mode / locality / contract VC / Lean proof failure).  A test that
  "passes" while rejecting for the wrong reason -- e.g. an
  elaboration error instead of a proof failure -- is a latent bug.
- Mutable-array ghosts: use McCarthy stores (writes return
  `{ _ = upd a j w }`, reads return the SAME atom `{ _ = a }`, three
  @[grind] store axioms).  Quantified per-call frame conditions do
  NOT scale: grind cannot instantiate forall-facts at goal indices.
  State loop invariants as prelude Props with ONE hand-proved step
  lemma per loop whose variables are all bound by its conclusion
  (see demo/lean_reverse.ml).
- Client `[%%vox.lean]` blocks may contain definitions and PROVED
  theorems (Lean checks them); a client `axiom` silently joins the
  TCB and can verify falsehoods -- currently unenforced, so review
  for it.

## Important Notes

- NEVER create files unless absolutely necessary
- ALWAYS prefer editing existing files
- NEVER proactively create documentation files (*.md) or README files
- NEVER stage or commit changes unless explicitly requested
