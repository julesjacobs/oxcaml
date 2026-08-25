# AGENTS.md

This file provides guidance to AI agents when working with code in this repository.

# OxCaml Compiler Development Guide

Do not stage or commit your changes unless prompted to.
Use `./dev` for the normal build-and-test loop. Do not start a full build or
full test suite in the middle of that loop. Run broader validation only at
closeout when the scope and risk of the change warrant it.

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

## Development workflow

After configuring a fresh worktree with a local install prefix, initialize it
once:

```bash
autoconf
./configure --prefix="$PWD/_install"
./dev init
```

After compiler or test edits, run one command; paths are relative to
`testsuite/tests`:

```bash
./dev test typing-modes/modes.ml
./dev test typing-modes/
./dev test --promote typing-modes/modes.ml
```

`./dev test` inspects the selected test actions, incrementally rebuilds and
stages the required compiler, compiler-library, toplevel, and expect-test
artifacts, then reuses the test tree. It rejects unsupported action families
before building and prints the full-build command to use. It also detects stale
or interrupted initialization and asks for `./dev init`; do not bypass these
checks or manually copy build artifacts.

By default, `.opt` test actions use the real compiler built with the bytecode
host, avoiding the optimized-compiler rebuild. Use the native host only when a
test depends on the compiler host backend:

```bash
./dev test --compiler-host=native path/to/test.ml
```

The incremental workflow does not cover changes to bootstrap-language support,
the runtime, the standard library, the compiler-libs installation, or test
infrastructure. In those cases, or if `./dev` rejects a test action, run the
fallback command it prints. Do not run concurrent `make`, Dune, or `./dev`
commands in the same worktree; they share build and test state.

If the execution of `autoconf` fails because the version is too old, try with `autoconf27` instead.

Configuration is needed after changing `.in` files or the autoconf script.

## Updating Merlin After Compiler Frontend Changes

Merlin (`external/merlin/`) vendors the compiler's frontend. Any frontend change — approximately `parsing/`, `typing/`, and the files in `file_formats/` and `utils/` they use — must be imported into Merlin; a CI check verifies this. Import by running `external/merlin/scripts/import-ocaml-source.sh` (never hand-merge compiler changes into `external/merlin`, and never manually modify `external/merlin/upstream/ocaml_flambda` except for `external/merlin/upstream/ocaml_flambda/.gitattributes`), then get `make merlin-test` passing. This is what the user is asking you to do if they say something like "Update Merlin", "Fix Merlin", or "Merge compiler/frontend/typing/type-checker changes into Merlin". New compiler flags (even backend-only ones) also require a Merlin update. Don't do anything before reading the full documentation: `external/merlin/HACKING.jst.md`.

## Development Guidelines
- Use `./dev test` for the compiler build-and-test loop
- Run `make -s fmt` before committing
- Keep lines under 80 characters
- Don't add excessive comments unless prompted
- Don't disable warnings or tests unless prompted
- Use pattern-matching and functional programming idioms
- Avoid `assert false` and other unreachable code

## Important Notes

- NEVER create files unless absolutely necessary
- ALWAYS prefer editing existing files
- NEVER proactively create documentation files (*.md) or README files
- NEVER stage or commit changes unless explicitly requested
