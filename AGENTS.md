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
  `$VOX_LEAN`, PATH, or its pinned copy).  CAVEAT (diagnosed
  2026-07-04): that binary hard-codes `standard_library =
  $(pwd)/_install/lib/ocaml`, so after compiler changes or a pull a
  STALE `_install` feeds it old cmis and it SIGSEGVs unmarshalling
  them -- on EVERY file, even `-c trivial.ml` (the harness is immune:
  it installs its own tree).  The fix is `make -s install`; treat a
  standalone segfault as staleness, not your edit.  Expect-style
  demos (`expect;` in the TEST header, e.g. lean_isqrt, lean_reflect)
  never compile directly -- `Uninterpreted extension 'expect'` -- so
  iterate on those through `make test-one-no-rebuild`.
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
- `[%%vox.lean]` blocks in an INTERFACE (.mli) are compiled to a
  VoxSig_<Unit>.olean module (clients import the artifact; author
  marks declarations `public`, defs clients unfold `@[grind, expose]`),
  and an interface `axiom` is an OBLIGATION: the implementation's
  solver input ends in a seal that demands a same-named proved
  theorem.  Blocks in an IMPLEMENTATION or client .ml keep the old
  trust story: definitions and theorems are checked, but a .ml
  `axiom` silently joins the TCB and can verify falsehoods --
  unenforced there, so review for it.
- FULL abstraction: `type t [@@vox.sort opaque]` in an .mli gives the
  type its OWN uninterpreted sort (Vox_<Unit>_t) instead of the
  shared VoxU, so interface blocks can state laws about it (declare
  model constants as `axiom`s -- `opaque` needs an inhabitation
  witness).  The implementation's concrete declaration registers
  under the SAME solver name, which is how the seal's re-elaborated
  laws land on the concrete type (see lib/oset.mli).  Sound
  asymmetry: opaque interface over concrete impl is allowed; the
  int/bool ghost sorts still must match on both sides.

## Token economics (measured 2026-07-04 across all vox sessions)

The prompt cache expires 5 minutes after last use; re-reading it costs
ctx x $1/M, rewriting it after expiry costs ctx x $12.5/M.  A transcript
audit (deduped by requestId) found 73% of all spend on calls with >400k
context and ~$1.5k of full-prefix rewrites caused by blocking waits that
outlived the TTL.  Rules:

- NEVER block longer than 270s in one call (TaskOutput, sleeps, long
  waits).  Chain 270s blocks instead: each cycle re-reads the warm
  cache at 1/12.5 the cost of the rewrite that expiry causes.
- The keep-alive-vs-drop break-even is ~55 MINUTES, independent of
  model and context size (poll and rewrite both scale with ctx, so it
  cancels: 12.5 polls x 4.5 min).  Expected wait under ~55 min: stay
  in-turn and chain 270s blocks.  Longer or unbounded (waiting on a
  human): end the turn and accept the one rewrite.
- Cheaper than either: BATCH long validations (full suites, long Lean
  runs) at the end of a work block so the cache dies once, not five
  times -- or babysit them from a separate small-context session where
  a rewrite costs cents.
- Keep working context under ~300-400k.  You CAN measure it: the last
  usage entry of your own transcript is the current context --
    tail -50 ~/.claude/projects/<proj-dir>/<session-id>.jsonl \
      | grep -o '"cache_read_input_tokens":[0-9]*' | tail -1
  (add cache_creation + input for the exact figure).  Check it when a
  phase completes.  The split itself is the USER's action (/clear or a
  new session) -- your actions are: (a) push exploration into
  subagents so the main thread stays lean, (b) at phase boundaries
  past ~300k, write a handoff note (state, next steps, open questions)
  to a file or commit message and SUGGEST the split, (c) below ~100k,
  none of this matters -- don't busy-poll or nag there.

## Important Notes

- NEVER create files unless absolutely necessary
- ALWAYS prefer editing existing files
- NEVER proactively create documentation files (*.md) or README files
- NEVER stage or commit changes unless explicitly requested
