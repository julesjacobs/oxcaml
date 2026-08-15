# Vox erasure — final report

State of the piece at the end of build + review loop, on branch
`jujacobs/vox/erasure` (9 commits, `6929017cbd..9745883e94`). The design doc
proper is `erasure.md` next to this file; its "Decisions taken during
implementation" section records each choice and the alternatives.

## What landed

**The axis.** `Erasure` (`Retained < Erased`, comonadic, legacy `Retained`),
following the Forkable/Yielding pattern end to end: lattice, solver axis,
user-facing `Mode.Erasure`, `@ erased` / `@ retained` mode names, printing.
Never crossable — an erased value has no runtime representation, so no type
may treat Erased as Retained. Pinned at every place a crossing is built
(`min_crossable`, `cross_all_crossable`, `always_constructed_at`,
`Axis_lattice.create`, `~erasure:false` at bool-created crossings);
`mod erased` / `mod retained` are rejected as kind modifiers and
`mod everything` excludes erasure (precedent: staticity).

**`erased_ e`.** New keyword and `Pexp_erased`, typed as an exp_extra
(`Texp_erased`, the `Texp_stack` precedent). The expression itself is erased:
only the erasure axis of the expected mode is constrained, so `erased_ e` in
a retained position is a submode error. The ambient rule ("inside an erased
context, anything goes on the erasure axis, because the context is deleted")
is an environment flag consulted at the single `Typecore.submode` funnel —
compositional over variables, application results, and nested `erased_`.

**Typing rules.** The permissive expected mode requires Retained on the
erasure axis; the erased-tolerant positions are the explicit exceptions
(erased contexts, statement position, type-driven `@ erased` positions).
Destructuring patterns, record field access/mutation, and the function
position of applications constrain their fresh mode variables; binding
patterns (variables, wildcards, aliases) do not, so `let x = erased_ e`
works. Closure captures and `close_over` exclude erasure (a retained closure
may capture erased values; partial application over an erased argument does
not erase the wrapper). Erasure is invariant in argument position under
moregen and coercion, in both directions at every arrow-nesting depth —
whether an argument is passed is ABI.

**Codegen.** The void path, as the design doc prescribed. Erased parameters,
arguments, and bindings translate at `Punboxed_product []`; every other
erased occurrence becomes a placeholder of whatever layout the context
requests, which makes structural erasure total. Measured on native code:

- an erased parameter is absent from the emitted function; a function whose
  only parameter is erased compiles to a nullary symbol;
- a closure whose only capture is erased becomes a *static* closure (no
  slot, no allocation);
- partial application across an erased parameter uses the ABI arity
  (`caml_curry_V_V`);
- `erased_ e` deletes evaluation including effects and exceptions; a
  retained argument at an erased parameter is evaluated for its effects and
  dropped at the boundary;
- `.cmi` round-trips: an erased parameter called from another compilation
  unit links and runs.

**Tests.** `testsuite/tests/vox/erasure.ml` (typing: ambient rule,
application, closures, read positions, structural erasure, the full
sealing/coercion matrix in both positions and directions, inference
defaults, no-crossing), `erasure_runtime.ml` (runtime deletion semantics —
deliberately pins the effect-deleting unsoundness so the totality piece
shows up as a reference diff), `erasure_units.ml` (two-unit compile+run:
ident-table isolation and the cross-unit `.cmi` round trip).

Full suite: 2443 passed, 208 skipped, 2 failed — both pre-existing dev-flow
infrastructure failures (`formatting/test_locations.ml`,
`tool-ocamlc-stop-after/stop_after_typing_impl.ml`: `ocamlc.byte`-flavored
tests cannot run under the dev harness; a runtime magic-number mismatch in
test *setup*, unrelated to this branch).

## Review loop

Three reviewers in their own worktrees under `erasure/review/` (two claude
lenses: design/simplicity and correctness/coverage; one codex full review —
codex needed three attempts, its content filter tripped twice). Reports are
preserved as `erasure/review/*-report.md`. Every accepted finding was
reproduced before acting. Fixed as a result:

- nested-arrow argument invariance escape (sealed callback ABI mismatch,
  runtime abort);
- structural erasure aborts (~a dozen: erased components inside tuples,
  records, branches, closures put void operands in value-layout blocks);
- missed read positions (`while` conditions, `for` bounds, field
  access/mutation) — closed by flipping the default expectation rather than
  extending an un-greppable blacklist;
- non-compositional ambient rule (env lock replaced by the submode-funnel
  flag);
- per-unit erased-ident table leaking across compilation units (Ident
  stamps restart per unit; a stale entry gave *another unit's* like-named
  parameter the void layout);
- optional-parameter and over-applied-primitive ABI mismatches;
- externals with erased parameters and the `@@ erased` modality now fail
  closed instead of compiling to broken code / warning-and-ignoring;
- an error-message hint-chain regression from the closure carve-out.

One review suggestion was adopted and then **falsified**: pinning the
no-crossing invariant at the two readers of stored kind bounds instead of at
construction sites broke kind subsumption at scale (672 suite failures; the
kind machinery reads bounds through several views and pinning only some is
incoherent). Reverted with the evidence in the commit message — the
reviewer's version had only been validated against four test directories.

## Open design decisions (flagged for Jules)

1. **`@@ erased` record fields.** The design doc's `@@ global` analogy has
   the direction backwards: comonadic modalities are meets (they can only
   strengthen a field relative to its record), while an erased field in a
   retained record is a weakening. Options are a comonadic `Join_const`
   modality (touches modality composition, zapping, inclusion, cmi format)
   or a special-cased representation-bearing marker; either is a piece of
   its own. Currently `@@ erased` is rejected, not ignored.
2. **[Resolved after this report was first written] Arrow-mode laundering.**
   The queued RED test (`erasure_gaps.ml`) plus experiments identified the
   mechanism: not `instance`, not moregen/subtyping, but the arrow-mode
   loosening in `Typecore.type_argument` (`loosen_arrow_modes`), which gives
   inferred function arguments contravariant/covariant mode subsumption —
   safe for every pre-existing axis, ABI-changing for erasure. Fixed by
   equating the erasure component instead of loosening it; all three
   spellings (annotation, sealing, coercion) now agree, locality analogues
   verified unchanged, full suite green. Erased optionals without a default
   also now agree with their callers (retained convention on both sides).
   **Still open (gap 2):** a call site can infer an unannotated parameter's
   erasure within one structure (`h (erased_ 5)` gives
   `h : 'a @ erased -> int`), against the doc's "never inferred" rule —
   pinned in `erasure_gaps.ml`, needs a decision on fixing unannotated arrow
   erasure at creation vs. rejecting erased arguments to flexible arrows.
3. **Deferred, documented in `erasure.md`:** zero-width erased *returns*
   (currently a correct one-word placeholder), structure-level erased
   bindings (rejected; needs the same missing weakening-modality story as
   fields), `erased_` in quotations (rejected), erased optional parameters
   (retained convention on both sides), erased array elements, and the
   `e @ total` requirement that ties into the totality piece.

## Round-2 soundness review (second loop)

A soundness-only review loop on the merged tip (two claude lenses + codex)
found and closed four more paths where an erased value could be observed at
run time or caller/callee ABIs could disagree — each reproduced (segfault,
silent wrong value, or compiler abort) before fixing, each pinned in
`erasure_gaps.ml`:

1. `(e :> ty)` coercion with a non-closed target laundered an erased parameter
   into a retained arrow via a fourth arrow-mode loosening path
   (`Ctype.build_subtype`) that argument invariance had not reached — verified
   silent garbage and a segfault. Fixed by equating erasure there.
2. Erased optional parameters (type says erased, codegen keeps the option's
   retained convention) leaked a placeholder into a real slot — now rejected
   outright, at the definition site and in written arrow types.
3. An external's *result* arrow could carry an erased parameter (a stub
   returning a closure); the check now walks the whole type.
4. A local module (`let open struct ... end`) stored an erased binding as a
   void operand in a value block; fixed by capping a module allocation's
   erasure to Retained. Splice and the magic-staged quotation body were raw
   `Value.max` read positions accepting an erased code value (segfault); both
   now require retained, as does the overwrite cell mode.

Full suite green after the batch (only the two pre-existing dev-harness
failures). One non-soundness robustness gap remains: an erased occurrence at a
SIMD vector layout is a `Misc.fatal_error` in translation rather than a located
type error (needs `-extension simd`; no code produced, so not a runtime
hole) — flagged for follow-up.

## Tooling friction observed

- The dev watcher's dune repeatedly wedges: `dune rpc build` hangs forever
  against an idle watch instance (needed ~10 `tools/dev-watcher.py stop` +
  retry cycles during this piece). Detection: build silent >10 min while
  the watch dune sits at 0% CPU.
- `ocamlc.byte`-flavored tests cannot run under the dev harness (the boot
  bytecode compiler and the installed test runtime disagree on exec magic),
  which accounts for both standing suite failures.
