# IDE VC dump report

## Status

Complete. Flag plumbing, per-VC capture, JSON serialization,
annotation/contract/seal provenance, source-like predicate displays, per-fact
origins, and the type-only gate are implemented and verified. The boot compiler
builds; all required suites are green; the unchanged default output and live
JSON samples are confirmed (see Verification evidence below).

## Flags and mechanism

- `-vox-dump-vc-json FILE` writes one JSON document to `FILE` at process exit.
  It does not write JSON to stdout or stderr. If the sidecar cannot be written,
  it emits a warning on stderr and preserves the compile's normal exit status.
  It records every VC actually passed to `Vox_lean.discharge`, including VCs
  processed before a later verification error aborts compilation.
- `-vox-type-only` skips both `Vox_verify.verify_structure` calls in
  `typing/typemod.ml` and skips seal-obligation verification.  It therefore
  generates and discharges no refinement VCs. It typechecks the requested unit
  in isolation, emits `.annot` only when `-annot` is explicitly present, and
  prints the inferred signature when `-i` is present. It does not write
  `.cmi`, `.cmti`, `.cmo`, `.cmt`, `.cms`, or `.cmsi` files, including for
  `.mli` inputs and partial typed trees on type errors.
- `-vox-dump-vc` streams a compact, human-readable block for each VC to stderr
  as compilation proceeds (in source order), for expect/`.reference` testing of
  VC generation. It is a dump mode, not verification: it does NOT invoke the
  solver (Lean-independent, so the suite is fast and hermetic), and it ends the
  compile with a distinctive `Error: VCs dumped, not discharged.` and a non-zero
  exit so a dry dump can never be mistaken for a passing verification. See "VC
  text dump" below.

`-vox-type-only` intentionally does not check `.ml`-vs-`.mli` conformance: it
takes the single-unit no-save path before an adjacent `.mli` is loaded and
checked. This is the intended single-buffer editor dry-run behavior; a
conformance-checking dry run is outside this mode's scope.

The flags are independent.  Combining them produces a valid document whose
`verification_conditions` array is empty.

The dump distinguishes the VC origins present in the implementation:
`annotation`, `contract-argument`, and `seal-implication`.  Branch conditions
are facts in an enclosing VC, not separately discharged VCs, and therefore
appear in `facts` with their spans instead of being assigned an invented VC
kind.

## JSON schema (version 2)

The top-level document is:

```text
{
  "schema_version": 2,
  "verification_conditions": [ VC, ... ]
}
```

Each `VC` has exactly these fields:

```text
{
  "location": Span,
  "program_point": Span,
  "kind": "annotation" | "contract-argument" | "seal-implication",
  "goal": GoalPredicate,
  "facts": [ FactPredicate, ... ],
  "discharge": {
    "status": "proved" | "not-proved" | "disproved" | "solver-error",
    "detail": string | null,
    "counterexample": string | null
  },
  "generated_lean": string | null,
  "emission_error": { "message": string, "location": Span } | null,
  "provenance": {
    "kind": "annotation" | "contract-argument" | "seal-implication",
    "name": string | null,
    "source_span": Span | null,
    "related_spans": [ { "role": string, "span": Span }, ... ]
  }
}
```

`GoalPredicate` is:

```text
{ "text": string, "display": string, "source_span": Span }
```

`FactPredicate` is:

```text
{
  "text": string,
  "display": string,
  "source_span": Span | null,
  "origin": {
    "kind": string,
    "name": string | null,
    "span": Span | null
  }
}
```

The goal always has a concrete `source_span`; fact spans can be `null` for
synthesized facts. `Span` is:

```text
{
  "file": string,
  "start": { "line": integer, "column": integer },
  "end": { "line": integer, "column": integer },
  "ghost": boolean
}
```

Lines are 1-based and columns are 0-based byte offsets, matching OCaml
locations. A VOX-local encoder handles string values; `Misc.Json` still builds
the object/array structure and escapes the ASCII field names. The local encoder
uses standard JSON escapes for quotes, backslashes, and controls, passes
well-formed UTF-8 through unchanged, and represents each ill-formed UTF-8 byte
as `\u00HH`. Thus Lean text such as `⊢`, `¬`, and `→` remains readable UTF-8,
while arbitrary solver bytes still produce JSON accepted by a strict parser.

VC provenance details by kind:

- `annotation`: `name` is null, `source_span` is the annotation span, and a
  `subject` related span identifies the checked expression.
- `contract-argument`: `name` is the parameter/binder name when available
  (falling back to the refinement view name for imported contracts),
  `source_span` is the contract predicate, and `argument` and `application`
  related spans identify the call site.
- `seal-implication`: `name` is the sealed value name, `source_span` is the
  seal point, and `interface` and `implementation` related spans identify the
  two declarations.

Fact origin details by kind:

- `binder`: `name` is the refined binder name and `span` is its pattern.
- `branch`: `name` is always null because a branch condition has no binder;
  `span` is the condition expression for both the positive and negated fact.
- `application`: for a refined application-result fact, `name` is the callee
  when the callee is a direct identifier and otherwise null; `span` is the
  call/result site.
- `annotation`: for a proved annotation retained as a later hypothesis,
  `name` is null and `span` is the annotation.
- `contract-argument`: for a proved argument contract retained as a later
  hypothesis, `name` is the parameter or refinement-view name and `span` is
  the contract predicate.
- `seal-implication`: `name` is the sealed value name and `span` is the
  implementation declaration supplying the hypothesis.

`generated_lean` is exactly the positive, non-negated theorem returned by
`Vox_lean.emit`; it does not include the negated theorem that `discharge`
constructs internally while checking for a disproof. It is null only when
`Vox_lean.emit` fails; in that case `emission_error` contains the failure.
For valid UTF-8, `detail` decodes to the solver diagnostic unchanged; malformed
input bytes decode as the corresponding U+00HH characters described above.
For a `disproved` result, `counterexample` repeats that encoded diagnostic when
it explicitly contains a `counterexample` or `witness` marker; otherwise it is
null. The
`Vox_lean.result.location` field is intentionally not serialized; the VC's
`location`, `program_point`, and provenance spans carry the source locations
exposed by schema version 2.

## VC text dump (`-vox-dump-vc`)

`-vox-dump-vc` exists so testsuite `.reference` files can pin down VC
GENERATION behavior (the way typing expect tests pin inference), independently
of the solver. It is separate from `-vox-dump-vc-json` (machine JSON, whole-run,
written to a file at process exit): the text dump is human-readable, streamed in
source order to stderr, and Lean-independent. Annotation, contract-argument,
inline module-seal, and file-level `.ml`-vs-`.mli` seal VCs are all included;
the final not-discharged error is deferred until after compilation-unit
inclusion has generated the file-level obligations.

### Block format

For each VC, at the point it is generated, one block is written to stderr:

```text
VC <kind> at <file>:<line>:<startcol>-<endcol>
  <hypothesis-1 display>
  <hypothesis-2 display>
  ...
|- <goal display>
```

`<kind>` is `annotation`, `contract-argument`, or `seal-implication`. Each
hypothesis line and the goal line are rendered by the source-like `display`
printer (the same one used for the JSON `display` field), two-space indented for
hypotheses; the goal is introduced by an ASCII turnstile `|-` (chosen over the
Unicode `⊢` for `.reference` friendliness). A blank line separates blocks. When
a VC's location spans multiple lines the header is
`<file>:<line>:<col>-<endline>:<endcol>`. The file component is the basename, so
output does not depend on the absolute build path.

### Hermeticity and the "not discharged" outcome (design)

`-vox-dump-vc` implies skipping discharge; there is no separate dry-run flag.
This is the cleaner design for expect tests: a single flag makes the dump fast
and hermetic (no Lean process, no network, works with Lean absent). To emit the
FULL VC set deterministically without a solver, each VC's goal is ASSUMED to
hold and added to the fact context (mirroring what `prove` does on `Proved`), so
downstream VCs still see it as a hypothesis. Consequently, a dump can emit more
VCs than a real aborting verification run: VCs after the first goal that would
fail are still reached. The emitted set matches what real verification would
walk only for runs in which every VC would prove.

Because nothing is actually discharged, the mode must never be mistaken for a
passing verification. It is therefore loud: after all blocks are printed,
`Vox_verify.finish_dump` writes `Error: VCs dumped, not discharged.` and raises,
so compilation exits non-zero (2) and writes no `.cmi`/`.cmo`; a partial `.cmt`
may be left under `-bin-annot`, like any failed compile. An editor or CI cannot
read a dry dump as "verified".

### Determinism

Output is deterministic across runs: the `display` printer uses binder NAMES,
not stamps; the file component is a basename; blocks are emitted in source-walk
order; no hash/set iteration order is exposed. No residual nondeterminism was
observed. (If a predicate node falls back to the raw prefix printer, that raw
form can include reference stamps like `global[x/281]`; the source-like infix
path used for the common comparison/arithmetic predicates does not.)

### Demonstration

For:

```ocaml
let positive (x : int{ _ > 0 }) = x
let annotation = (3 : int{ _ >= 3 })
let contract = positive 1
let branch y = if y > 0 then positive y else 0
```

`ocamlc -vox-dump-vc -c` writes (exit 2):

```text
VC annotation at d.ml:2:17-36
|- 3 >= 3

VC contract-argument at d.ml:3:24-25
  3 >= 3
  annotation >= 3
|- 1 > 0

VC contract-argument at d.ml:4:38-39
  3 >= 3
  annotation >= 3
  1 > 0
  y > 0
|- y > 0

Error: VCs dumped, not discharged.
```

This shows an annotation VC, contract-argument VCs, and (third block) a
branch-condition hypothesis `y > 0` carried into the fact context from
`if y > 0 then ...`. The `annotation >= 3` hypothesis is the top-level refined
binder `annotation` contributing its refinement to later VCs. The regression
test is `testsuite/tests/refinement/vc_dump.ml` / `.reference`.

## Predicate rendering

The `text` field remains the unchanged raw `Types.Refinement.print` rendering,
including prefix applications such as `(app[Stdlib!.>] _ 5)`. The additive
`display` field uses a new vox-local source-like printer: it resolves only the
primitive table shared with `Vox_lean`, strips module qualifiers, and renders
operators with precedence- and associativity-correct parentheses. Unsupported
subterms retain a faithful prefix fallback instead of being guessed into a
different source term. Raw fallbacks have the lowest display precedence, so a
compound fallback in operator or prefix-argument position is parenthesized and
cannot visually change the term's structure; standalone bound operator names
are parenthesized as function names. The positive theorem emitted for the VC
remains independently available in `generated_lean`.

This vox-local printer is a candidate for the compiler's own type/error
printing later; those diagnostics currently show forms such as
`int{ (app[Stdlib!.>] _ 5) }`. Wiring it into compiler diagnostics is
deliberately out of scope for this child because it would churn suite-wide
baselines and requires a separate user decision. No `out_type.ml` or error
printing path is changed here.

## Verification evidence

### Build

`make -s boot-compiler` builds clean with the round-2 changes applied.
`git diff --check` also passes. The known-broken sandbox `make -s fmt` target
was not used.

### Test suites (all green, no new flags in play)

These expect-tests run the compiler with no new flags and compare its output
against checked-in expected output. The separate byte-level default experiment
below covers the exact proved/disproved cases required by this review.

| suite | result |
|---|---|
| `refinement` | 14 passed, 0 failed, 0 unexpected errors |
| `refinement-lean` | 2 passed, 0 failed, 0 unexpected errors |
| `refinement-acceptance` | 14 passed, 0 failed, 0 unexpected errors |
| `refinement-examples` | 6 passed, 0 failed, 0 unexpected errors |
| `typing-modes` | 37 passed, 0 failed, 0 unexpected errors |

### Byte-identical default + additive dump (live)

Compiler used: `_build/_bootinstall/bin/ocamlc`.

`vcok.ml` = `let ok = (2 : int{ _ = 2 })` (VC proves):
- `ocamlc -c vcok.ml` (no flag): exit 0, **0 bytes** of output.

`vcbad.ml` = `let bad = (1 : int{ _ = 2 })` (VC disproves):
- `ocamlc -c vcbad.ml` (no flag): exit 2, exactly the standard located error
  (`Error: Refinement verification failed (disproved)`), 159 bytes.
- `ocamlc -vox-dump-vc-json vcbad.json -c vcbad.ml`: exit 2. The 159-byte
  standard located error on stderr is **byte-for-byte unchanged** and stdout is
  empty. `vcbad.json` is nevertheless written by the `at_exit` handler and
  parses strictly as JSON with one `disproved` VC. Its decoded `detail` contains
  the real `⊢` character, and the file contains the corresponding UTF-8 bytes.

A fake solver diagnostic beginning with a lone byte `0xFF` also produced a
strictly parseable document: the raw JSON contains `\u00FF`, not the malformed
byte. This covers the arbitrary-byte invariant independently of the live Lean
sample.

With `/nonexistent-dir/out.json` as the dump path, the proved input still exits
0 and the disproved input still exits 2. Both print one sidecar-write warning;
the I/O failure does not replace the compile result.

`grep -c schema_version` on both no-flag outputs is 0: the dump never appears
without the flag.

### Type-only fast pass

`-vox-type-only` short-circuits both `Vox_verify.verify_structure` calls
(typemod.ml:4151, :4471) and the `verify_seal_obligations` body, so no VC is
generated or discharged. Manual checks confirmed:

- An `.ml` with `-vox-type-only -c`: exit 0, silent, no output artifact.
- The same mode with explicit `-annot`: exit 0, silent, only `.annot` is
  produced.
- An `.mli` with `-vox-type-only -c`: exit 0 and no `.cmi`; its normal compile
  writes `.cmi`.
- A type-error `.ml` with `-vox-type-only -bin-annot -c`: normal exit 2 and no
  partial `.cmt`; its normal compile writes the partial `.cmt`.
- An `.ml` that disagrees with its compiled `.mli`: type-only exits 0 after
  checking the implementation alone; a normal compile reports the expected
  conformance error and exits 2.
- `-vox-type-only -i`: exit 0, prints
  `val ok : int{ (app[Stdlib!.=] _ 2) }` and produces no compiled artifacts.
- Combined with `-vox-dump-vc-json FILE`: exit 0, silent, and produces a valid
  document with an empty `verification_conditions` array.

### VC text dump

On the four-binding demonstration above, `ocamlc -vox-dump-vc -c d.ml`:
- exits 2 and prints the three blocks shown in "VC text dump", ending with
  `Error: VCs dumped, not discharged.`; it writes no `.cmi`/`.cmo`.
- is deterministic: two runs produce byte-identical output (`diff` empty).
- is Lean-independent: with `VOX_LEAN=/nonexistent/lean` the output and exit are
  unchanged, and by construction the `-vox-dump-vc` branch of `prove` /
  `verify_seal_obligation` never calls `Vox_lean.discharge`.
- the regression test `testsuite/tests/refinement/vc_dump.ml` passes (the
  `refinement` suite is 16 tests with it and the file-level seal regression
  `vc_dump_file_seal.ml`).
- a refined implementation behind a compiled `.mli` emits both its annotation
  block and a file-level `VC seal-implication` block before the final error.
- an `.mli`-only dump exits 2 with the same final not-discharged error.

### Why the default path is byte-identical (by construction)

- `record_vc` is called only when `Clflags.vox_dump_vc_json` is `Some _` in
  `prove` and `verify_seal_obligation`; the `Vox_lean.discharge` call and the
  verdict-matching / error-raising logic are unchanged and run in the same
  order.
- The `at_exit` handler is registered unconditionally but emits nothing unless
  a dump file was supplied; a supplied path receives the JSON document or
  causes a swallowed warning if the write fails.
- Provenance records are built by thunks forced only on the dump path; the
  new labeled arguments threaded through `prove`/`prove_refinement` and the
  call sites have no effect when the flag is off.
- `vox_type_only` defaults to `false`, so both verify gates run exactly as
  before, and the existing save/backend paths are unchanged.
- `vox_dump_vc` defaults to `false`, so `prove` / `verify_seal_obligation` take
  the unchanged `Vox_lean.discharge` path, and `Vox_verify.finish_dump` (called
  after typecheck in `compile_common`) is a no-op. The dump-only artifact-save
  gates in `Typemod.type_implementation` are therefore inert on the default
  path.
