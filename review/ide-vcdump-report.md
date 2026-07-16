# IDE VC dump report

## Status

Complete. Flag plumbing, per-VC capture, JSON serialization,
annotation/contract/seal provenance, and the type-only gate are implemented and
verified. The boot compiler builds; all required suites are green; the
byte-identical default and live JSON samples are confirmed (see Verification
evidence below).

## Flags and mechanism

- `-vox-dump-vc-json FILE` writes one JSON document to `FILE` at process exit.
  It does not write JSON to stdout or stderr. It records every VC actually
  passed to `Vox_lean.discharge`, including VCs processed before a later
  verification error aborts compilation.
- `-vox-type-only` skips both `Vox_verify.verify_structure` calls in
  `typing/typemod.ml` and skips seal-obligation verification.  It therefore
  generates and discharges no refinement VCs. It performs full typechecking,
  emits `.annot` when `-annot` is present, and prints the inferred signature
  when `-i` is present, but never writes `.cmi`, `.cmo`, or `.cmt` artifacts.

The flags are independent.  Combining them produces a valid document whose
`verification_conditions` array is empty.

The dump distinguishes the VC origins present in the implementation:
`annotation`, `contract-argument`, and `seal-implication`.  Branch conditions
are facts in an enclosing VC, not separately discharged VCs, and therefore
appear in `facts` with their spans instead of being assigned an invented VC
kind.

## JSON schema (version 1)

The top-level document is:

```text
{
  "schema_version": 1,
  "verification_conditions": [ VC, ... ]
}
```

Each `VC` has exactly these fields:

```text
{
  "location": Span,
  "program_point": Span,
  "kind": "annotation" | "contract-argument" | "seal-implication",
  "goal": Predicate,
  "facts": [ Predicate, ... ],
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

`Predicate` is:

```text
{ "text": string, "source_span": Span | null }
```

The goal always has a concrete `source_span`; fact spans can be `null` for
synthesized facts.  `Span` is:

```text
{
  "file": string,
  "start": { "line": integer, "column": integer },
  "end": { "line": integer, "column": integer },
  "ghost": boolean
}
```

Lines are 1-based and columns are 0-based byte offsets, matching OCaml
locations. `Misc.Json.string` encodes every string: quotes and backslashes use
the standard JSON escapes, and every byte outside printable ASCII (0x20-0x7e)
is encoded separately as `\u00HH`. This deliberately turns the UTF-8 bytes of
non-ASCII Lean output such as `⊢` and `→` into sequences of `\u00HH` escapes,
so arbitrary solver bytes (including invalid UTF-8) still produce valid JSON.

Provenance details by kind:

- `annotation`: `name` is null, `source_span` is the annotation span, and a
  `subject` related span identifies the checked expression.
- `contract-argument`: `name` is the parameter/binder name when available
  (falling back to the refinement view name for imported contracts),
  `source_span` is the contract predicate, and `argument` and `application`
  related spans identify the call site.
- `seal-implication`: `name` is the sealed value name, `source_span` is the
  seal point, and `interface` and `implementation` related spans identify the
  two declarations.

`generated_lean` is exactly the positive, non-negated theorem returned by
`Vox_lean.emit`; it does not include the negated theorem that `discharge`
constructs internally while checking for a disproof. It is null only when
`Vox_lean.emit` fails; in that case `emission_error` contains the failure.
`detail` is the unmodified solver diagnostic. For a `disproved` result,
`counterexample` repeats that diagnostic when it explicitly contains a
`counterexample` or `witness` marker; otherwise it is null. The
`Vox_lean.result.location` field is intentionally not serialized; the VC's
`location`, `program_point`, and provenance spans carry the source locations
exposed by schema version 1.

## Predicate rendering

Goals and facts use the existing `Types.Refinement.print` source-like printer.
This preserves refinement references and binder names and is easier for an IDE
user to read than extracting a term from an entire generated theorem.  The
positive theorem emitted for the VC is independently available in
`generated_lean`; as noted above, the internal negated disproof variant is not.

## Verification evidence

### Build

`make -s boot-compiler` builds clean with the change applied. `make -s fmt` was
also attempted, but the repository-wide target could not obtain its sandboxed
`patdiff` executable and reported unrelated pre-existing line-length failures;
it left no unrelated source changes, and `git diff --check` passes.

### Test suites (all green, no new flags in play)

Because these expect-tests run the compiler with no new flags and compare its
exact output against checked-in expected output, their passing is itself the
byte-identical-default proof: any perturbation of the default path would break
them.

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
- `ocamlc -vox-dump-vc-json vcok.json -c vcok.ml`: exit 0, **0 bytes** on
  stdout and stderr, and well-formed JSON in `vcok.json` with one `annotation`
  VC. The file contains only ASCII bytes.

`vcbad.ml` = `let bad = (1 : int{ _ = 2 })` (VC disproves):
- `ocamlc -c vcbad.ml` (no flag): exit 2, exactly the standard located error
  (`Error: Refinement verification failed (disproved)`), 159 bytes.
- `ocamlc -vox-dump-vc-json vcbad.json -c vcbad.ml`: exit 2. The 159-byte
  standard located error on stderr is **byte-for-byte unchanged** and stdout is
  empty. `vcbad.json` is nevertheless written by the `at_exit` handler and
  parses as JSON with one `disproved` VC. It contains only ASCII bytes; Lean's
  UTF-8 `⊢` bytes appear as `\u00E2\u008A\u00A2`.

`grep -c schema_version` on both no-flag outputs is 0: the dump never appears
without the flag.

### Type-only fast pass

`-vox-type-only` short-circuits both `Vox_verify.verify_structure` calls
(typemod.ml:4151, :4471) and the `verify_seal_obligations` body, so no VC is
generated or discharged. Manual checks with `vcok.ml` confirmed:

- `-vox-type-only -c`: exit 0, silent, no `.cmi`, `.cmo`, `.cmt`, or `.annot`.
- `-vox-type-only -annot -c`: exit 0, silent, only `.annot` is produced.
- `-vox-type-only -i`: exit 0, prints
  `val ok : int{ (app[Stdlib!.=] _ 2) }` and produces no compiled artifacts.
- Combined with `-vox-dump-vc-json FILE`: exit 0, silent, and produces a valid
  document with an empty `verification_conditions` array.

### Why the default path is byte-identical (by construction)

- `record_vc` is called only when `Clflags.vox_dump_vc_json` is `Some _` in
  `prove` and `verify_seal_obligation`; the `Vox_lean.discharge` call and the
  verdict-matching / error-raising logic are unchanged and run in the same
  order.
- The `at_exit` handler is registered unconditionally but emits nothing unless
  a dump file was supplied; when supplied, it writes only to that file.
- Provenance records are built by thunks forced only on the dump path; the
  new labeled arguments threaded through `prove`/`prove_refinement` and the
  call sites have no effect when the flag is off.
- `vox_type_only` defaults to `false`, so both verify gates run exactly as
  before, and the existing save/backend paths are unchanged.
