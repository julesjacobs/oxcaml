# Compiler report: refinement predicate subterm types in `-vox-dump-vc-json`

Implements the "type at cursor inside a refinement predicate" data channel
(Option B from `refine-type-at-cursor-rootcause.md`, Phase 1). Emit-only
addition to the compiler; no verification-behavior change.

## Commit / build

- Worktree: `/usr/local/home/jujacobs/oxcamls/vox2/worktrees/refine-types-emit`
- Branch: `refine-types-emit`
- Base commit: `9ebf427cf7` ("Refinement: per-branch VC location spans")
- Commit: **`ebedc4dec1`** (single file changed: `typing/vox_verify.ml`, +82/-4)
- Built compiler: `/usr/local/home/jujacobs/oxcamls/vox2/worktrees/refine-types-emit/_install/bin/ocamlc.opt`

## What changed (all in `typing/vox_verify.ml`)

A new optional top-level array `refinement_expression_types` on the
`-vox-dump-vc-json` document. One entry per `refinement_expression` node found
by walking each refinement type's `ref_pred`.

Mechanism:
- `collect_refinement_types structure` runs a `Tast_iterator` over the whole
  structure with only the `typ` method overridden. For every core type whose
  `ctyp_type` is `Trefine refinement`, it walks `refinement.ref_pred`. Because
  a refinement annotation survives typing as a core type whose `ctyp_type` is
  `Trefine` (the predicate Typedtree is discarded during lowering; the lowered
  `refinement_expression` is retained in the type), this covers every
  *syntactic* refinement in the buffer.
- `collect_refinement_expression ~env pred` recurses over the 8
  `refinement_expression_desc` constructors, emitting `{location, type}` for
  each node from its stored `rexp_loc` / `rexp_type`. The hole `_` is an
  `Rexp_ident` node, so it gets its own entry.
- Types render with `render_type` = `Printtyp.type_expr` inside
  `Printtyp.wrap_printing_env ~error:true env`, so refinement types print
  source-like (`int{ _ > 0 }`) via the same predicate printer `Vox_lean`
  installs — never the raw `(app[...] ...)` AST.
- Gathered **per refinement type encountered during translation, not per VC**:
  the collector is independent of `Vox_lean.discharge`, so a refined parameter
  that produces no obligation still contributes cursor types (verified below:
  `let f (x : int{ _ > 0 }) = x` yields 0 VCs but 4 subterm-type entries).
- Gating / additivity: `collect_refinement_types` is called only when
  `!Clflags.vox_dump_vc_json` is `Some`, and the `at_exit` writer omits the
  `refinement_expression_types` field entirely when the list is empty. So for
  any input with no refinements the JSON is byte-identical to before.
  `schema_version` stays `2` (the field is purely additive/optional; no
  consumer that ignores unknown fields is affected).

## JSON schema of the new array

```
"refinement_expression_types": [        // omitted entirely when empty
  {
    "location": {                       // same Span shape as other locations
      "file":  <string>,
      "start": { "line": <int, 1-based>, "column": <int, 0-based byte> },
      "end":   { "line": <int>,          "column": <int> },
      "ghost": <"true"|"false">
    },
    "type": <string>                    // source-like rendered rexp_type
  },
  ...
]
```

Ordering is the recursion order (a node precedes its children); the client
selects by smallest containing span, so order is not load-bearing.

## Validation evidence (built `ocamlc.opt`, `TMPDIR` on the big disk)

### 1. Subterm spans + source-like types

Input `t_refine.ml`: `let f (x : int{ _ > 0 }) = x`
Column map (0-based): `_`=16, `>`=18, `0`=20; predicate `_ > 0` = cols 16..21.

`ocamlc.opt -c -vox-dump-vc-json refine.json t_refine.ml` produced entries:

| span (cols) | subterm  | type                |
|-------------|----------|---------------------|
| 16–21       | `_ > 0`  | `bool`              |
| 18–19       | `>`      | `int -> int -> bool`|
| 16–17       | `_`      | `int`               |
| 20–21       | `0`      | `int`               |

The predicate subterms `_`, `0`, and `_ > 0` are each covered by an entry whose
loc matches the source columns, with source-like types. (The operator `>` also
gets its own faithful entry.)

Nested `let f (x : int{ _ > 0 && _ < 10 }) = x` produces 10 entries covering
`_>0&&_<10` (bool), `&&` (bool->bool->bool), `_>0` (bool), `>`, `_`, `0`,
`_<10` (bool), `<`, `_`, `10` — all spans/types correct.

### 2. Byte-identical (no refinements) + only-new-array delta (with refinements)

Baseline binary: `worktrees/scratch-h3/_install/bin/ocamlc.opt` (also at
`9ebf427cf7`).

- No-refinement input (`let g = 5 / let h x = x+1 / List.map ...`):
  `cmp base_norefine.json new_norefine.json` → **identical** (0 diff). The new
  compiler emits no `refinement_expression_types` key at all.
- Refinement input (`positive`/`annotation`/`contract`): the only key added is
  `refinement_expression_types`; `schema_version` and `verification_conditions`
  are structurally identical between base and new (8 subterm entries added).

### 3. Independence from VC discharge

`t_refine.ml` (`x` is returned unrefined, no obligation is generated):
`verification_conditions` length = **0**, `refinement_expression_types` length
= **4**. Confirms the map is gathered from encountered refinement types, not
from VCs.

### 4. Refinement test suite

`make -s test-one DIR=refinement` (final flambda2 compiler + stdlib):

```
23 tests passed / 0 skipped / 0 failed / 0 unexpected errors
```

Includes `vc_dump.ml` and `vc_dump_file_seal.ml` (the existing dump tests).
`make -s boot-compiler` also clean.

## Notes / deferred

- No new compiler expect test was added: `-vox-dump-vc-json` writes to a file
  (not stderr) and its `file` field embeds an absolute source path, so it is
  exercised via the IDE harness rather than ocamltest, matching the existing
  pattern (the JSON dump has no compiler expect test today; only the stderr
  `-vox-dump-vc` does). The four validations above are the on-binary evidence.
- Server/client wiring (`compiler.py` reading the new array; `app.js` merging
  the ranges into `expressionTypes`) is the IDE-side follow-on, out of scope
  for this compiler-only stage.
- Mode-at-cursor (`type @ mode`) is Phase 2 and untouched here.
