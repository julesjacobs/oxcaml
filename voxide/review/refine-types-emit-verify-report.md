# Independent verification: `refinement_expression_types` emit (commit ebedc4dec1)

**Lane 2 of a dual-verify.** Re-derived every claim independently; did not rely
on the implementer's report.

- Under review: `ebedc4dec1` on `refine-types-emit`, base `9ebf427cf7`, file
  `typing/vox_verify.ml` (+89/-4).
- New binary: `worktrees/refine-types-emit/_install/bin/ocamlc.opt`
- Baseline binary (same base, without change):
  `worktrees/scratch-h3/_install/bin/ocamlc.opt`
- All runs `TMPDIR=/usr/local/home/jujacobs/tmp`.

## Overall verdict: **CONFIRM** (emit-only; safe to swap live + publish)

Every item below passed. The one nuance (item 4) is a pre-existing property of
the whole dump, not introduced by this change.

---

## Static read

`collect_refinement_types` runs in `verify_structure`'s `walk_root` **before** the
main verification iterator, and only when `Option.is_some !Clflags.vox_dump_vc_json`
(`vox_verify.ml:1152`). It is a `Tast_iterator` with only the `typ` method
overridden; for each `core_type` whose `ctyp_type` is `Trefine`, it walks
`ref_pred` read-only and appends `{location, type}` JSON to a fresh ref
`refinement_expression_types`. It never touches `state`, VC generation, or
`dumped_vcs`. The `at_exit` writer appends the field only when the list is
non-empty (`List.rev … | [] -> []`), and `schema_version` stays `2`. The span
helper is the identical `json_span` used by every VC/fact/error location
(`vox_verify.ml:103`), column = `pos_cnum - pos_bol`. So the change is
structurally emit-only; the checks below confirm it empirically.

## 1. Byte-identical for no-refinement inputs — CONFIRM

Three distinct non-refinement inputs (plain lets/`List.map`; record + `rec`;
nested module + `Printf`). For each, `cmp` of base vs new `-vox-dump-vc-json`
output = **identical (0 diff)**, and `grep -c refinement_expression_types` on the
new output = **0** (key absent when there are no refinements).

## 2. Additive for refinement inputs — CONFIRM

`r_simple` (`_ > 0`), `r_nested` (`_ > 0 && _ < 10`), `r_mix` (the 4-line
`vc_dump.ml` body). `diff base→new` is a **pure append** (`a` hunk only, no
`c`/`d`). Parsed structurally:

| input | schema_version equal | verification_conditions equal | keys added | keys removed |
|-------|------|------|------|------|
| r_simple | yes | yes | `{refinement_expression_types}` | none |
| r_nested | yes | yes | `{refinement_expression_types}` | none |
| r_mix    | yes | yes | `{refinement_expression_types}` | none |

The only textual delta is the new key (plus the JSON comma the extra field
requires); all outputs are valid JSON (python `json.load`).

## 3. No verification-behavior change — CONFIRM

- **Normal verify (no dump)**, base vs new: stderr **identical** and exit code
  identical on `r_mix`/`r_simple` (exit 0, all proved), `r_fail` (exit 2,
  disproved), `r_rich` (exit 2, proved + not-proved).
- **Dump mode VC bodies**: `verification_conditions` initially differed *only*
  in a randomized temp filename embedded in `discharge.detail`
  (`vox2-vc<hex>.lean`) — this differs **base-vs-base across two runs too**, i.e.
  pre-existing nondeterminism, not a behavior change (status stayed `disproved`).
  After normalizing that path, base vs new `verification_conditions` are
  **byte-identical**, including per-VC `discharge.status` and `generated_lean`:

  | input | statuses (base = new) | VC bodies equal (norm) | generated_lean equal |
  |-------|------|------|------|
  | r_fail | `[disproved]` | yes | yes |
  | r_rich | `[proved, not-proved]` | yes | yes |
  | r_mix  | `[proved, proved, proved]` | yes | yes |

## 4. Faithful entries — CONFIRM (with one honest nuance)

`let f (x : int{ _ > 0 }) = x` — every predicate subterm covered with exact
source columns and source-like types:

| span (L1 cols) | subterm | type |
|---|---|---|
| 16–21 | `_ > 0` | `bool` |
| 18–19 | `>` | `int -> int -> bool` |
| 16–17 | `_` | `int` |
| 20–21 | `0` | `int` |

Verified against the source byte offsets (`_`@16, `>`@18, `0`@20). The nested
`_ > 0 && _ < 10` yields 10 entries all mapping exactly (e.g. `&&`@22–24 =
`bool -> bool -> bool`, `_ < 10`@25–31 = `bool`, `10`@29–31 = `int`).

No emitted `type` string contains raw-AST tokens (`app[`, `constructor[`,
`Trefine`, `Rexp_`) across any refinement input — grep returns nothing. Types
render through the same `Printtyp.type_expr` + `Vox_lean` predicate printer used
by the display path (already verified source-like in task #141), so a
refinement-typed subterm would render source-like by construction. I could not
construct a *valid* predicate whose subterm is itself refinement-typed (the
language rejects `incr _ > 0` / binder-reference forms — "invalid lowered
refinement predicate"), so this path is exercised only by the shared printer, not
directly; not a defect of this change.

**Convention nuance:** columns are 0-based **byte** offsets (`pos_cnum -
pos_bol`), not UTF-16. This is emitted by the *same* `json_span` used for every
other span in the dump, so it matches the dump's existing spans exactly. If the
IDE ever needs UTF-16 columns, that is a pre-existing property of the whole VC-JSON,
untouched (and unaffected) by this change.

## 5. Independence from VCs — CONFIRM

`let f (x : int{ _ > 0 }) = x`: `verification_conditions` length **0**,
`refinement_expression_types` length **4**. A refined param that generates no
obligation still yields subterm-type entries.

## 6. Build + suite — CONFIRM

`make -s test-one DIR=refinement` (TMPDIR on big disk):
**23 tests passed / 0 skipped / 0 failed / 0 unexpected errors** (23 considered),
including `vc_dump.ml` and `vc_dump_file_seal.ml` (the JSON/text dump tests).

---

### Bottom line
The change adds one optional, gated, purely additive output field. It is
byte-identical for non-refinement inputs, additive for refinement inputs, and
provably does not alter VC generation, discharge outcomes, or generated Lean.
Entries are faithful (correct spans, source-like types, same span convention as
the rest of the dump). **CONFIRM.**
