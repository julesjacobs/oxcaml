# IDE proof-pane corrected re-audit

Corrected re-audit of the vox2 IDE proof pane after the two verified
pane-correctness fixes landed: the IDE-side attribution/honesty batch
(C1/B1/H1/H2/H3-tiebreak/H4/S1-clarity/CL1/CL4) and the display-fixed compiler
(refinement predicates print source-like). It supersedes the earlier audit,
whose SHOWN column was a static model that never invoked the tool and so
reported deviations the fixes have removed.

Environment: `VOX2_OCAMLC` = the display-fixed compiler
(`worktrees/scratch-printer-fix/_install/bin/ocamlc.opt`, branch tip
0ff1a7432d), `TMPDIR=/usr/local/home/jujacobs/tmp`, node v18. Tree
vox2-ide-integrated (fixes at d023904c54).

## Harness fix (SHOWN is now the real tool)

`review/caret_attribution_audit.py` previously modelled SHOWN with a hardcoded
static rule — program_point-first anchor, half-open `[start, end)` membership —
and never ran the tool, so it kept scoring the pre-fix behavior (150/861). SHOWN
now IS the tool's output: at each caret it drives

    node tools/voxide-pane.js --vcs-json <fixture> --line L --col C --json [--file U]

and reads the goal the pane actually attributes (`obligation.goalDisplay`, or `·`
when the pane is not on an obligation). OPTIMAL is unchanged — location-first
(skip ghost), inclusive `[start, end]`, innermost, id tiebreak — computed from a
fresh `-vox-dump-vc-json`. Comparison is by goal display, the user-facing
attribution identifier and the same granularity the old harness used (so the
150→0 change is apples-to-apples). A stale-fixture guard compares each fixture's
goal-display multiset against the fresh dump and prints a loud warning on
divergence; it stayed silent for every unit here (fixtures are fresh).

The raw dump's 1-based line / 0-based column map to the tool's `--line L`
(1-based) / `--col L = col+1` for these ASCII examples; verified against the
`abs` annotation (dump L8/col9 == tool --line 8 --col 10 == goal `x >= 0`).

## 1. Attribution: 0 / 861 carets deviate (was 150 / 861)

Every interesting caret across all 13 single-file examples and both units of the
xmod workspace now shows exactly the optimal obligation. Per unit:

| unit | dev/total | unit | dev/total |
|---|---|---|---|
| overview | 0/23 | recursion | 0/92 |
| abs | 0/48 | multi_arg | 0/95 |
| binder | 0/53 | multi_param | 0/86 |
| guard | 0/34 | nested_call | 0/68 |
| dependent | 0/22 | predicate_forms | 0/62 |
| counterexample | 0/23 | unproved | 0/45 |
| proof_tour | 0/82 | xmod Lib.ml | 0/76 |
|  |  | xmod Client.ml | 0/52 |

**TOTAL: 0 / 861.**

### Expected residue (H3, compiler-side #144) — not a deviation

The then/else whole-if-span case is expected residue and does NOT surface as a
deviation here, by construction. Example `abs` has two branch obligations that
carry the SAME compiler `location` span — the whole annotation:

    id 0  span L7 c3–c30  goal  x >= 0        (then branch)
    id 1  span L7 c3–c30  goal  0 - x >= 0    (else branch)

Because both the tool's `vcOrder` and OPTIMAL resolve this tie the same
deterministic way (innermost, then id → id 0), SHOWN == OPTIMAL == `x >= 0` at
every caret in the span, so it scores 0. The genuine limitation — the
else-branch obligation cannot be reached by clicking in the else region — is
compiler-side coarseness tracked as #144 (per-branch VC location spans), not a
tool/optimal attribution mismatch and not a regression. This audit measures
tool-vs-optimal; it cannot (and should not) charge the pane for a span the
compiler emits coarsely.

## 2. Display raw leaks: 0

Regenerated `tests/fixtures/*.vcs.json` + `xmod.workspace.json` through
`compiler.py` with the display-fixed compiler (`tests/regen_fixtures.py`;
4 of 14 fixtures changed: abs, predicate_forms, recursion, xmod.workspace).
Grepped every goal + hypothesis DISPLAY for `app[`, `constructor[`, `Stdlib!.`,
`global[`, `.field[`, and stamps (`/NN!`):

- 77 display fields across all fixtures: **0 leaks**.
- 53 distinct rendered pane bodies (tool, default compact mode): **0 leaks**.

The intentional `[raw predicate]` disclosure (full mode only) still carries raw
app-syntax by design; it is not a default-view display and is out of scope for
this grep. Concrete before/after from the display fix:

- abs branch fact: `(if (app[Stdlib!.>=] x 0) then constructor[bool/6!.false] …)`
  → `if x >= 0 then false else true`.
- xmod Client.ml goals: `one (constructor[unit/7!.()]) > 0` →
  `Lib.one () > 0`; `pos (one (constructor[unit/7!.()])) > 0` →
  `Lib.pos (Lib.one ()) > 0`.
- `-i` signature: `val three : int{ (app[Stdlib!.>=] _ 3) }` → `int{ _ >= 3 }`.

## 3. Honesty confirmations (all flipped, all locked)

- **H1** — no fabricated `a : 3 > 0`. At `multi_arg` L8 c45 the context view
  shows the real binders `a : a > 0` and `b : b > 0` (each once, deduped) and
  never the concrete call-site value `3` mislabeled as parameter `a`.
  (`test_pane_regressions.js` H1/H2 block.)
- **H4** — Client.ml does not pool Lib obligations. Without `--file`, the tool
  follows `payload.active` (Client.ml) and Lib.ml's `x > 0` obligation does NOT
  appear; Client's own `0 > 0` does; and `--file Lib.ml` does surface `x > 0`.
  (`test_pane_regressions.js` H4 block, end-to-end CLI.)
- **S1-clarity** — an errored unit is not shown "verified". A buffer that fails
  to compile and yields no placeable obligations (`let x : int = true`) returns
  `unavailable: true` (pane: "verification data unavailable"), never a clean
  "no obligations / verified" false-green.
  (`test_compiler.py::RealCompilerVcTests.test_errored_buffer_with_no_obligations_is_unavailable`.)

## 4. Anti-drift suites (all green with the regenerated fixtures)

| suite | result |
|---|---|
| `tests/test_pane_fidelity.js` | 22 / 22 checks pass (CLI == DOM byte-match) |
| `tests/test_pane_regressions.js` | 21 / 21 checks pass (C1/B1/H1/H2/tiebreak/CL1/CL4/H4) |
| `tests/test_frontend.js` | all checks pass |
| `python3 -m unittest discover -s tests` | 104 / 104 pass |

One test expectation was updated as a direct consequence of the display fix:
`test_good_refinements_produce_types_and_signature` asserted the pre-fix raw
`val three : int{ (app[Stdlib!.>=] _ 3) }` in the inferred signature; it now
asserts the source-like `int{ _ >= 3 }` the `-i` printer produces. This is the
fix reflected in the test, not a masked failure.

## Verdict

The pane attributes optimally at every audited caret (0/861), shows no raw
compiler internals in any default view (0 leaks), the three honesty properties
hold, and every suite is green against the freshly regenerated fixtures. The
only residue is the compiler-side #144 per-branch span, which is expected and
not measurable as a tool/optimal deviation.
