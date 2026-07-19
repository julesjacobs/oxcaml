# Fade-unused hypotheses: implementation report

Makes vox1's "fade the hypotheses the proof did not use" feature real in vox2.
Design followed: `design/fade-unused-hypotheses-rootcause.md`, Option A (ride
Lean's `unusedVariables` linter on the existing single grind discharge run —
zero extra Lean processes). Fade-only, display-only, verdict unchanged.

## Deliverables

- Compiler change: branch `fade-unused`, commit `0fff71f089` (parent
  `ebedc4dec1`, the live `refine-types-emit` compiler).
  Built compiler: `/usr/local/home/jujacobs/oxcamls/vox2/worktrees/fade-unused/_install/bin/ocamlc.opt`
- IDE adapter change: branch `vox2-ide-integrated`, commit `fb5225d930`
  (`voxide/compiler.py` only).
- IDE client (`voxide/pane_model.js`) already consumes `used`
  (`used: !raw || raw.used !== false`; `faded: fadeUnused && !hyp.used`) — no
  client change beyond compiler.py, as the design predicted.

## What changed

Compiler (`0fff71f089`, 3 files):
- `typing/vox_lean.ml` `emit_internal`: new `?linter` param. When on, emit
  `set_option linter.unusedVariables true` **in place of** the blank separator
  line, so the theorem keeps its line number (no line-number drift in any Lean
  error `detail`), and the `linter=false` emission stays byte-identical.
- `run_lean`: `Process_succeeded` now carries the captured stdout/stderr
  (previously discarded on success).
- `discharge`: the positive proof run is emitted with `~linter:true`; on
  success its output is scanned by `parse_unused_facts`, whose indices go into
  the new `result.unused_facts`. Strict match: `` Variable name `h_<digits>` ``
  immediately followed by the exact suffix `is not explicitly referenced` — so
  scope vars (`v_*`, `g_*`), `h_exhK`, and any unrelated warning can never fade
  a fact. Non-proved verdicts leave `unused_facts = []`.
- `typing/vox_lean.mli`: `result` gains `unused_facts : int list`.
- `typing/vox_verify.ml`: `json_fact` takes the index + `unused_facts` and
  emits `"used": <bool>` (`not (List.mem index unused_facts)`); `record_vc`
  switches `List.map` → `List.mapi`. A local `json_bool` helper (Misc.Json has
  no bool).

IDE adapter (`fb5225d930`, `voxide/compiler.py`): both `_hypothesis` builders
now emit `"used": bool(fact.get("used", True))` instead of hardcoded `True`.
Absent field (older compiler) defaults to used — never a spurious fade.

## Validation (evidence)

### Step 1 — linter reproduces on vox2's pinned Lean 4.31.0
`/nix/store/h6z4nr52r2x6v7ygqg59cl8nzjg0yxcy-lean4-4.31.0/bin/lean` on a file
mirroring vox2's output (two hyps `h_0`(x>0), `h_1`(x>5); goal uses only h_0)
with `set_option linter.unusedVariables true` + plain `grind`:
```
fade_probe.lean:4:56: warning: Variable name `h_1` is not explicitly referenced.
EXIT=0
```
`h_1` (unused) flagged, `h_0` (needed) not, warning does not change exit code.

### Step 2 — used:false on a genuinely-unused hyp
Probe: `let f (x:int{_>0}) (y:int{_>5}) = (x:int{_>0})` and the mirror
`let g (x:int{_>5}) (y:int{_>0}) = (y:int{_>0})`. New compiler dump:
```
VC 0  goal x>0   facts: [x>0 used=True, y>5 used=False]
VC 1  goal y>0   facts: [x>5 used=False, y>0 used=True]
```
i.e. the vox1 `[true,false]` and `[false,true]` patterns. Both still `proved`.

### Step 3 — verdict-invariance + emit-additive across the refinement suite
Ran base (`refine-types-emit/_install`) vs new on all 54 `testsuite/tests/refinement/*.ml`
(8 produce VCs, 16 VCs, 5 used / 9 unused facts observed):
- exit-code mismatches: **none**
- discharge status mismatches: **none** (verdict-invariance holds)
- non-`used` JSON diff after normalizing the inherently random
  `Filename.temp_file` path (`vox2-vcXXXX.lean`, which the base itself varies
  run-to-run): **none** — the `used` field is the ONLY structured change.
- `generated_lean` (the user-facing emission, `linter=false`) is
  byte-identical and contains no linter line.

`make -s test-one DIR=refinement` with the final compiler: **23/23 passed, 0
failed** (includes `vc_dump` / `vc_dump_file_seal`, whose textual `-vox-dump-vc`
references are unaffected — `used` is JSON-only).

### Step 4 — end-to-end through the pane pipeline
`node tools/voxide-pane.js probe.ml --ocamlc <new> --line L --col C` (live:
new ocamlc → committed compiler.py → pane_model), reading the view-model
`faded` flags:
- VC0 pane (`f`): `x` faded=False, `y` faded=True  (y>5 faded)
- VC1 pane (`g`): `x` faded=True,  `y` faded=False  (x>5 faded)
- Control: `--fade off` → both faded=False (fade driven by data + toggle).

## Honesty / soundness invariants held
- Verdict decided by grind before warnings are read; linter never changes exit
  status (proved across the whole suite; statuses identical to base).
- Fade only a hyp the linter explicitly names; default used on absent/parse
  miss/non-proved; a hypothesis is never removed from the dump.
- Only the `used` field is added to the VC-JSON; generated_lean unchanged.

## Note on the pane-fidelity suite (not caused by this change)
`tests/test_pane_fidelity.js` shows 8/28 failing on the integrated tree. These
are all `app.js` DOM-structure comparisons (`verdict-token` vs `goal`) driven
by the **uncommitted** in-flight redesign of `app.js`/`pane_model.js` (the
concurrent "label anonymous hypotheses / goal-line" work). They are orthogonal
to this change (commit `fb5225d930` touches only `compiler.py`); the fade
end-to-end above ran against that same in-flight `pane_model.js` and produced
correct `faded` flags.
