# Fade-unused hypotheses — independent dual-verify (lane 2)

Overall verdict: **CONFIRM**. Every claim re-derived independently below; all
five items pass. Safe to swap the live :8471 compiler + publish to PR#65.

Artifacts verified:
- Compiler `0fff71f089` (branch `fade-unused`, parent `ebedc4dec1`), binary
  `/usr/local/home/jujacobs/oxcamls/vox2/worktrees/fade-unused/_install/bin/ocamlc.opt`
- Baseline `/usr/local/home/jujacobs/oxcamls/vox2/worktrees/refine-types-emit/_install/bin/ocamlc.opt`
- IDE adapter `fb5225d930` (`voxide/compiler.py` only) on `vox2-ide-integrated`

---

## Item 1 — VERDICT-INVARIANCE + EMIT-ADDITIVE: CONFIRM

Ran both binaries on all 54 `testsuite/tests/refinement/*.ml` with
`-c -vox-dump-vc-json`. Results (independent script):
- .ml producing VCs: 8. exit-code mismatches: **0**. discharge-status
  mismatches: **0** (verdict-invariance holds).
- After stripping the new `used` field and normalizing the pre-existing random
  `vox2-vcXXXX.lean` temp path in `discharge.detail`, the **entire VC-JSON is
  byte-equal** between new and base across the whole suite → `used` is the ONLY
  structured change.
- `generated_lean` identical per-VC, and contains no `linter.unusedVariables`
  line.

Root-cause confirmed in code: `generated_lean` is produced by `Vox_lean.emit`
(`vox_lean.ml:1102`), which calls `emit_internal ~negated:false` with no
`~linter` → default `false`. The `linter=false` branch emits a bare `'\n'`, so
the header stays `set_option autoImplicit false\n\n` — byte-identical to base
(`vox_lean.ml:1056-1064`). Only the positive discharge run passes
`~linter:true` (`vox_lean.ml:1241`), and that emission never reaches the JSON.

## Item 2 — LINTER-PARSE CORRECTNESS + CONSERVATISM: CONFIRM

Probe `let f (x:int{_>0}) (y:int{_>5}) = (x:int{_>0})` and mirror `g`, plus
all-used `h (x+y)`:
- VC0 goal `x>0`: `x>0` used=true, `y>5` used=**false** (genuinely unused)
- VC1 goal `y>0`: `x>5` used=**false**, `y>0` used=true (mirror)
- VC2 goal `x+y>0`: both used=true (both needed)

`parse_unused_facts` (`vox_lean.ml:1198`) matches marker ``Variable name `h_``
then requires ≥1 digit then the exact suffix `` ` is not explicitly
referenced``. Therefore scope vars (`v_*`,`g_*`), `h_exhK`, and any partial/
unrelated warning can never fade a fact; default is used:true (empty
`unused_facts` on any non-proved verdict and when the linter is silent).

Load-bearing index alignment verified: `emit_internal` numbers facts
`(h_N …)` via `List.iteri` over `vc.facts` (`vox_lean.ml:1086-1094`); `json_fact`
attributes `used` via `List.mapi` over the *same* `vc.facts`
(`vox_verify.ml:252-255`, `used = not (List.mem index unused_facts)`). Same list,
same 0-based index ⇒ `h_N` ↔ fact position N. Empirically confirmed (VC0's 2nd
fact / index 1 faded; VC1's 1st fact / index 0 faded).

## Item 3 — HONESTY (display-only, conservative, never drops): CONFIRM

- Verdict is decided before warnings are read: `Process_succeeded detail ->
  result Proved ~unused_facts:(parse_unused_facts detail)` (`vox_lean.ml:1245`).
  `Proved` is unconditional on parse output; the Lean linter emits warnings, not
  errors, so exit status stays 0. Suite-wide statuses identical to base (item 1).
- No hypothesis is ever removed — the full `vc.facts` list is still emitted; only
  a per-fact `used` bool is added.
- Attempted to find a needed hyp faded: transitive probe
  `let k (x:int{_>0}) (y:int{_>x}) = (y:int{_>0})` → both `x>0` and `y>x`
  used=**true** (proof needs both; neither faded). Trivial probe
  `(1:int{_>0})` with two irrelevant hyps → both used=false (honest).
  A needed hyp is never marked unused: the linter only flags binders absent
  from the found proof term.

## Item 4 — END-TO-END via /vcs pipeline: CONFIRM

Called `compiler.vcs_for_source` (the exact function the server's `/vcs`
endpoint runs) with the committed `compiler.py` (`fb5225d930`):
- New ocamlc: VC0 `y>5` faded (used=false), `x>0` used=true; VC1 mirror; VC2
  both used=true — the `used` bits flow compiler dump → compiler.py → payload.
- Baseline ocamlc (emits no `used`): every hypothesis `used=true`
  (`bool(fact.get("used", True))` fallback), i.e. no spurious fade with an older
  compiler.
(Per instructions, pane visual rendering / `test_pane_fidelity` not judged —
orthogonal, owned by the mid-redesign lane.)

## Item 5 — `make -s test-one DIR=refinement` on 0fff71f089: CONFIRM

**23 tests passed, 0 failed, 0 skipped** (exit 0), including `vc_dump` and
`vc_dump_file_seal` (textual `-vox-dump-vc` path unaffected — `used` is
JSON-only).

---

Both diffs (`0fff71f089` = vox_lean.ml/.mli + vox_verify.ml; `fb5225d930` =
compiler.py) match the implementer report exactly. No discrepancies found.
