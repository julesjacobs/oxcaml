# Consolidated compiler — independent verification (lane 2)

**Verdict: CONFIRM.** The merge preserved all four verified features, introduced
no cross-feature interaction, and did NOT reintroduce the defeq forgery hole.
Safe to swap the live `:8471` compiler and publish to PR#65.

- **Under review:** commit `7c9ccc2e4b` on `consolidate-defeq`
- **Binary tested:** `worktrees/consolidate/_install/bin/ocamlc.opt`
- **Baselines:** `worktrees/seal-anchor/_install/bin/ocamlc.opt` (`d0d506a52c`,
  fade+seal = current live) and `worktrees/defeq-build/_install/bin/ocamlc.opt`
  (`bdc38e6a7b`, defeq alone)
- All probes independently authored (not reused from the merge lane's report),
  run with `TMPDIR=/usr/local/home/jujacobs/tmp`. Live `:8471` untouched.

Verification was done both ways: I re-read the load-bearing source in the merged
tree AND exercised the merged binary end-to-end. Source alone is not evidence a
cherry-pick landed intact; the binary is.

---

## 1. DEFEQ FORGERY STILL DEAD — CONFIRM (the #1 soundness re-check)

**Source (merged tree).** The verify-skip is keyed on unforgeable expander
provenance, not on any spellable attribute:
- `typing/vox_defeq.ml:40-43` — `generated_lemma_locations : Location.t list ref`
  and `is_generated_lemma_loc loc` matching by **physical identity** (`==`).
- `typing/vox_defeq.ml:117-118` — the expander mints a fresh ghost `loc` and
  records that exact object.
- `typing/vox_verify.ml:638-639` — `is_def_axiom_binding binding =
  Vox_defeq.is_generated_lemma_loc binding.vb_loc`.
- `typing/vox_verify.ml:897` and `:1128` — both `walk` sites guard with
  `if not (is_def_axiom_binding binding) then walk_expression …`.

A hand-written binding's `vb_loc` is a distinct physical object produced by the
lexer, so it can never `==` a recorded ghost loc — the skip is unreachable from
source. A lost-identity false negative would only over-reject a genuine lemma,
never admit an unverified one (fails closed). The mechanism is intact and
identical in spirit to the standalone defeq lane's fix.

**Binary (empirical).**
| Probe | Result |
|---|---|
| `let bad (x:int)=(():unit{0=1}) [@@vox.def.axiom]; let()=bad 0; (7:int{_=99})` | **REJECTED** — `verification failed (disproved)` at the `0=1` axiom |
| `let[@vox.def.axiom] forged (x:int)=(():unit{0=1}); …` | **REJECTED** — `disproved` at the axiom |
| genuine `let[@vox.def] double x=x+x`; `let()=double_def 5`; `(double 5:int{_=10})` | **ACCEPTED** (proved) |
| same, `(double 5:int{_=11})` | **REJECTED** — `disproved` |
| genuine `double` WITHOUT the `double_def 5` deposit, `(double 5:int{_=10})` | **REJECTED** — `not-proved` (opaque; `double` stays uninterpreted) |

The forged `[@@vox.def.axiom]` / `let[@vox.def.axiom]` never deposits `0=1`; both
are verified normally and fail. The genuine flow deposits the equation and proves
`double 5 = 10` while disproving `= 11`. Provenance fix survived the cherry-pick.

## 2. FADE intact — CONFIRM

`f (x:int{_>0}) (y:int{_>100}) : int{_>=0}` (goal `x >= 0`): in the VC-JSON the
`x > 0` fact carries `used:true`, the irrelevant `y > 100` carries `used:false`.
`generated_lean` for the VC is **identical** to the seal-anchor baseline, and the
**entire** VC-JSON document for this input is **byte-identical** to the baseline
(`diff` clean). Fade behavior and Lean emission unchanged by the merge.

## 3. SEAL intact — CONFIRM

Covariant-weaken repro (`Lib.mli: val f : int -> int{_>=0}` +
`Lib.ml: let f (x:int):int{_>0}=1`): VC-JSON contains a `kind=seal-implication`
VC whose goal is `value >= 0` over subject `value`, with fact `value > 0`,
`file=lib.ml` (the `.ml`, not the `.mli`), discharge `proved`. The full seal
VC-JSON is **byte-identical** to the seal-anchor baseline. Seal re-anchoring
preserved.

## 4. NOT-C display + fallback — CONFIRM

`clamp (n:int) = (if n > 0 then n else 100 : int{_>=0})` (`-vox-dump-vc`):
- else-branch renders `not (n > 0)` — no `if c then false else true`.
- then-branch hypothesis `n > 0` unchanged; both branch spans tight (`1:37-38`,
  `1:44-47`).
- **Fallback:** with `not` shadowed (`let not x = x`), the else fact falls back to
  the literal `if n > 0 then false else true`. The fallback is built from
  constructors directly (it does NOT call the user's shadowed `not`), so the
  branch fact is genuine negation and never lost — soundness-preserving.

## 5. PART 1 totality — CONFIRM

With `let expects_total (f @ total) = f`: `+`, `*`, `land` accepted `@ total`
(exit 0). `/` rejected — `The value (/) is partial`. `mod` rejected —
`The value \#mod is partial`. Both partial-op rejections point at the operator.

## 6. NO INTERACTION / additive — CONFIRM

The merged VC-JSON is exactly the **union** of the individual features' additive
changes, with nothing unexpected:
- **Plain, no-refinement input** (`let add a b = a+b; …`): VC-JSON
  **byte-identical** to the seal-anchor baseline.
- **fade input** and **seal input**: VC-JSON **byte-identical** to seal-anchor
  baseline (§2, §3).
- **defeq input vs the defeq-alone baseline** (`bdc38e6a7b`): the *only* delta is
  the added `"used": true` field — i.e. defeq's output picks up fade's additive
  field, which the defeq-alone binary lacks. No other change. This is precisely
  additivity: `merged = defeq ∪ fade(used) ∪ seal(re-anchor)`.
- Discharge statuses unchanged across every probe and the full suites (§7).

The three-lane overlap file `typing/vox_verify.ml` shows fade, seal, and defeq
occupying disjoint regions (`json_fact`/`used`, `verify_seal_obligation`, and the
`is_def_axiom_binding` helper + two guarded walk sites); no region collides.

## 7. Suites — CONFIRM (real counts)

Full `make test-one` on the merged binary:
- `refinement`: **24 passed, 0 failed**
- `refinement-lean`: **4 passed, 0 failed**
- `refinement-acceptance`: **14 passed, 0 failed**

Working tree stayed **clean** after all three runs (no `.reference`/test-file
promotions triggered). `branch_span.reference` already carries the promoted
`not (n > 0)` else-branch line at the committed tip — the sole #164 promotion,
and it is already committed. (One stray 57-byte JSON file named `-` was a probe
artifact of mine in the worktree root; removed — not a test output.)

---

## Overall: CONFIRM

The defeq forgery hole is dead (physical-provenance skip intact, empirically
unforgeable via either attribute spelling); fade, seal, not-c, and Part-1
totality all behave as their standalone lanes did; the merged VC-JSON is the
clean additive union with byte-identical output on non-defeq inputs; all three
suites are green with no unexpected promotions. No regression, no interaction, no
readmitted forgery. Clear to swap live and publish `7c9ccc2e4b` to PR#65.
