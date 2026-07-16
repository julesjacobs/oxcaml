# vox2 refinement — executable acceptance corpus

Branch: `refinement-corpus` (from `213d8cb729`, the latest refinement-codex
commit). Files added: `testsuite/tests/refinement-acceptance/*.ml` (8 expect
tests, 28 documented cases) + this report. No other files touched.

## What this is

The end-to-end acceptance suite the finished refinement system must pass,
written ahead of the implementation as an executable spec. It is organized by
the plan's checking model (binder-as-fact, annotation/contract obligations,
rigid unification, seals, recursion/IH, scope & mutation), and every case
documents its **final** expected behavior and its **current** behavior, both
encoded in an ocamltest `expect` block so the suite is green today and forces a
promotion (against the documented target) as each roadmap stage lands.

The suite is green now: `make test-one DIR=refinement-acceptance` → 8/8 pass.

## Base reality (what actually runs today at `213d8cb729`)

The base has W1–W4 committed (surface syntax, `Rexp_*` predicate AST, `Trefine`
in the type graph with **rigid** equality, and predicate elaboration). It does
**not** have the typechecker-integration rules (those are still uncommitted
working-tree changes in `worktrees/refinement-codex`), the verification pass,
seals, or persistence. Consequently the feature **fails closed**: every attempt
to put a refinement on a bare value is a rigid type clash. I verified this
empirically — 27 of 28 cases reject today; the single acceptor is the
same-refinement annotation.

### The load-bearing, non-obvious finding

The plan says refined-vs-bare is "a clash, always, in every relation and at
every depth." At the **source** level that is only the whole story for **nested**
refinements. Once the integration rules land, a **top-level** refinement is
stripped on every *use* (`use.exp_type` is the skeleton — confirmed in the codex
lane's own `typechecker_rules.ml`) and *demanded* only at obligation sites
(annotation / contract / seal). So at the source level:

- **Nested** refined-vs-bare / unequal-predicate mismatches (`int{p} list` vs
  `int list`, `int{_=1} list` vs `int{_=2} list`, tuples) are **permanent**
  rigid clashes — they reject today and must keep rejecting *unchanged* forever.
  These are the corpus's stable anchors (`stable=yes`), and if a future stage
  ever changes their output that is a **red flag**: the rigid invariant eroded.
- **Top-level** refined-vs-bare at a *use* site (arithmetic, passing to a bare
  parameter, a neutral `if`-branch, annotating down to the skeleton) rejects
  today but must **flip to ACCEPT** (covariant weakening) when integration
  lands — see `skeleton_weakening.ml`. These are the tripwires proving the
  strip/weaken behavior shipped.
- **Top-level** refined *demand* on a bare value (annotation, contract arg,
  seal) rejects today and stays a reject-or-accept decided by the **VC**: a
  provable predicate accepts, an unprovable one fails with a *verification*
  error (today it is an indistinguishable rigid clash; the message tightens).

This distinction is why the "rigid rejection" cases are split across
`rigid_unification.ml` (permanent, nested) and `skeleton_weakening.ml`
(today-rejects-then-accepts, top-level). Collapsing them would have mislabeled
half the rigid cases as permanent when the finished system accepts them.

## Harness / marker mechanism

Each case carries one greppable marker line (`grep -rh '@acc id='`):

```
@acc id=CASE final=ACCEPT|REJECT today=ACCEPT|REJECT stable=yes|no unlocks=STEP
```

- `final` — what the finished system must do (the acceptance target).
- `today` — behavior observed at the base, encoded in the `[%%expect]` block.
- `stable=yes` — today already equals final and the expect output is an anchor
  that must not drift. `stable=no` — today is the fail-closed placeholder; the
  expect block **must change to match `final`** when `unlocks` lands, and that
  promotion is the mechanical tightening.
- `unlocks` — the roadmap step that brings today into line with final.

The green-today / tightens-later property is exactly ocamltest expect
semantics: the `[%%expect]` block records current output (green); when a stage
changes behavior the block diverges, forcing a `promote-one` whose new output
the header + marker say must equal `final`. A reviewer at each stage promotes
and checks the diff against the documented target.

## Case table

Legend: F=final, T=today, S=stable. Unlocks: **int**=typechecker integration
rules; **ver**=verification pass (VC + Lean); **seal**=seals; **—**=already
final.

### binder_facts.ml — binder-as-fact
| id | F | T | S | unlocks | what it pins |
|----|---|---|---|---------|--------------|
| bf_use_fact | ACC | REJ | no | int+ver | let-binder fact `x=1` discharges downstream `x=1` |
| bf_skeleton_use | ACC | REJ | no | int+ver | refined binder used at skeleton (`x+1`) |
| bf_needs_fact | ACC | REJ | no | int+ver | correctness **needs** the fact: `x=7` ⊢ `x>0` |
| bf_param_fact | ACC | REJ | no | int+ver | parameter contract `n=5` ⊢ result `n>0` |

### annotation_obligations.ml
| id | F | T | S | unlocks | what it pins |
|----|---|---|---|---------|--------------|
| ao_provable | ACC | REJ | no | int+ver | `(5 : int{_>=0})` → VC `5>=0` discharged |
| ao_unprovable | REJ | REJ | no | int+ver | `(-5 : int{_>=0})` → clean VC failure (msg tightens) |
| ao_same_refinement | ACC | **ACC** | **yes** | — | identical refinement: no obligation, cheap accept |

### contract_obligations.ml
| id | F | T | S | unlocks | what it pins |
|----|---|---|---|---------|--------------|
| co_provable | ACC | REJ | no | int+ver | `c_eq1 1` → VC `1=1` discharged |
| co_unprovable | REJ | REJ | no | int+ver | `c_eq1 2` → clean contract VC failure |
| co_dependent | ACC | REJ | no | int+ver | dependent param `a:int{_=n}`; `dep 3 3` (defn elaborates today) |

### rigid_unification.ml — permanent clashes (green forever)
| id | F | T | S | unlocks | what it pins |
|----|---|---|---|---------|--------------|
| ru_nested_refined_to_bare | REJ | REJ | **yes** | — | `int{_=1} list` ✗ `int list` |
| ru_nested_bare_to_refined | REJ | REJ | **yes** | — | `int list` ✗ `int{_=1} list` |
| ru_nested_unequal_preds | REJ | REJ | **yes** | — | `int{_=1} list` ✗ `int{_=2} list` |
| ru_syntactically_distinct_preds | REJ | REJ | **yes** | — | `int{_>0}` ✗ `int{0<_}` (plan's example) |
| ru_tuple_nested | REJ | REJ | **yes** | — | `int{_=1} * int` ✗ `int * int` |

### skeleton_weakening.ml — top-level use weakens (rejects today → accepts)
| id | F | T | S | unlocks | what it pins |
|----|---|---|---|---------|--------------|
| sw_annot_to_skeleton | ACC | REJ | no | int | `(x : int)` on refined `x` |
| sw_use_in_arith | ACC | REJ | no | int | `x + 1` on refined `x` |
| sw_pass_to_bare_param | ACC | REJ | no | int | refined arg to bare parameter |
| sw_neutral_if_branches | ACC | REJ | no | int | refined/bare `if`-branches both weaken |

### seals.ml
| id | F | T | S | unlocks | what it pins |
|----|---|---|---|---------|--------------|
| seal_conforming | ACC | REJ | no | seal | refined `.mli` over conforming impl → implication discharged |
| seal_nonconforming | REJ | REJ | no | seal | refined `.mli` over bad impl → seal VC fails |
| seal_covariant_drop | ACC | REJ | no | int+seal | refined impl behind bare interface (fails in impl body today) |

### recursion_fib.ml
| id | F | T | S | unlocks | what it pins |
|----|---|---|---|---------|--------------|
| rec_fib_nonneg | ACC | REJ | no | int+ver | fib with `int{_>=0}` result; rec call result = IH |
| rec_sum_to | ACC | REJ | no | int+ver | triangular sum; rec-call contract + IH |

### scope_mutation.ml
| id | F | T | S | unlocks | what it pins |
|----|---|---|---|---------|--------------|
| scope_fact_in_scope | ACC | REJ | no | int+ver | fact used where its binder is in scope |
| scope_fact_dropped | REJ | REJ | no | int+ver | fact dropped out of scope → obligation unprovable |
| mut_binder_exempt | ACC | REJ | no | int+ver | mutable refined binder declared+read |
| mut_no_persistent_fact | REJ | REJ | no | int+ver | mutable fact havocked on write → re-annotate fails |

## Totals

28 cases. Final: 18 ACCEPT / 10 REJECT. Today: 1 ACCEPT / 27 REJECT (fail
closed). Stable anchors (green forever): 6 (5 nested rigid clashes +
same-refinement). Tightening-later: 22.

## How the suite tightens as stages land

- **Typechecker integration** (`int`): all 4 `skeleton_weakening.ml` cases flip
  REJ→ACC (weakening/skeleton use ships). Binder/annotation/contract/recursion
  cases stop failing at their *introduction* annotation and begin failing (if at
  all) only at their real obligation site — intermediate promotions expected;
  the anchors in `rigid_unification.ml` and `ao_same_refinement` must NOT move.
- **Verification pass** (`ver`): provable cases (bf_*, ao_provable, co_provable,
  co_dependent, rec_*, scope_fact_in_scope, mut_binder_exempt) reach ACCEPT;
  unprovable cases (ao_unprovable, co_unprovable, scope_fact_dropped,
  mut_no_persistent_fact) change from a rigid-clash message to a clean VC-failure
  message (still REJ).
- **Seals** (`seal`): seal_conforming → ACC, seal_nonconforming → clean seal VC
  failure, seal_covariant_drop → ACC.
- When the modes stub is swapped for total+logical checking (last roadmap step),
  predicates using comparisons (`>=`, `>`, `<`) additionally depend on the
  comparisons unlock (kind-constrained decls / spec prelude); until then the
  stub lets them typecheck as ordinary bool expressions. Cases that need only
  equality-style facts are unaffected.

## Caveats / notes for maintainers

- Format is toplevel `expect` (matching the committed `refinement/*.ml` tests).
  The concern that the toplevel masks batch behavior applied to the *modes*
  work (totality inference); refinement modes are stubbed, so it does not bite
  here, and the verification/seal passes are compile-time and run identically in
  batch.
- The dependent-contract case prints an environment-dependent value stamp
  (`global[n/NNN]`), the same pattern the committed `elaboration.ml` relies on;
  stable within a build. If a rebuild renumbers it, `promote-one` re-syncs.
- Cases were written against, and their `today` status verified by, a real
  build of this worktree (boot + final compiler; `promote-one` to capture
  current output, `test-one` to confirm green).
