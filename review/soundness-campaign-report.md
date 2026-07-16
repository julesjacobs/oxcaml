# vox2 Soundness Campaign — report (first sweep)

- **Worktree / branch:** `worktrees/soundness-campaign` / `soundness-campaign`
- **Base:** `refinement-codex` tip `874c53ecea` (merge of refinement-persist into
  refinement-codex). NOTE: this line predates the v2 rename, so the mode axes
  are still named `total`/`partial` and `program`/`logic` (Ghostness), with the
  pre-v2 crossing/rules. Everything below was compiled and run against a fresh
  build of this worktree.
- **Suite:** `testsuite/tests/soundness-campaign/` — 10 test files, all green via
  `make test-one DIR=soundness-campaign` (10 passed / 0 failed). The suite is
  designed to be re-run and extended as each stage lands. (First pass: 8 files;
  second pass added extensions A and B below.)
- **Harness note:** attack-repelled = a passing expect test whose block records
  the rejection. The one confirmed OPEN FINDING is quarantined in its own file
  (`late_inference_construct_matrix.ml`) whose blocks record the CURRENT
  (unsound) accepts so the suite stays green; when the hole is fixed those
  blocks break loudly, signalling "flip to the secure expectation".

## Headline finding (reported to main 2026-07-15)

### F1 — totality late-inference (snapshot) hole: effects/divergence laundered into `total`

**Verified.** This compiles today and yields a `total` value that never
terminates:

```ocaml
let expects_total (f @ total) = f
let escaped =
  let bad () = while true do () done in
  expects_total bad
(* val escaped : unit -> 'a @ total  —  a TOTAL value that diverges *)
```

**Root cause.** The totality "hand-written residue" restrictions
(`while`/`for`/mutable construction/`try`/local exception/`lazy` literal) are
enforced by `reject_in_total_context` (`typing/typecore.ml:572`), which raises
only when `expected_mode.total_context = Some Total_mode`. That context is
installed (`typing/typecore.ml:~6500`) only when
`Totality.Guts.check_const_conservative` (`typing/mode.ml:5999`) reports the
enclosing closure's totality alloc-mode **already pinned to `Total`** (loose
ceil ≤ loose floor). While that mode is still an unresolved inference variable —
the normal case for an unannotated let-bound closure whose totality is fixed
*later* by use — `check_const_conservative` returns `None`, so no total-context
is installed and the construct is silently allowed. A subsequent use at `total`
(`expects_total bad`, or a `@ total` annotation on an outer binding) then pins
the totality variable, with **nothing having constrained it to partial**.

This is exactly the failure mode `TASK-SPEC.md`'s (Ops) ruling warns against:

> There must be NO "if the context is total right now, error" snapshot checks
> anywhere: a closure whose totality is an unsolved inference variable must be
> constrained non-total by such a body, never checked against a momentary
> boolean (the snapshot version has a late-inference soundness hole).

The residue constructs must instead **constrain** the enclosing closure's
totality mode variable to partial (a submode edge), so that a later demand for
`total` fails by ordinary submoding.

**Scope (verified matrix, `late_inference_construct_matrix.ml`).** Each
construct placed in a `let`-in closure whose totality stays unpinned, then
forced to `total`:

| Construct | Result today | Severity |
|---|---|---|
| `while` loop | **ACCEPTED** → total value diverges | termination broken |
| `for` loop | **ACCEPTED** → total value diverges | termination broken |
| mutable-record literal | **ACCEPTED** | effect in total |
| array literal | **ACCEPTED** | effect in total |
| `try`/`with` | **ACCEPTED** | effect in total |
| local `exception` | **ACCEPTED** | effect in total |
| `lazy` literal | **ACCEPTED** | effect/laziness in total |
| `assert` / `ref` / `raise` / `Array.get/set` / `Lazy.force` / `print_string` / recursion | REJECTED | (sound) |

**Why only those constructs leak.** `ref`, `raise`, `Array.*`, `Lazy.force`,
`print_string`, `assert` (via `=`), and every recursive self/mutual reference
are enforced by *capturing a partial value*; they travel the closure-lock path
(`Env.walk_locks` / `closure_mode`), which records a genuine submode constraint
that survives late inference. The leaking set is precisely the pure-syntax
constructs that have no captured value and are guarded *only* by the snapshot.

**Note on top-level vs let-in.** The top-level form
`let bad () = while ... ;; expects_total bad` is *accidentally* repelled,
because the structure-boundary legacy-zap pins `bad` to partial before the use.
The `let`-in form defeats that and is the robust exploit; the campaign suite
keeps both so a fix cannot "pass" by only handling the top-level case.

**Relationship to known work.** The v2 repair (v2-fable / v2-codex lanes)
replaces the snapshot with a submode constraint precisely to close this. This
report confirms the hole is live on the integrated `refinement-codex` line,
which branched before that repair merged — i.e. the fix must reach this line
before totality is relied upon.

## Families probed — no new findings (all sound / documented-pending)

### Family 1b — crossing abuse (arrow-erasure). Repelled.
`crossing_arrow_laundering.ml`. Hiding a partial/diverging function inside a
GADT existential (`Pack : (unit -> unit) -> packed`), a record with a function
field, or an existential over `unit -> int` does NOT let it be captured freely
by a total closure: totality-crossing correctly inherits non-crossing from the
function-typed field, so the pack value is itself `partial` and the capture is
rejected via the sound lock path. The crossing mirror is intact here.

### Family 1c — logicality (physical access to logic values). Repelled.
`logic_physical_laundering.ml`. A `logic` value cannot reach a `program`
position through tuple/record/constructor projection, through a `total`
identity function, or by dereferencing a `logic` ref — every channel rejects.
(The late-inference hole is totality-specific: `total` is the comonadic *floor*
that a demand forces the variable down to, whereas `logic` is the monadic
*ceiling* that everything already submodes into, so there is no analogous
"force to logic then discover the body was illegal" path.)

### Family 2 — refinement rigidity bypass. Repelled.
`refinement_rigidity_bypass.ml`. The rigid refined-vs-bare clash holds across
every equality channel that does not go through `Ctype.unify`'s guard: module
sealing in **both** directions (bare impl behind refined interface; refined impl
behind bare interface), private-type abbreviation, GADT propositional equality
(`(int, int{_>0}) eq` cannot be `Refl`), and functor `with`-constraint
aliasing. A bare value cannot silently acquire a refinement through the module
system.

### Family 3 — obligation-mark integrity. No finding (documented pending-VC).
`obligation_mark_integrity.ml`. Direct annotation (`(x : int{_>0})`), let-then-
annotate, and a bare argument to a refined parameter all ACCEPT today and
**retain the refinement on the result/parameter type** as the obligation mark —
the designed "obligation site" behaviour, matching the acceptance corpus's
`today=ACCEPT` markers and plan.html's "unsound by design until VC lands". This
is not a mark *drop*: `:>` coercion and refined return-annotation on a bare body
go through the clash path and reject. Whether the obligation is actually
*emitted* is only observable once the VC pass lands; this file anchors mark
retention so a future regression (accept with the refinement silently erased to
bare) would surface here.

### Family 4 — persistence / substitution under adversarial module graphs. No finding.
`persistence_module_graph.ml`. Functor-result refinements survive instantiation
(impl mismatch rejected); two alpha-equal predicates are treated as equal; two
same-named nested types with distinct predicates stay distinct (clash); and
distinct predicates across a functor tower are not merged. No distinct
predicates were collapsed and no predicate was lost. (Cross-`.cmi` round-trips
proper are covered by `testsuite/tests/refinement/persistence_*.ml` and the
persistence reviews; this file stresses the in-memory substitution / alpha-
equality / reference-head paths.)

### Family 5 — termination-specific laundering. Repelled (except via F1).
`termination_laundering.ml`. (Rec) makes self/mutual recursion partial inside
and after the group; the spec's `let rec ops = ((fun x -> (fst ops) x), 0)`
example stays partial; lazy corecursion is rejected because `Lazy.force` is a
captured partial value. The only termination escape is F1 (the `while`/`for`
snapshot hole), which is filed there.

## Second pass — extensions (2026-07-16, no new findings)

Two extensions from the next-sweep list that needed no upstream landing.

### Extension A — first-class modules & objects as totality capture vehicles. Repelled.
`fcmodule_object_capture.ml`. Completes the crossing-abuse coverage (GADT
existential / record-with-fn-field were done in the first pass). A `(module S)`
value and an object value that carry a diverging/effectful member are themselves
`partial`; capturing them in a total closure is rejected via the closure-lock
path (E1–E5 all reject, both by later use and by direct `@ total`). Totality-
crossing correctly inherits non-crossing from function-typed module fields and
object methods — same result as GADTs/records. First-class modules and objects
are sound capture vehicles.

### Extension B — cross-.cmi diamond persistence. Repelled (compiles clean).
`sc_diamond.ml` + `sc_diamond_base.mli`. Real separate compilation: the base
interface (a refined type alias, a sibling-reference value `g : int{ _ = base }`,
and a functor whose result predicate names its own parameter) is compiled to a
`.cmi`, then imported and re-exported via two independent paths. Forcing the two
round-tripped copies to meet — `pos` type equality, `[Ra.g; Rb.g]` (sibling-ref
predicate + reference-head equality across two independent imports), and
`[M1.v; M2.v]` (functor-result predicate after instantiation) — all typecheck.
No predicate was corrupted, dropped, or spuriously clashed across the diamond.
(Complements the in-memory family-4 graphs and the existing
`refinement/persistence_cmi.ml` CRC double-compile / `-i` round-trip test.)

## How to re-run / extend

```bash
export TMPDIR=/usr/local/home/jujacobs/tmp
export PATH="/home/jujacobs/.opam/5.4.0/bin:$PATH"
cd worktrees/soundness-campaign
[ -f configure ] || autoconf27 -o configure configure.ac
./configure --prefix=$PWD/_install
make test-one DIR=soundness-campaign          # first run ~6 min (builds compiler+stdlib)
make -s test-one-no-rebuild DIR=soundness-campaign   # ~seconds thereafter
```

When F1 is fixed, `late_inference_construct_matrix.ml` M1–M7 will flip from
ACCEPT to a `total`-mode rejection; update those blocks to the secure
expectation and move any that become repelled into the repelled files.

## Suggested next-sweep extensions
- Re-run against the v2-renamed line once the mode repair merges here, to
  confirm F1 is closed (submode-constraint path) rather than snapshot-patched.
- Obligation *emission* probes once the VC pass lands (family 3 currently can
  only observe mark retention, not VC generation).
- Cross-unit (`.cmi`) adversarial graphs as a multi-file test group (diamond
  imports, same-name/different-CRC collisions) to complement the in-memory
  family-4 probes.
- First-class modules and objects as capture vehicles for totality (only GADT/
  record/existential covered this sweep).

---

# Integrated-tree re-sweep (2026-07-16)

- **Worktree / branch:** `worktrees/soundness-campaign` / `soundness-resweep`
  (branched from `refinement-codex` integrated tip `34c1e95414`, "Modes
  integration: total + logical predicate checking").
- **What changed under us since the first sweep:** the repaired v2 mode axes
  (5 review rounds), the VC pass + Lean discharge + Q-003 purity gate, seals
  with directed implication, and modes integration (predicates elaborated at
  total, variables at logical). The first sweep ran on a pre-v2 / pre-VC /
  pre-seals line.
- **Suite state:** `testsuite/tests/soundness-campaign/` now 13 files, all green
  via `make test-one DIR=soundness-campaign` (13 passed / 0 failed) with the
  secure expectations committed. Independent anchor corpora re-run green on the
  same tree: `refinement-acceptance` 14/14, `refinement` 12/12,
  `refinement-lean` 2/2.
- **Method note:** batch `ocamlc -c` was used for the adversarial probes (the
  toplevel expect runner masks late-inference and bottom-evaluation); the suite
  records the toplevel form for re-runnability.

## Headline: F1 (totality late-inference) — CLOSED for the residue set

The first sweep's F1 hole (a snapshot `reject_in_total_context` that fired only
when the closure's totality was already pinned, so an unpinned-then-used-at-total
closure laundered while/for/etc into `total`) is fixed on the integrated tree.
The snapshot is replaced by a submode CONSTRAINT: the genuinely-partial
constructs call `constrain_enclosing_totality` (`typing/typecore.ml:688`, via
`Env.constrain_enclosing_totality_partial`), forcing the enclosing closure's
totality variable to partial; a later demand for `total` then fails by ordinary
submoding.

`late_inference_construct_matrix.ml` M1–M4 now REJECT via the constraint path,
verified both toplevel and batch:

```
Error: This value is "partial" but is expected to be "total".
```

| Construct | First sweep | Integrated tree |
|---|---|---|
| `while` loop | ACCEPTED (diverged at total) | **REJECTED** (constraint path) |
| `for` loop | ACCEPTED (diverged at total) | **REJECTED** |
| mutable-record literal | ACCEPTED | **REJECTED** |
| (mutable) array literal | ACCEPTED | **REJECTED** |
| pure `try 0 with _ -> 1` | ACCEPTED | ACCEPTED — **sound** (see below) |
| unused local `exception` | ACCEPTED | ACCEPTED — **sound** |
| `lazy 0` literal | ACCEPTED | ACCEPTED — **sound** |

The residue set was correctly NARROWED. The v2 constrain call sites are: while
(8127), for (8156), mutable-record (6881), mutable array literal / comprehension
(7932 / 12347), overwrite (8955), letmutable (7294), and an explicit
`@ partial` on a function literal (8189 / 8231). `try`/`with`, local `exception`,
and the `lazy` LITERAL are deliberately NOT residue: their partiality (if any)
comes from their CONTENTS, handled by the closure-lock / capture path. I
verified every effectful/divergent variant is repelled — anchored as M5b/M6b/M7b:
`while`-in-`try`, `ref`/`raise`-in-`try`, `ref`/`while`/recursion-in-`lazy`,
and `raise`-of-local-exn all REJECT. So a pure `try 0 with _ -> 1`,
`let exception E in 0`, and `lazy 0` genuinely denote terminating, effect-free
values, and their acceptance at `total` is sound. This narrowing is a soundness
improvement over the pre-v2 blanket syntactic residue.

## F2 — NEW MUST finding: lazy-pattern force at total

`lazy_force_at_total.ml` (quarantine anchor). A `@ total` function forces an
arbitrary lazy through the `lazy` PATTERN and thereby diverges or performs I/O
at total. Confirmed by execution (an effectful lazy printed inside a total call)
and by clean compilation of the divergent form.

```ocaml
let (force_it @ total) l = match l with lazy x -> x
let diverging : int Lazy.t = lazy (let rec loop () = loop () in loop ())
let boom = force_it diverging      (* total call that never terminates *)
```

Two independent defects, either sufficient; the second is the clean fix point:
1. Lazy CONSTRUCTION does not inherit body partiality into totality:
   `let l @ total = lazy (let rec loop () = loop () in loop ())` and
   `let l @ total = lazy (print_string "x"; 0)` both ACCEPT. Contrast:
   portability IS inherited from the lazy body
   (`testsuite/tests/typing-modes/lazy.ml`).
2. The `lazy` PATTERN forces but is NOT partial:
   `let (f @ total) l = match l with lazy x -> x` ACCEPTS, whereas
   `let (f @ total) l = Lazy.force l` correctly REJECTS.

Clean fix: make the `lazy` pattern-match a partial (forcing) operation mirroring
`Lazy.force`, closing the hole regardless of defect (1). Reported to `main`
2026-07-16 and disk-backed at `review/soundness-resweep-MUST-F2.md`; the repair
is tracked separately. The quarantine records the current unsound accepts
(Q1/Q2/Q3/Q5) alongside the sound `Lazy.force` reject (Q4) so it breaks loudly
when fixed.

## Re-run families — verdicts on the integrated tree

- **Crossing-arrow laundering** (`crossing_arrow_laundering.ml`): repelled,
  unchanged. GADT existential / record-fn-field / existential-pack carrying a
  diverging/effectful function stays `partial`; capture into a total closure
  rejects via the closure-lock path.
- **First-class modules & objects** (`fcmodule_object_capture.ml`): repelled,
  unchanged.
- **Cross-`.cmi` diamond** (`sc_diamond.ml` + `.mli`): repelled, compiles clean.
- **Refinement rigidity bypass** (`refinement_rigidity_bypass.ml`): repelled,
  unchanged. Bare-behind-refined seal (Q-001), private abbreviation, GADT eq,
  functor `with`-constraint all clash structurally.
- **Logicality physical access** (`logic_physical_laundering.ml`): repelled,
  REWRITTEN for the v2 rename (axis is now `physical`/`logical`, was
  `program`/`logic`). v2 gives immediates and arrows logicality CROSSING, so the
  first sweep's `int`-based L1–L4 accepts were sound crossing, not leaks. The
  file now documents that crossing (X0: logical `int` → physical accepts) and
  probes laundering with a NON-crossing carrier (a logical `int ref`): L1–L5
  (tuple / record / constructor / total-identity / deref) all REJECT with
  `This value is "logical" but is expected to be "physical"`.
- **Termination laundering** (`termination_laundering.ml`): repelled. T1/T3/T4
  reject; T2 (lazy corecursion) still rejects, now via the `Lazy.force`
  capture-path wording rather than a snapshot message.
- **Persistence module graphs** (`persistence_module_graph.ml`): no finding.
  P4 flipped from a first-sweep pending-VC accept to a `not-proved` rejection
  now that the VC pass is live — again confirming no predicate collapse.

## New surfaces

- **Obligation EMISSION** (`obligation_mark_integrity.ml`): VERIFIED. The VC
  pass is live: imposing a refined type on a bare value now EMITS an obligation
  and discharges it. O1/O3/O4/O5 fail `Refinement verification failed
  (not-proved)` (a bare int cannot be proved positive); O6 (default literal `1`,
  `1 > 0` provable) is DISCHARGED and accepts. O2 is a structural `:>` clash.
  No bare value acquires a refinement without an emitted-and-satisfied
  obligation.
- **Predicate discipline** (`predicate_discipline.ml`): repelled. An impure call
  (`read_int ()`), a ref deref (`!r`), and an effectful sub-term (`print_int`)
  inside a predicate all REJECT at totality ("The value ... is partial but is
  expected to be total"). The comparison admission works inside a predicate
  (PD4 accepts) but does NOT leak into program code (PD5: the same `(>)` in an
  ordinary closure still makes it partial).
- **Seal channel** (`seal_channel.ml`): repelled. A bare impl behind a refined
  interface — even a TAUTOLOGICAL `int{ _ = _ }` — is a structural mismatch
  rejected WITHOUT Lean (fail-closed). A refined impl that implies the refined
  interface discharges via Lean (SC3: `5 = 5 => 5 > 0` accepts); the wrong
  direction fails `Refinement verification failed at module seal ... (not-proved)`
  (SC4: `_ > 0` does not imply `_ = 5`).
- **Fact discipline** (verified via probes + the `refinement-acceptance` corpus,
  14/14 green): the Q-003 purity gate holds — an impure branch condition mints
  no fact, so `if bad () > 0 then needs_pos (bad ())` rejects `not-proved`,
  while a pure guard (`if y > 0 then needs_pos y`) accepts. Cross-occurrence
  result-contract facts (`fp_*`) are sound: a proved refined result holds for
  every evaluation (pure and impure), while a value-varying exact contract
  (`let g () : int{ _ = 1 } = read_int ()`) is rejected `not-proved`.

## Known accepted holes — still KNOWN, not findings

- **`Obj.magic` laundering** (`refinement-acceptance/imposition_channels.ml`
  `imp_magic_*`): `{ f = Obj.magic 0 }` at a refined field type still ACCEPTS —
  the documented "using Obj.magic is basically saying trust me" hole.
- **Integer overflow model** (`refinement-acceptance/integer_model.ml`
  `int_overflow_unsound`): `(max_int + 1 : int{ _ > max_int })` still ACCEPTS —
  the documented integer-model hole (today=ACCEPT / final=REJECT).

Both anchors verified present (corpus green); neither is reported as a finding.

## Net

One MUST finding on the integrated tree: **F2 (lazy-pattern force at total)**.
F1 is closed for the residue set. All other families — crossing, logicality
(re-based for the rename + crossing), rigidity, termination, persistence,
obligation emission, predicate discipline, seal channel, and fact discipline —
are repelled or sound. Known `Obj.magic` / integer-model holes remain documented
and anchored.

---

# Reconciliation onto the refinement final tip (2026-07-16)

- **Branch:** `soundness-final`, re-branched from the refinement line's final tip
  `031643ffda` ("Merge F2 lazy-totality fix (650e185c8d) into refinement-codex")
  and the soundness commit `ed33daa2d1` cherry-picked on top (clean; a pure
  1874-line addition of the suite, no conflicts). Re-branch, not rebase.
- **What moved under the suite since the re-sweep base `34c1e95414`:** the F2
  lazy-totality repair (`08d7979c46`, merged at `031643ffda`) plus three
  modes-integration commits (`9440721030`, `f0356c08bb`, `50416be104`). The F2
  repair is the only change that touches a family the suite probes.
- **Suite state:** `testsuite/tests/soundness-campaign/` — 13 files, 13 passed /
  0 failed via `make test-one DIR=soundness-campaign` on a fresh build of this
  tip. Full (rebuilding) `test-one` was used, not `-no-rebuild`, so the edited
  source is re-copied into `_runtest` (no stale-deletion masking).

## F2 anchor: flipped to REJECT, judged genuinely secure

`lazy_force_at_total.ml` was a quarantine anchor recording the unsound accepts at
`34c1e95414`. On the first run at `031643ffda` it FAILED loudly — the observed
behavior flipped from accept to reject on all four exploit blocks, which is the
signal the anchor was built to raise. I judged the new behavior against the F2
repair design (`08d7979c46`: lazy construction inherits body partiality via
`mode_lazy` crossing `~totality:false`; the `lazy` pattern calls
`constrain_enclosing_totality`) and confirmed each rejection is the designed one,
not an incidental error:

| Block | 34c1e95414 (recorded) | 031643ffda (observed) | Judged |
|---|---|---|---|
| Q1 `let q1 @ total = lazy (diverging)` | ACCEPT (`total` lazy) | REJECT — `"loop" is "partial"`, used inside the thunk | secure: construction inherits body partiality |
| Q2 `let q2 @ total = lazy (effectful)` | ACCEPT | REJECT — `"print_string" is "partial"` | secure: same mechanism |
| Q3 `let (q3 @ total) l = match l with lazy x -> x` | ACCEPT | REJECT — `function is "partial" but ... "total"` at the `lazy` pattern | secure: pattern-force constrains enclosing totality |
| Q4 `let (q4 @ total) l = Lazy.force l` | REJECT (control) | REJECT (unchanged) | sound control, unchanged |
| Q5 `force_it`/`diverging`/`boom` | ACCEPT at `force_it`, diverges at runtime | REJECT at `force_it` definition | secure: exploit closed at definition (stronger than recorded) |

The anchor was updated to record these secure rejections and its prose header
rewritten from "OPEN FINDING / quarantine" to "F2 CLOSED / secure-behavior
anchor". The expect blocks are the observed toplevel output, kept single-line to
preserve the `Line 1, characters …` positions (an ocamlformat pass had to be
bypassed so it would not multi-line the source and desync those positions).

## Per-test verdict table (all judged, not auto-passed)

| Test | Verdict at 031643ffda | Judgment |
|---|---|---|
| `lazy_force_at_total.ml` | REJECT (flipped) | F2 CLOSED — secure rejection matches repair design; anchor updated |
| `late_inference_construct_matrix.ml` | pass | F1 residue set rejects (while/for/mutable/array); pure try/exn/lazy accept — sound narrowing, unchanged from re-sweep |
| `late_inference_totality.ml` | pass | late-inference laundering repelled, unchanged |
| `crossing_arrow_laundering.ml` | pass | GADT/record/existential fn-carrier stays partial, capture rejects — unchanged |
| `fcmodule_object_capture.ml` | pass | first-class module / object fn-carrier stays partial, capture rejects — unchanged |
| `logic_physical_laundering.ml` | pass | non-crossing carrier (logical `int ref`) rejects every channel; immediate/arrow crossing is sound — unchanged |
| `refinement_rigidity_bypass.ml` | pass | bare/refined seal both directions, private abbrev, GADT eq, functor `with` all clash — unchanged |
| `termination_laundering.ml` | pass | (Rec) partiality + lazy corecursion via `Lazy.force` capture — unchanged |
| `persistence_module_graph.ml` | pass | no predicate collapse/loss under adversarial graphs; VC-live P4 rejects `not-proved` — unchanged |
| `obligation_mark_integrity.ml` | pass | VC emission live: bare→refined emits+discharges (O6 accepts, O1/O3-O5 `not-proved`) — unchanged |
| `predicate_discipline.ml` | pass | impure/deref/effect predicates reject at totality; comparison admission does not leak to program code — unchanged |
| `seal_channel.ml` | pass | tautological bare-behind-refined fail-closed w/o Lean; directed Lean discharge (SC3 accept, SC4 `not-proved`) — unchanged |
| `sc_diamond.ml` (+ `sc_diamond_base.mli`) | pass | cross-`.cmi` diamond persistence: no predicate corrupted/dropped/spuriously clashed — unchanged |

The 12 non-lazy tests were green on the first run at this tip *before any
promotion*, so their recorded expectations match observed behavior — the
modes-integration commits between `34c1e95414` and `031643ffda` did not perturb
any probed family. Every verdict above was checked against the family's design,
not accepted on the strength of a green run.

## Net

F2 is CLOSED on the refinement final tip `031643ffda`: the lazy-force / lazy-
construction totality holes reject as designed. No open MUST finding remains in
the suite on this tip. Known, documented holes (`Obj.magic` laundering, integer
overflow model) remain anchored in the `refinement-acceptance` corpus and are
not suite findings.
