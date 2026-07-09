# Total correctness for vox: termination, totality, liveness

*Design-options study, 2026-07-06. First item on the vox future-work list
(`docs/vox/index.html#future`: "Total correctness, termination,
liveness").*

vox is a **partial-correctness** verifier today: a contract constrains
what a function returns *if it returns*. A path that diverges satisfies
any postcondition vacuously. The canonical exhibit is the `unit{ 1 = 2 }`
self-call in the lemma design doc
(`docs/plans/2026-07-05-vox-lemma-design.md`): as an *ordinary* recursive
function it type-checks, because the recursive call's contract is assumed
as an induction hypothesis and the never-reached postcondition is free.
The same loophole is documented, approvingly, inside the shipped Fibonacci
demo (`testsuite/tests/vox/demo/lean_fib.ml:94`): "For n < i the loop
diverges; partial correctness is unbothered."

This document maps what termination checking *already* exists (it is more
than zero, and unevenly distributed), surveys the literature, and lays out
options for closing the gap — turning "if it returns, the answer is
correct" into "it returns, and the answer is correct" where the author
asks for it.

---

## 1. The four-way gap map (grounded by probing the tree)

vox has four recursion/iteration surfaces. Termination is checked on two
of them and ignored on the other two — and the two where it *is* checked
are exactly the two that run inside Lean, not the two that describe the
user's program.

| Construct | Termination checked today? | Mechanism | Evidence |
|---|---|---|---|
| **(a) `total_` spec function** | **Yes** — Lean's checker | reflected to a Lean `@[grind] def`; structural recursion accepted by Lean's equation compiler for free, int-indexed recursion emits `termination_by (e).toNat` / `decreasing_by omega\|grind` from `[@@vox.decreases e]` | `vox_verify.ml:4213` (`lean_spec_def`), `vox_verify.ml:4224`; demo `lean_fib.ml:25`, `total_attr.ml:11` |
| **(b) `[@@vox.lemma]` (v2)** | **Yes** — Lean's checker | the lemma is a proof-carrying recursive Lean *definition* mirroring the OCaml body; structural recursion is inferred, int-indexed emits `termination_by` from `[@@vox.decreases]`; a false or non-terminating "lemma" has no Lean proof and fails **closed** | `vox_verify.ml:4622` (`termination ()`), `:4660`; lemma doc §"Soundness is the solver's" |
| **(c) ordinary recursive function** (the program vox verifies) | **No** | the recursive call re-instantiates the callee's dependent contract = the partial-correctness IH; **`[@@vox.decreases]` is never read here** (only the reflect and lemma paths scan for it); a `unit{ false }` self-call verifies vacuously | `vox_reflect.ml:963` and `vox_verify.ml:4660` are the *only* readers of `vox.decreases`; `lean_fib.ml:94`; isqrt's inner `go` at `lean_isqrt.ml:36` |
| **(d) `while` / `for` loop** | **No** | classical Hoare quadruple with `[@vox.invariant]`: asserted on entry, assumed at the head, re-asserted on the back-edge, head-∧-¬guard after. **There is no variant / decreasing clause at all** | `vox_verify.ml:3096` (`Texp_while`), `:3150` (`Texp_for`); `lean_isqrt.ml` (the `go` loop is partial) |

**The shape of the gap.** Termination is *already* enforced wherever a
recursive object is handed to Lean as a `def` (a, b): Lean's equation
compiler will not accept a definition it cannot prove terminating, so
vox inherits that check for free. Termination is *absent* wherever vox
reasons about the user's program itself with its own VC machinery (c, d):
there vox assumes the contract at the back-edge and never discharges a
decreasing measure. So the engine that proves termination is present and
trusted; it is simply not wired to the two surfaces where a user writes
a loop.

Two consequences worth stating precisely, because the options below turn
on them:

- The `[@@vox.decreases]` attribute **already means "Lean, prove this
  metric decreases"** — but only for reflected `total_`/lemma
  definitions. Reusing that exact attribute for ordinary functions is the
  smallest possible surface change; the semantics are already specified
  and tested (`lean_lemma_baddecr_fail.ml` rejects a bogus metric).
- A `total_` function is *by construction already total* (Lean checked
  it). So "total spec function" and "total program function" are two
  points on one axis, and the naming collision between the existing
  `total_` marker and any new totality mode has to be resolved
  deliberately (§7).

---

## 2. What "liveness" can honestly mean here

Liveness ("something good eventually happens") is, in the classical
Alpern–Schneider sense, only interesting relative to an execution model
with *ongoing* behaviour — reactive systems, concurrency, fairness. vox
today verifies sequential, terminating-or-not OCaml functions. In that
setting the only liveness property expressible is **termination itself**
(and its refinement, *bounded* termination: "returns within `f(n)`
steps"), which is exactly a loop/recursion variant.

Genuine response/progress properties ("every enqueue is eventually
dequeued", "no thread starves") require a temporal logic over a trace
semantics and a fairness assumption, neither of which vox has. Those
belong to the **concurrency** design (sibling doc, task #132) and should
not be promised here. This document therefore scopes "liveness" to
*termination and resource bounds*, and flags temporal liveness as
out-of-scope-until-concurrency. Saying so plainly is the honest move; a
variant clause is not a substitute for a temporal-logic model checker.

---

## 3. Literature (from training knowledge; flagged where uncertain)

**Structural + well-founded recursion (Lean 4, Coq, Agda).** A recursive
definition is accepted if some argument decreases in a well-founded order
on every recursive call. Lean's equation compiler tries *structural*
recursion first (a syntactic subterm of a matched argument), then
*well-founded* recursion via a user `termination_by` measure discharged by
`decreasing_by`. Coq's `Function`/`Program Fixpoint` and Agda's
sized-types / termination checker are the same idea with different
ergonomics. vox already rides this for `total_`/lemmas — the machinery is
present, the question is how to expose it to programs.

**Dafny `decreases` clauses (the ergonomic gold standard).** Dafny attaches
a `decreases` tuple to every method/loop; each recursive call or back-edge
generates a VC that the tuple strictly decreases in the lexicographic
order (with a well-founded per-component order: `<` on nats, structural on
datatypes, `⊂` on sets). Crucially Dafny *guesses a default measure* — the
tuple of decreasing-looking parameters, or `decreases *` to opt out — so
the overwhelming majority of functions need **no annotation**. This
default-measure inference is what makes termination "on by default and
invisible" in Dafny, and is the single most important ergonomic lesson for
vox.

**F\* effects (`Tot` vs `Dv`).** F\* puts totality *in the type*: a
function is `Tot t` (total, terminating) or `Dv t` (may diverge), and the
effect composes — a `Tot` function may not call a `Dv` one without
incurring `Dv`. Termination of a `Tot` function is proved by a `decreases`
metric on a well-founded order, defaulting to the lexicographic order on
arguments. The lesson: **totality as a property of the arrow** gives clean
composition (a total caller cannot silently depend on a divergent callee)
and a natural default (`Tot`), at the cost of an effect discipline.

**Liquid Haskell termination metrics.** LH proves termination by default,
using the first `Int`-typed (or size-measurable) argument as the metric;
the programmer overrides with `/ [e1, e2]` (a lexicographic termination
expression) in the refinement signature, or `lazy f` / `Terminating` to
opt out. The metric is checked by generating a refinement VC that it
decreases — discharged by the same SMT backend that does everything else.
This is the closest existing system to vox's architecture (refinement
types + SMT), and its "termination is just another refinement VC" stance
is directly transplantable.

**Size-change termination (Lee–Jones–Ben-Amram, 2001).** Instead of a
user measure, SCT builds a graph of size-change relations between call
arguments and checks that every infinite call sequence would decrease some
value infinitely (impossible over a well-founded order). It is *fully
automatic* and handles many mutual/permuting recursions that a single
lexicographic guess misses, but gives worse error messages ("no
size-change witness") and is a whole analysis to build and trust
vox-side. AProVE and Isabelle's `fun` use SCT-family methods.

**ACL2 measures.** Every ACL2 `defun` must be admitted with an *ordinal*
measure (down to ε₀) proved to decrease; the prover attempts a default and
asks for `:measure` otherwise. Battle-tested that a measure discipline
scales to enormous developments, and that ordinals (not just nats) are
occasionally necessary (Ackermann-like nests).

**Terminator / transition invariants (Cook–Podelski–Rybalchenko).** For
imperative loops, termination = the transition relation is contained in a
finite union of well-founded relations; found by counterexample-guided
refinement over ranking functions. Overkill for vox's first cut, but the
right reference if loop-variant *inference* (sibling task #128) grows
ambitious.

**Bounded / cost refinements (RaML, TiML, Liquid Haskell's `[t]`).** If a
metric is exposed as a spec function, "terminates in ≤ g(n) steps" is
provable as an ordinary refinement about a cost accumulator. This is the
bridge from termination to *resource* liveness, and vox's cost model
(sibling: vox8 sweep, memory `vox-via-adversarial-sweep`) already measures
per-VC cost, so it is a natural later increment.

---

## 4. Design axes

Every option below is a choice along three orthogonal axes.

**Axis S — surface (how the user asks for totality).**
- **S1 per-definition attribute**: `let rec f ... [@@vox.total]` /
  `[@@vox.decreases e]`. Opt-in, local, mirrors today's `total_`.
- **S2 mode on the arrow**: a `-total->` arrow (or a `total_`-annotated
  result), F\*-style, so totality composes through the type and callers
  see it. Opt-in but *contagious upward* (a total function may only call
  total functions).
- **S3 whole-module default with opt-out**: every function in a
  `[@@@vox.total]` module must terminate unless marked `[@vox.diverges]`
  (F\*'s `Dv`, Dafny's `decreases *`, LH's `lazy`). Dafny/LH-style
  "on by default".

**Axis C — checking (who proves the measure decreases).**
- **C1 reuse the lemma-v2 translator, generalized**: emit the function's
  body as a Lean recursive `def` (structural) or with a synthesized
  `termination_by`; Lean's equation compiler *is* the termination
  checker. Zero new trusted code — it is the machinery of (a)/(b).
- **C2 vox-side measure VC (Dafny/LH style)**: at each recursive
  call/back-edge, emit an ordinary vox VC `measure[args'] < measure[args]
  && measure[args] >= 0` and discharge it with grind, exactly like any
  refinement obligation. No Lean `def` translation needed; works for
  arbitrary bodies (not just the reflectable fragment).
- **C3 vox-side size-change**: infer termination with no measure at all.

**Axis I — inference (does the common case need an annotation).**
- **I0 always annotate.**
- **I1 default lexicographic measure** on the int/datatype-sized
  arguments (Dafny/LH default), with the annotation as override. This is
  the adoption lever.

---

## 5. Options

### Option A — "`total_` grows up": generalize the lemma-v2 translator to programs (S1 + C1)

**Idea.** The lemma-v2 work already translates an ordinary recursive
OCaml function into a *proof-carrying Lean definition* whose termination
Lean checks (`docs/plans/2026-07-05-vox-lemma-design.md` §v2). Reuse that
translator for *any* function marked `[@@vox.total]`: emit the body as a
Lean `def` (structural recursion accepted for free; `[@@vox.decreases e]`
→ `termination_by (e).toNat`), and if it elaborates, the function is
total. Literature: Lean/Coq structural + well-founded recursion; this is
`total_` and `fun_induction` applied to the program rather than the spec.

- **(b) surface.** S1: `let rec f ... [@@vox.total]`, with optional
  `[@@vox.decreases e]` when recursion is not structural — *identical* to
  the spelling `total_` spec functions and lemmas already use. No arrow
  mode, no module default.
- **(c) checking.** C1: no new trusted checker; Lean's equation compiler
  is the oracle, exactly as for `total_`. The translator already exists
  and is tested.
- **(d) semantics / what it licenses.** A `[@@vox.total]` function's
  postcondition may be used *without* a reachability caveat. The
  `unit{ false }` self-call is rejected: it has no terminating Lean
  definition. Only functions *marked* total change behaviour; every
  existing test is untouched. This is the key adoption property —
  strictly additive.
- **(e) mutual / higher-order / loops.** Mutual recursion needs Lean
  `mutual ... end` blocks; the translator emits independent `def`s today
  (`register_spec_def` queues one `spec_def` per binding, `vox_verify.ml
  :847`), so mutual totality is a translator extension, not free.
  Higher-order is **out**: reflected functions are first-order by
  construction (`vox_reflect.ml:280`), and a fixpoint through a closure
  has no Lean `def` image. **while/for loops are the fatal limitation**:
  the lemma-v2 translator only covers the reflectable expression fragment
  (matches, ifs, lets, first-order calls) — it cannot translate a mutable
  loop body into a Lean `def` at all. So Option A leaves gap (d) wide
  open.
- **(f) benchmark.** Passes: the lemma suite (already total by
  construction), Fibonacci `total_`. Ackermann: works *only* if written
  in the reflectable fragment with a lexicographic `decreases` — but the
  translator's `termination_by` today emits a single `(e).toNat`, not a
  tuple, so lexicographic Ackermann needs a tuple extension. **Fails**:
  isqrt's `go` (it is a fine first-order function, so this one is in
  reach), and any imperative loop.
- **(g) incremental / naming.** Smallest delta: it is the existing
  translator with a wider entry point. Naming: `[@@vox.total]` on a
  program function vs `total_` on a spec function — see §7.

**Verdict.** Cheapest and soundest for the *functional* fragment, and it
reuses proven machinery. But it structurally cannot reach loops, which is
where users most want termination.

### Option B — Dafny-style measure VCs discharged by grind (S1 + C2 + I1)

**Idea.** Do what Dafny and Liquid Haskell do: at every recursive call
and every loop back-edge, emit an *ordinary vox VC* stating the measure
strictly decreases and is bounded below, and discharge it with the SMT/
grind backend that already proves every other obligation. No Lean `def`
translation; the measure is a refinement term. Default the measure
(Dafny/LH `I1`) to the lexicographic tuple of the function's int- and
datatype-sized arguments, with `[@@vox.decreases e]` (now also legal on
ordinary functions and loops) as the override.

- **(b) surface.** S1 with I1 default. `[@@vox.total]` (or a module
  default, §7) turns the obligation on; `[@@vox.decreases e]` supplies the
  measure when the default guess is wrong; loops take
  `[@vox.decreases e]` next to their existing `[@vox.invariant]`.
- **(c) checking.** C2: reuses `emit_vc` / grind. For recursion, at each
  self/mutual call emit `0 <= m[params] && m[args] < m[params]` under the
  path facts already in scope (the same context the contract VC uses). For
  loops, emit at the back-edge `0 <= v && v_next < v` alongside the
  invariant re-assertion (`vox_verify.ml:3138` is exactly where the
  back-edge VC is emitted today — the variant VC slots in beside it). The
  measure term rides the existing predicate language (`Refinement.pred`),
  so int and lexicographic-tuple measures are already expressible;
  datatype-size measures need a reflected `size` spec function (a `total_`
  the compiler can synthesize per datatype).
- **(d) semantics.** Same license as A (postcondition without reachability
  caveat; `unit{ false }` loop rejected because no measure decreases into
  the never-terminating self-call). Additionally it closes gap **(d)**:
  `while`/`for` loops become totalizable. Only marked/opted-in code
  changes; the fib demo's `fib_loop` would need a `decreases n - i` to be
  called *total*, and its comment at `:94` would be rewritten.
- **(e) mutual / higher-order / loops.** Mutual recursion: one shared
  measure across the SCC, VC at each cross-call — natural in C2 (no Lean
  `mutual` block needed). Higher-order: honestly partial — a call through
  a closure/argument function cannot get a syntactic measure VC unless the
  functional carries a `total_` contract with its own measure (a
  "decreasing function" refinement, LH-style); document as future work.
  Loops: variant + invariant pairing, and this is precisely the
  **inference sibling's** territory (task #128) — the *default* measure
  and any *inferred* loop variant come from that loop; interface note in
  §8.
- **(f) benchmark.** Passes: the lemma suite (trivially), Fibonacci,
  isqrt's `go` (`decreases hi - lo`), Ackermann (lexicographic
  `decreases (m, n)` — expressible as a `Refinement.pred` tuple, grind
  proves the lexicographic step from omega), the McCarthy 91 function
  (`decreases 101 - n` on nat, or `decreases (0 - n) + 101` — needs the
  nonlinear reasoning grind already does for isqrt). The `unit{ 1 = 2 }`
  self-call is **rejected** because no measure decreases.
- **(g) incremental / naming.** Larger than A but self-contained: new VC
  emission points, a default-measure synthesizer, a per-datatype `size`.
  Composes with A (a function may be checked by *either* translator-C1 or
  measure-C2; pick C1 when the body is reflectable and structural, C2
  otherwise). Naming: §7.

**Verdict.** The most complete single option: it is the only one that
reaches loops, mutual recursion, and lexicographic measures with one
mechanism, and it reuses the SMT backend rather than adding a checker. Its
risk is grind's power on the decrease VCs (mitigated: they are small
arithmetic/structural goals, cheaper than the functional VCs vox already
discharges).

### Option C — totality as a mode on the arrow (S2 + C1/C2)

**Idea.** F\*'s `Tot`/`Dv`. Introduce a total arrow (surface `-total->`,
or a `total_` result annotation lifted to the arrow) so that totality is a
property of the *type* and composes: a total function may call only total
functions, and a caller reading a total function's contract needs no
reachability side-condition because the type already guarantees it.
Checking is A or B underneath; the novelty is the *typing discipline*.

- **(b) surface.** S2. Contagious upward, which is the point: `val f :
  (n : int) -total-> int{ ... }` promises termination to every client
  through the `.cmi`, and the OxCaml mode system is the natural carrier
  (a totality axis alongside the existing mode axes — coordinate with the
  mode-system inventory, sibling #119/#123).
- **(c) checking.** C1 or C2 at each total definition; the arrow only
  governs *composition and export*, not the per-function proof.
- **(d) semantics.** Strongest license: totality crosses module
  boundaries as a type, so a client of `val quicksort : ... -total-> ...`
  may use the result unconditionally without seeing the body. The
  `unit{false}` loophole closes structurally: a diverging function cannot
  be given a total arrow. Behaviour change is larger — every arrow's
  default totality must be decided, and `-total->` in an `.mli` becomes an
  obligation the implementation's seal must discharge (mirrors the
  interface-`axiom` seal already in `vox_verify.ml`).
- **(e) mutual / higher-order / loops.** Higher-order is where the arrow
  *shines*: a total higher-order function can demand a total function
  argument (`(int -total-> int) -total-> ...`), which is exactly the
  "decreasing function" refinement B needed and could not express
  ergonomically. Mutual/loops inherit from the underlying C1/C2.
- **(f) benchmark.** Same proof obligations as A/B; additionally a
  *composition* benchmark: a total `map` over a total function argument;
  a total function that calls a partial one must be **rejected at the
  type level** (not the solver).
- **(g) incremental / naming.** Heaviest: needs a new arrow/mode, effect
  propagation in the type-checker, and `.cmi` transport. Best done *after*
  A or B proves out the per-function checking, as the composition layer on
  top. Naming: a `total_` value and a `-total->` arrow should share the
  word deliberately (§7).

**Verdict.** The right *long-term* surface (composition and higher-order
totality are real needs), but too large as a first cut and dependent on
the mode-system work. Sequence it after B.

### Option D — whole-module totality by default (S3 + C2 + I1)

**Idea.** Dafny's actual default: in a `[@@@vox.total]` module, *every*
recursive function and loop incurs a termination VC unless marked
`[@vox.diverges]` / `[@vox.decreases *]`. Inference (I1) supplies the
measure so most functions need no annotation, so the module is
"total-by-default, divergence-opt-out."

- **(b) surface.** S3. One module-level pragma flips the polarity; the
  escape hatch is per-function. This is the *adoption* option: it makes
  totality the norm rather than a decoration, matching how Dafny/F\*
  developments actually look.
- **(c) checking.** C2 (measure VCs) with I1 default measure — the same
  engine as B, applied to every function in scope.
- **(d) semantics.** Every function in a total module gains the
  unconditional-postcondition license. Behaviour change is the *largest*
  of any option for existing code: turning the pragma on for the current
  test suite would demand a `decreases`/`diverges` on every recursive
  demo whose measure the default cannot guess (isqrt's `go`, the fib
  loops, any `while true` server loop). That migration cost is the price
  of the strong default.
- **(e) mutual / higher-order / loops.** Inherits B. Higher-order still
  needs the arrow-level "decreasing argument" (C) to be fully honest;
  under D a higher-order function that calls its argument is either
  restricted or marked `diverges`.
- **(f) benchmark.** The whole lemma suite is total-by-construction and
  passes with *no* annotations (the win); the imperative demos each need
  one `decreases`; a `while true` event loop is marked `[@vox.diverges]`
  and still verifies partially.
- **(g) incremental / naming.** Should be the *last* layer: it is B plus a
  polarity flip plus a migration. Turning it on firmwide without I1's
  default measure would be miserable; with a good default it is the
  end-state. Naming: §7.

**Verdict.** The desirable end-state for *adoption*, but only after B
(the VC engine) and I1 (the default measure) exist and have been shaken
out on opt-in code.

### Option E — automatic size-change termination (S1/S3 + C3)

**Idea.** Lee–Jones–Ben-Amram: infer termination with *no* measure by
building the size-change graph of argument relations across calls and
checking the SCT criterion. Fully automatic; handles permuting and mutual
recursions a single lexicographic guess misses.

- **(b) surface.** Any (it needs no annotation); pairs naturally with S3.
- **(c) checking.** C3: a new vox-side analysis and its own trusted
  implementation — the opposite of the "reuse Lean/grind" thesis of A/B.
- **(d) semantics.** Same license; the difference is purely *how* the
  measure is discharged.
- **(e/f).** Strong on mutual/permuting recursion; **weak on
  value-dependent** termination (isqrt's `hi - lo`, McCarthy 91, anything
  where the decrease is arithmetic rather than structural — SCT sees only
  size relations, not `hi < lo` arithmetic). So it is *complementary* to,
  not a replacement for, B's measure VCs.
- **(g).** A whole analysis to build and trust; best considered as an
  *inference upgrade* to B's default-measure step (task #128), not a
  standalone option.

**Verdict.** Keep in the quiver as a future inference booster for mutual
recursion; do not build first — it cannot do the arithmetic termination
that the motivating loops (isqrt) need, and it adds trusted code where
A/B add none.

---

## 6. Options at a glance

| | Surface | Checker | Reaches loops? | Mutual | Higher-order | New TCB | First-cut cost |
|---|---|---|---|---|---|---|---|
| **A** generalize lemma-v2 | attr (S1) | Lean `def` (C1) | **no** | needs `mutual` block | no | none | **low** |
| **B** measure VCs | attr + default (S1+I1) | grind VC (C2) | **yes** | yes | partial (needs C) | none | med |
| **C** total arrow | mode (S2) | A/B underneath | via A/B | via A/B | **yes** | type-checker mode | high |
| **D** module default | pragma (S3) | grind VC (C2) | **yes** | yes | partial | none | med + migration |
| **E** size-change | any | vox-side SCT (C3) | structural only | **yes** | no | **whole analysis** | high |

---

## 7. The `total_` naming decision

vox already has `total_` (spec functions) and would gain a totality notion
for *program* functions. Three names are in play: `total_ len` (a spec
function reflected to Lean and *by construction total*), `[@@vox.total]`
(the `.cmi`-transport attribute spelling of the same), and any new
program-totality marker/arrow.

Recommendation: **unify, do not multiply.** A `total_` spec function is
already the degenerate case of a total program function (it is total, it
just also happens to be reflected into the logic). So:

- Keep `[@@vox.total]` as *the* per-definition totality request, for both
  spec and program functions. For a spec function it additionally means
  "reflect me"; for a program function it means "prove me terminating."
  The two readings coincide on the totality obligation and differ only in
  whether the body is *also* reflected — which the existing `total_`
  already decides by reflectability.
- Reserve a distinct spelling — `-total->` or a mode keyword — for the
  *arrow-level* (Option C) notion, and document that
  `let rec f ... [@@vox.total]` gives `f` a total arrow, exactly as
  `total_` today gives a reflected function its `.cmi` marker. One word,
  two levels (value and arrow), consistently — the same discipline the
  reflect-primitive doc uses for `total_` vs `[@@vox.reflect]`.

This avoids a `total_` (spec) / `[@@vox.total]` (program) split that would
confuse users who have learned exactly one `total_` today.

---

## 8. Cross-references to sibling designs

- **Invariant inference (task #128):** loop *variants* are the termination
  dual of loop *invariants*. Option B's back-edge variant VC (`vox_verify
  .ml:3138`) and its default-measure guess (I1) are the same inference
  problem as invariant synthesis, and should share the loop-analysis
  sibling's machinery. The `[@vox.decreases]` loop annotation is the
  manual fallback when inference fails, mirroring `[@vox.invariant]`.
- **Exceptions (task #124):** a function that always `raise`s "terminates"
  by exiting abnormally. Whether a total function may raise (F\*: `Tot`
  excludes divergence but a separate `exn` effect covers exceptions) is a
  joint decision with the exceptions design. Interface note: totality here
  means "no infinite reduction"; exceptional exit is the exn doc's to
  define, and a total-and-pure arrow is the conjunction.
- **Concurrency (task #132):** temporal liveness (response/progress/
  fairness) is out of scope until a trace semantics exists there (§2).
  Termination variants are the only liveness this doc delivers.
- **Mode system (tasks #119/#123):** Option C's total arrow wants to be a
  mode axis; coordinate the arrow spelling and `.cmi` transport with the
  mode inventory before committing surface syntax.

---

## 9. Recommendation and first milestone

**Recommend Option B (Dafny/LH-style measure VCs discharged by grind, with
a default lexicographic measure), opt-in via `[@@vox.total]` /
`[@@vox.decreases]`, sequenced ahead of the arrow (C) and the module
default (D).**

Rationale: B is the only single mechanism that reaches all four gap-map
surfaces — functional recursion, imperative loops, mutual recursion, and
lexicographic/arithmetic measures — and it adds **no trusted code**: the
decrease conditions are ordinary refinement VCs discharged by the same
grind backend that already proves isqrt's nonlinear bracket. Option A is
cheaper but structurally cannot reach loops (its translator only covers
the reflectable fragment), which is exactly where the `fib_loop` /
`isqrt.go` divergence loophole lives. C and D are the right end-state
(compositional totality; total-by-default adoption) but both sit *on top*
of B's per-function checking and one of them (C) depends on unfinished
mode-system work. E cannot do arithmetic termination and adds a whole
trusted analysis. Where a body *is* in the reflectable structural fragment,
route it through A's translator (C1) as a fast path — B and A compose, they
do not compete.

**First milestone (one to two weeks, opt-in, additive, no existing test
changes):**

1. Accept `[@@vox.decreases e]` (and the bare `[@@vox.total]`) on an
   *ordinary* `let rec`, reusing the existing metric parser
   (`vox_reflect.ml:839`, `translate_metric`) — it already validates
   "pure, parameters-only, int-valued."
2. At each recursive call inside a `[@@vox.total]` function, emit one VC
   `0 <= m[params] && m[args] < m[params]` under the call's path context,
   via the existing `emit_vc`. Start with a single int metric (no tuple),
   which already covers Fibonacci and isqrt.
3. At a `while`/`for` back-edge (`vox_verify.ml:3138`/`:3202`) with a
   `[@vox.decreases e]`, emit the analogous variant VC beside the existing
   invariant re-assertion.
4. Tests, mirroring the lemma suite's fail-closed exhibits:
   `total_isqrt.ml` (isqrt's `go` proved total with `decreases hi - lo`);
   `total_loop_fail.ml` (the `unit{ 1 = 2 }` / `let rec loop x = loop x`
   self-call **rejected** under `[@@vox.total]`); `total_while.ml` (a
   counting loop proved to terminate); and a `total_baddecr_fail.ml`
   (a metric that does not decrease, rejected at the solver) — the direct
   analogue of the passing `lean_lemma_baddecr_fail.ml`.

Deferred to later milestones, in order: lexicographic tuple measures
(Ackermann, McCarthy 91) and a per-datatype synthesized `size`;
default-measure inference (I1) with the invariant-inference sibling; the
total arrow (C) once the mode work lands; the module default (D) once I1
is trustworthy.
