# Verifying exceptions in vox — a design-options study

Date: 2026-07-06
Status: design study (no implementation). Small discriminating probes only.
Tree: `vox-exn` clone off `vox-proof-pane` (`c60ad6e2f`), boot-compiler + install built.

vox today is a **partial-correctness verifier over normal termination**: it
reasons about the value a function returns *when it returns normally*, and says
nothing about exceptional exits. Exceptions are on the public future-work list.
This document surveys the exceptional-postcondition literature, maps it onto
vox's actual machinery (probed, not assumed), and develops several design
options with honest tradeoffs, a benchmark set, and a staged recommendation.

Sibling design quests run concurrently and own the mutable-state and
uniqueness details this document only touches: **vox-sharedmut** (shared-mutation
semantics) and **vox-borrowlang** (borrowing / uniqueness language support).
Cross-references are flagged `[for reconcile]`.

---

## 0. Executive summary

The headline probe result reframes the problem: **`raise` does not "end the
verified path" in general today.** It ends the path *only in tail position*
(the implicit result obligation is skipped at a `raise` leaf). A `raise` in the
*middle* of a sequence or `let` leaves the continuation fully live — vox keeps
walking it and demands its verification conditions, with no vacuity. So before
we can *state* anything about exceptions, there is a foundational soundness-and-
usability fix (Milestone 0) that is independent of which surface design we pick.

Options, ranked later against a defined benchmark set:

| # | Option | Core idea (literature) | Unlocks | Automation risk | New TCB | Effort |
|---|--------|------------------------|---------|-----------------|---------|--------|
| 0 | **Divergence postcondition for `raise`** | bottom / `⊥` postcondition (Hoare partial correctness) | dead-code-after-raise stops spuriously failing; `unreachable_` generalizes | none (fewer goals) | none | ~days |
| 1 | **`raises` clauses** | JML `signals`, Why3/Krakatoa `raises` | per-exception postconditions; handler arms get the exceptional fact; Not_found idioms | low–medium | small, declared | ~1 quarter |
| 2 | **Logical `Result` desugaring** | exceptions-as-sum (Haskell/F\* `Result`, monadic VC) | uniform join of normal+exceptional at `try`; total reasoning | medium (grind on sums) | small | ~1–2 quarters |
| 3 | **Handlers get path facts only** | weakest-precondition negation; ESC-Java "cheap signals" | handler learns "no normal return happened"; zero new syntax | none | none | ~1 week |
| 4 | **Effect-row refinements** | Koka/Frank effect rows, F\* `EXN`/effect indices | which exceptions escape, checked in the type; bridges to effect-handlers work | high | medium | multi-quarter (v2+) |

**Recommendation:** ship **Milestone 0** immediately (it is a prerequisite for
everything and is a pure soundness/precision win), then build **Option 1
(`raises` clauses)** as v1, using **Option 3** semantics as its zero-annotation
default (a handler with no matching declared `raises` still learns the
path-negation fact). Keep **Option 2** as the internal VC *encoding* if the
join algebra gets hairy, and sketch **Option 4** as the forward bridge to the
effect-handlers future-work item without committing to it. This staging matches
vox's grind-first, honest-TCB culture: `raises` clauses are declarations that
turn into ordinary Lean obligations at raise sites and ordinary grind facts at
handlers, with a closed-world default that fails closed.

---

## 1. What vox does today (grounded + probed)

### 1.1 The verification model

`typing/vox_verify.ml` runs as a pass over the final typedtree, walking each
expression under a logical context `ctx = { cfacts; cscope }` and emitting VCs
`facts |- goal` discharged by a Lean 4 subprocess (solver error / unknown /
timeout all count as failure). The logic language is `Refinement.pred`
(`Pfun`, `Pis`, `Pfield`, `Pproj`, `Ptuple`, `Pquant`, `Pand`, `Por`, `Pimp`,
`Pbinop`, `Pnot`, …). Facts arrive through the channels `DESIGN.md` enumerates:
binder refinements and arrow **contracts**, selfification equations, unpacking,
`if`/`match` path facts (positive and negative), dependent application.

Three VC kinds (`vc_kind`): `Prove` (to the solver), `Runtime_check` (`assume_`,
compiled to a runtime test), `Assume` (`assume_unchecked_`, trusted).

### 1.2 `raise` / `failwith` — the actual semantics (probed)

There is **no special-casing of `raise` in `vox_verify.ml`** (no grep hits for
`raise`/`failwith` as primitives). `raise e` is an ordinary `Texp_apply`
returning a fresh-unknown value at bottom type `'a`. Concretely, from probes in
this tree (`_tmp/probe/*.ml`, compiled with the pinned Lean):

- **Tail position is vacuous.** `let allraise () : int{ _ = 5 } = raise Not_found`
  **verifies**, and `if b then 5 else raise Not_found : int{ _ = 5 }` verifies —
  only the `5` leaf's obligation is emitted (under path fact `b`), and the
  `raise` leaf's implicit result obligation is **skipped**. The result
  refinement is checked leaf-wise at tail positions; a `raise` leaf carries no
  obligation.

- **Non-tail continuation is *not* vacuous.** With
  `let need_false (_ : int{ false }) = 0`, the body
  `raise Not_found; need_false 7` is **DISPROVED** ("the goal is false
  unconditionally"). vox walks past the `raise` into the "dead" continuation and
  demands its contract-precondition VC. So mid-function `raise` does **not**
  establish a false/unreachable context.

- **Branch joins havoc.** `let x = if b then raise Not_found else 0 in refine_ (x : int{ x = 0 })`
  **fails** with counterexample `x = 1`: the `if` join produces an unconstrained
  `x` (the raise branch contributes an unknown value, exactly like any
  disagreeing branch), and the fact `x = 0` is lost.

The upshot: the sanctioned way to mark dead code today is **not** `raise` but
`unreachable_ (u : unit{ false })` — a function with a `false` precondition,
which only a genuinely dead path can call (`demo/lean_assume.ml`). `raise` gets
you vacuity *by accident* when it happens to sit in a tail leaf, and gets you
spurious obligations everywhere else. **Milestone 0 fixes this.**

### 1.3 Interrupted arms — the load-bearing havoc (already sound)

`match ... with exception E ->` and `try ... with` are handled by treating the
handler/exception arm as **interrupted**: it can be reached with the scrutinee
(or the `tried` expression) stopped *between writes*, so it runs from the
**pre-scrutinee state with everything the scrutinee/tried writes havocked**, and
receives **no facts** from the scrutinee. The canonical example is
`testsuite/tests/vox/mechanics/mutable.ml:339-372`:

```ocaml
let interrupted (p : bool) : {r:int | r = 1} =
  let mutable x = 0 in
  match (if p then raise Not_found); x <- 1 with
  | () -> refine_ x                    (* value arm: keeps x@1 = 1 *)
  | exception Not_found -> refine_ x   (* interrupted: x havocked, no facts *)
```

The value arm keeps the threaded fact `x@1 = 1`; the exception arm's VC is
`x@2 = 1` with `hypotheses: <none>` (an unconstrained fresh version). The
continuation of a match that *has* an interrupted arm likewise starts from the
havoc-join. Code: `Texp_match` (`vox_verify.ml` ~2949-3040, `exceptionless`
gate at 2496, `single_arm` at 2508) and `Texp_try` (~3414-3445).

This is a **sound over-approximation**: it never tells a lie about the state,
but it also cannot say *why* the exception was raised or *what payload it
carried*. Every design below builds on this havoc as its floor; the interesting
work is *adding facts back* to a handler arm safely.

`[for reconcile: vox-sharedmut]` — "which writes are guaranteed to have
happened at the raise point" is precisely the exceptional-postcondition-over-
mutable-state question. Today the answer is "none — all of them are havocked."
Any option that lets an exceptional post mention mutable state must agree with
whatever framing/write-set model sharedmut lands on. This doc designs the
*exceptional-post surface and VC join*; it does not design the write-set model.

### 1.4 `assume_` — the one place vox already reasons about an exceptional exit

`assume_ e` compiles (in `Translcore.vox_assume_check`, gated by
`runtime_check_gate`, `vox_verify.ml:1587`) into runtime code that evaluates the
refinement and **raises `Failure` on violation**; past the check the predicate
is a `Prove`-free assumed fact. `demo/lean_assume.ml` shows both the
trust-boundary use (`nth l (assume_ i)`) and the lemma-as-oracle use
(`rev_involutive`). Notably the gate observes that a partial operation (integer
division) *inside* a checked refinement **raises where the logic totalizes**,
which "aborts the check rather than mis-answering it" (`vox_verify.ml:1516`).
This is the seed of the correct mental model: **partial primitives are total in
the logic but raise at runtime**, and vox's soundness rests on the raising path
being unreachable in the continuation vox reasons about.

### 1.5 How specs travel across units

Arrow **contracts** (parameter preconditions and result refinements) ride the
`.cmi` as part of the refined type itself (`Trefine (skel, maps, pred)` inside
`type_expr`; `param_refinement`/`refinement_of_type` read them back). Lean
`[%%vox.lean]` blocks and `.mli` signatures compile to a `VoxSig_<Unit>.olean`
sidecar next to the `.cmi` (`typing/vox_module.ml`). `[@@vox.lemma]` exports a
proven proposition as an ambient grind fact (`vox_verify.ml:4233`).
**Consequence for exceptions:** an exceptional postcondition attached *to the
arrow type* rides the existing `.cmi` channel for free; one attached via a
*separate attribute table* needs a new serialization path. This strongly favors
encodings that live in the type (Options 1 and 2) over side tables.

---

## 2. Literature, mapped onto vox

**JML `signals` / `signals_only` (ESC-Java, OpenJML).** A method spec has a
normal postcondition (`ensures`) and per-exception exceptional postconditions
(`signals (E e) P`), plus `signals_only E1, E2` to bound *which* exceptions may
escape. Semantics: if the method terminates by throwing `E`, `P` must hold in
the post-state, with `e` bound to the thrown object. This is the closest
existing design to vox's arrow-contract model and directly inspires **Option 1**.
ESC-Java's pragmatic default — an unmentioned exception gives the caller no
information but is not itself an error — is **Option 3**.

**Why3 / Krakatoa `raises { E -> Q | ... }`.** WhyML has exceptions as a
first-class effect; a function's contract lists `raises` with a per-exception
postcondition, and the VC generator produces a *disjunctive* postcondition:
the computed WP of `try e with E -> h` splits into the normal WP of `e` and,
for each `E`, the WP of `h` under `e`'s exceptional post for `E`. Why3 tracks
the raise set in the effect and errors on an undeclared escaping exception
(closed world). This is the most complete blueprint for **Option 1**'s VC
generation and its closed-world default.

**F\* `EXN` effect (and the effect-lattice / lift / reify tradition).** F\*
models exceptions as a computation monad `EXN` with representation
`unit -> M (option a)` (or `result`), gives Hoare-style indexed specs
`EXN a (requires pre) (ensures post)` where `post` ranges over
`V x | E err`, and *reifies* effectful code into pure `Tot` code returning a
sum so SMT can reason about it. This is exactly **Option 2**: model exceptional
functions as sum-returning *in the logic only*, keeping the runtime unchanged.
F\*'s experience is the cautionary tale for automation — reasoning about the
`option`/`result` wrapper adds case-splits that a tactic must discharge.

**Exceptions-as-`Result`/sum desugaring (Haskell `ExceptT`, Rust `Result`,
monadic intermediate languages).** The oldest trick: compile `e : t` that may
raise into `e' : (t, exn) result`, `raise E` into `Error E`, and a handler into
a `match`. Verification then needs no exception theory at all — it is ordinary
sum reasoning. The tradeoff is that *every* call site becomes a match in the VC
stream, which either bloats the goal or requires the prover to prune the
`Error` branch using a `raises`-set fact. vox already generates match negatives
and joins; this reuses that machinery. **Option 2** adopts this *for the VC
encoding* while leaving surface OCaml and runtime untouched.

**Eiffel `rescue`/`retry` (design-by-contract).** Eiffel's model is
*operational recovery*, not logical specification: a `rescue` clause runs on
failure and either `retry`s or lets the exception propagate; the class invariant
must be re-established. The transferable idea for vox is the **invariant
obligation at the boundary**: on the exceptional exit of a stateful routine, a
declared invariant should still hold. This maps to a *weak* form of Option 1
where the exceptional post is fixed to "the declared type/mutable invariant,"
which is cheap and matches vox's existing declared-refinement re-attachment on
havoc-join. Not a primary option, but a useful default for stateful code
`[for reconcile: vox-sharedmut]`.

**Koka / Frank effect rows; F\* effect indices.** Row-polymorphic effect
systems track the *set* of effects (including `exn<E1,E2>`) a computation may
perform in its type, with row polymorphism for higher-order code. This is the
right long-horizon home for "which exceptions escape" and the natural bridge to
OCaml 5 effect handlers (vox's separate future-work item). It is heavyweight:
it wants an effect-inference pass and row unification. **Option 4** sketches it
and explicitly defers it.

**OCaml idioms that must work for adoption.**
- `Not_found`-style control flow: `Hashtbl.find`, `List.assoc` raise `Not_found`;
  callers `try ... with Not_found -> default` constantly. The handler wants the
  fact "the key was absent."
- Payload-carrying exceptions: `Failure of string`, `Invalid_argument`, and
  user exceptions like `Parse_error of position`. An exceptional post may need
  to talk about the payload (`signals (Parse_error p) p <= len input`).
- Local exceptions as early exit: `let exception Done in try ... raise Done ... with Done -> ...`
  — a common performant idiom; the "post" is usually trivial but the *raise
  site* must be allowed and the handler must recover the loop's accumulated fact.
- `match ... with exception E ->` — vox already **parses and walks** these
  (as interrupted arms); the design must *upgrade* them, not introduce them.

---

## 3. Design options

Notation: I use vox's existing surface conventions — refinement types
`t{ pred }` / `{x:t | pred}`, dependent arrows `(k : int) -> t{ ... }`,
attributes `[@... ]`. The `_` in a refinement denotes the refined value.

### Milestone 0 — divergence postcondition for `raise` (prerequisite)

**Idea + literature.** Hoare partial correctness: a diverging/never-returning
command has postcondition `⊥` (false), so its continuation is vacuously
verified. Today vox gets this right *only* in tail position; Milestone 0 makes
it uniform by recognizing bottom-typed / never-returning calls.

**Surface syntax.** None. This is a semantics fix. Optionally expose the
existing idiom name: keep `unreachable_` as the explicit spelling and make
`raise`/`Stdlib.raise`/`failwith`/`invalid_arg`/`assert false` behave the same
implicitly.

**VC generation.** At a call whose result type is `'a`-instantiated-to-bottom
via a `raise`-shaped primitive (or, more robustly, any expression the typer
marks *never-returning* — OCaml already computes this for the
`nonreturning-statement` warning), the walker returns a context with an added
`Pbool false` fact (or a dedicated "unreachable" marker) for the continuation.
Everything downstream is then discharged vacuously. Two implementation choices:
(a) recognize a small closed set of primitives (`raise`, `raise_notrace`,
`failwith`, `invalid_arg`, `assert false`); (b) piggyback on the type checker's
existing never-returns analysis. (b) is more principled and covers user
`val f : ... -> 'a` "always raises" functions, but (a) is a safe, tiny start.

**Lean encoding + grind.** Trivial: a `False` hypothesis closes any goal
(`grind`/`omega`/`exact absurd`). Fewer real goals reach the solver, so this is
a net *speedup*.

**Soundness.** Adding `false` to the continuation's facts is sound **iff** the
continuation is genuinely unreachable on the normal path — which is exactly what
"this expression never returns" means. The risk is *mis*-marking a returning
expression as never-returning; restricting to the known primitives (choice a)
makes this obviously safe. Asynchronous exceptions do not affect this: `false`
is asserted only *after* the raise executes, and an async exception before it
just means the continuation is even more unreachable.

**Unlocks / benchmark.** Directly fixes the "early-exit search" and
"parser with error recovery" benchmarks (below), where a `raise` sits mid-body
and today poisons the continuation. It generalizes `unreachable_` so users stop
writing the boilerplate helper.

**Incremental path.** ~days. No new syntax, no `.cmi` change, no new TCB. This
should land first regardless of which fuller option is chosen.

---

### Option 1 — `raises` clauses (per-exception postconditions) [RECOMMENDED v1]

**Idea + literature.** JML `signals` + Why3 `raises`. A function's arrow type
carries, alongside its result refinement, a set of *exceptional* postconditions
keyed by exception constructor, each a predicate that must hold at the raise and
that a matching handler may assume. Closed-world by default: the declared set
bounds which exceptions may escape.

**Surface syntax.** An attribute on the binding whose payload is a per-exception
predicate list, with the payload variables in scope:

```ocaml
val find : (t : tbl) -> (k : int) -> int{ mem k t && _ = sel t k }
  [@raises Not_found { not (mem k t) }]
```

Payload-carrying:

```ocaml
val parse : (s : string) -> ast{ renders _ = s }
  [@raises (Parse_error p) { p < strlen s }]
```

Multiple exceptions: `[@raises Not_found { ... } | (Failure m) { ... }]`.
A function that declares no `raises` and is not marked otherwise is **claimed
exceptionless** on its declared domain — a `raise` that could escape it is a
verification error (closed world). An explicit escape hatch
`[@raises _]` (or `[@raises open]`) opts back into today's open-world "may raise
anything, promises nothing" behavior for gradual adoption.

**VC generation.**
- *At a raise site* `raise E` inside a function `f` whose declared exceptional
  set is `R`: if `E ∈ R`, emit the obligation `facts |- P_E[payload := args]`
  (the declared exceptional post must hold in the pre-raise state), then mark the
  continuation unreachable (Milestone 0). If `E ∉ R`, **error** (undeclared
  escaping exception) — unless the raise is inside a `try` that handles `E`
  (then it never escapes `f`, so no obligation against `f`'s contract; the
  obligation is against the *handler entry*, see below).
- *At a call* `g x` where `g` declares exceptional post `Q_E`: `g` may raise `E`;
  the caller must account for it. If the call is under a `try` handling `E`, the
  handler arm for `E` receives `Q_E[payload := ...]` as a fact (this is the
  payoff — the handler *learns something*). If not under such a handler, `E`
  propagates, so the enclosing function must itself declare `E` in its `raises`
  (closed-world check), and the propagated exceptional post must *imply* the
  enclosing declared post (a subtyping/entailment VC, exactly like result-
  refinement subsumption).
- *At `try e with E -> h`:* walk `e`; for each handled `E`, walk `h` under the
  interrupted-arm state (§1.3 havoc) **plus** the exceptional post `Q_E`
  contributed by any call in `e` that can raise `E`. The join of the normal exit
  of `e` and the normal exit of every handler `h` is the `try`'s post, exactly as
  today's havoc-join but with the extra handler facts.

**Interaction with interrupted-arm havoc.** The handler still starts from the
havocked pre-`try` state — the exceptional post `Q_E` is *added on top*, and
`Q_E` may only mention state that is guaranteed at *every* raise point of `E`
inside `e`. In v1, restrict exceptional posts to **immutable data and payloads**
(no mutable-state claims), which sidesteps the write-set question entirely and
is sound against the existing havoc. Mutable exceptional posts are a v2 gated on
`[for reconcile: vox-sharedmut]`'s write-set model.

**Lean encoding + grind.** No new Lean theory. An exceptional post is an
ordinary `Refinement.pred`; the raise-site obligation is an ordinary `Prove` VC;
the handler fact is an ordinary hypothesis. The closed-world escape check is a
syntactic pass over raise sites, not a solver query. This is the key reason
Option 1 fits vox's grind-first culture: **it adds no automation burden** — the
same predicates, the same `grind`.

**Soundness.**
- *Unlisted exceptions:* closed world by default. A `raise E` with `E` not in the
  declared set and not locally handled is a compile-time error; there is no way
  to silently escape. This is stronger than JML's default and matches vox's
  fail-closed ethos. `[@raises _]` degrades to today's sound-but-uninformative
  behavior for un-annotated legacy code (the handler learns nothing, matching
  §1.3).
- *Payload soundness:* the payload binder is substituted by the raise site's
  argument translation (a variable/literal/pure reflected term — the same
  restriction `stable_arg_name` already enforces for dependent application).
- *Asynchronous exceptions* (`Sys.Break`, `Out_of_memory`, `Stack_overflow`,
  `Thread.kill`): **explicitly out of scope.** They can occur at *any* program
  point, so treating them as declared would make every point a raise site.
  vox continues to model them the way it models any abrupt termination it does
  not track: the reasoning is about the normal path, and an async exit only makes
  a continuation *more* unreachable, never invalidating a proven normal-path
  fact. We state this as an assumption: **exceptional posts are for synchronous,
  value-carrying exceptions raised by `raise`; asynchronous exceptions are not
  modeled and never escape a `raises` obligation.**

**Unlocks (ranked on the benchmark set §4):** Not_found-with-handler-proof (full),
early-exit search (full, with Milestone 0), assume\_'s own failure contract
(natural — `assume_` becomes `[@raises (Failure _) { not pred }]`), resource
cleanup (partial — needs mutable posts, v2), parser recovery (partial).

**Incremental path.** ~1 quarter. Parse the attribute; store the exceptional
set in the arrow type (rides `.cmi`); add the raise-site obligation and
closed-world check; add the handler-arm fact injection at `try`/`match
exception`. Composes with `[@@vox.lemma]` (a lemma about a total spec fn is
unaffected) and `via` (an exceptional post over a `via` value sorts at the image
like any other refinement). Milestone 0 is a hard prerequisite.

---

### Option 2 — logical `Result` desugaring (sum in the logic only)

**Idea + literature.** F\* `EXN` reification; exceptions-as-`Result`. Model an
exceptional function's *logical* meaning as returning
`Ok v | Err e` (a sum), while the **runtime and surface OCaml are unchanged**.
`raise E` denotes `Err E`; a handler denotes a `match` on the sum; `try`'s post
is the ordinary join of the `Ok` and handled-`Err` arms.

**Surface syntax.** Ideally none for the *user*: the same `[@raises ...]`
clauses as Option 1 can *elaborate into* the sum post. Or a lower-level spelling
where a function's result refinement is written over the sum directly:
`... -> int result{ match _ with Ok v -> v = sel t k | Err Not_found -> not (mem k t) }`.
The user-facing surface is Option 1; Option 2 is primarily an *internal VC
encoding* choice.

**VC generation.** The walker threads a "result-or-raised" value: every call to
an exceptional function binds a sum, and control-flow constructs pattern-match
it. This is uniform and composes trivially at `try` (it is just `match`), but it
changes the *shape* of many goals: a call `g x; k` becomes
`match ⟦g x⟧ with Ok r -> ⟦k⟧ | Err e -> propagate`, so the normal-path goal now
sits under an `Ok` case that the prover must select.

**Lean encoding + grind.** Introduce a `VoxResult` inductive (like the existing
`VoxCore` tuple/iarray theory) with `@[grind]` projection/injectivity lemmas.
The risk is real: every exceptional call adds a case-split, and `grind` must
prune `Err` branches using the `raises`-set fact. F\*'s experience says this is
tractable but noticeably heavier than direct predicates. Mitigation: only switch
to the sum encoding for functions that actually declare `raises` (pay-as-you-go);
exceptionless functions keep today's direct encoding.

**Soundness.** Very clean — it is ordinary total sum reasoning, no exception
theory to trust. Unlisted exceptions: the sum's `Err` constructor is closed over
the declared set (an `Err` outside the set is unrepresentable → escape is a type
error). Async exceptions: same story as Option 1 (out of scope; the sum models
only synchronous raises).

**Unlocks.** Same surface capability as Option 1, plus a cleaner story for
functions that are *both* value-returning and raising in a data-dependent way
(the sum makes the correlation explicit: `Ok v` *iff* `mem k t`). Its edge over
Option 1 is exactly correlated normal/exceptional specs; its cost is automation.

**Incremental path.** ~1–2 quarters, and higher risk. Recommended **not** as the
user surface but as a fallback *encoding* if Option 1's join algebra proves
insufficient for correlated posts. Compose: it would replace the VC-emission
core, so it is more invasive than Option 1.

---

### Option 3 — handlers get path facts only (minimal) [RECOMMENDED as Option 1's default]

**Idea + literature.** ESC-Java "cheap" default; weakest-precondition negation.
No exceptional postconditions at all. A handler arm simply learns the **negation
of the conditions under which the tried expression would have returned
normally** — i.e. "we are here because normal return did not happen." Combined
with vox's existing negative match facts, a handler for `E` after a scrutinee
that returns normally only when `pred` holds learns `not pred`.

**Surface syntax.** None.

**VC generation.** At `try e with E -> h` (or `match ... with exception E ->`),
keep today's interrupted-arm havoc, but additionally inject, as a handler fact,
the negation of the *normal-return* path condition of `e` when that condition is
translatable (the same `pattern_negation`/`cond_fact` machinery already used for
match negatives and `if`). For `Hashtbl.find`-style code where `find` is a spec
function with a `mem`-guarded normal post, this yields exactly `not (mem k t)` —
*without any `raises` annotation on `find`*, provided `find`'s normal contract
already says "returns normally ⇒ `mem k t`" (which a total-with-precondition or a
`raises`-annotated `find` implies).

**Lean encoding + grind.** None new. Pure fact injection.

**Soundness.** Sound because it only ever *adds a negation of a proven normal-
return condition*, which genuinely holds on the exceptional path. It is *weak*:
it says nothing when the normal-return condition is not translatable, and nothing
about payloads. But it never lies and needs no new syntax or TCB.

**Unlocks.** Not_found-with-handler-proof: *partial* — works when the raising
function's normal post is a clean guard (`mem`), which is the common case. Early-
exit search: needs Milestone 0. Others: little.

**Why it is the default, not the whole answer.** Option 3 is what a handler
should learn *even with zero annotations*. Option 1 layers *declared* facts on
top for the cases where the negation is not enough (payloads, correlated posts,
opaque conditions). Shipping Option 3 semantics as Option 1's zero-annotation
default gives immediate value to un-annotated code and a smooth adoption ramp.

**Incremental path.** ~1 week standalone. It reuses `pattern_negation`,
`cond_fact`, and the existing handler walk; the only new work is threading the
tried expression's normal-return condition to the handler context.

---

### Option 4 — effect-row refinements (sketch; v2+, keep effects out of v1)

**Idea + literature.** Koka/Frank effect rows; F\* effect indices. Track the
*set* of exceptions a computation may raise in its type as a row
(`exn<Not_found, Failure>`), with row polymorphism for higher-order functions,
and attach per-row-entry postconditions. This is the principled home for
"which exceptions escape" and the natural bridge to OCaml 5 **effect handlers**
(vox's separate future-work item): an effect row generalizes a raise set, and a
`match ... with effect (Op x) k ->` handler generalizes an exception handler with
a resumption `k`.

**Surface syntax (sketch).**
`val iter : (f : 'a -[exn<Break>]-> unit) -> 'a list -[exn<Break>]-> unit`
with posts attached per row entry. This needs an effect-annotation surface OxCaml
does not currently expose for `exn`, so it is contingent on language work beyond
vox.

**VC generation / Lean / soundness.** Would require an effect-inference pass and
row unification in the type checker, then per-entry obligation generation like
Option 1 but quantified over the row. Automation risk is high (row constraints
plus resumption reasoning for full effects). Async exceptions remain out of scope.

**Recommendation for Option 4.** Do **not** build it for exceptions v1. Design
Option 1's `raises` set so it is forward-compatible with becoming a row (keep it
an ordered set of constructor-keyed posts, avoid baking in closed-world at the
representation level even though the *default* is closed). Revisit when the
effect-handlers work starts; that is where row inference pays for itself.
`[for reconcile: this is the effect-handlers bridge — flag to whoever owns that]`.

---

## 4. Benchmark set (defined) and ranking

Five programs that discriminate the options. Each is a small module I would add
under `testsuite/tests/vox/` (demo + a `_fail` twin) to drive implementation.

1. **`find_or_default` (Not_found lookup with handler proof).**
   `find : tbl -> (k:int) -> int{ _ = sel t k }` raising `Not_found` when
   `not (mem k t)`; caller `try find t k with Not_found -> 0` and a downstream
   `refine_` that needs "either the found value or a default consistent with
   absence." *Discriminates:* Option 3 handles the common guard case; Option 1
   handles payload/correlated variants; Option 0 alone does not help the handler.

2. **`find_first` (early-exit search via local exception).**
   `let exception Found of int in try Array.iter (fun i -> if p a.(i) then raise (Found i)) ...; -1 with Found i -> i`,
   with a post relating the result to `p`. *Discriminates:* **Milestone 0** is
   mandatory (the `raise (Found i)` sits mid-body); Option 1 recovers the payload
   fact `p a.(result)`; Option 3 cannot (no clean normal-return guard).

3. **`with_resource` (resource cleanup / finally).**
   Acquire, run, release-on-both-paths (`try ... with e -> release (); raise e`),
   post = "resource released" as a mutable-state invariant. *Discriminates:*
   needs **mutable exceptional posts** → v2, gated on `[for reconcile:
   vox-sharedmut]`; Eiffel-`rescue`-style invariant-at-boundary is the cheap
   partial answer. Included to show the boundary of v1.

4. **`parse` (parser with error recovery).**
   `parse : string -> ast{ renders _ = s }` raising `Parse_error of pos`;
   caller recovers with a fact about the failing position. *Discriminates:*
   Option 1 payload posts; Option 2's sum shines if the ast/position correlation
   is intricate; Milestone 0 for the mid-body raise.

5. **`assume_`'s own failure contract.**
   Re-express the existing `assume_` runtime check as a `raises (Failure _)`
   spec: `assume_ e` behaves as `[@raises (Failure _) { not ⟦e⟧ }]`.
   *Discriminates:* validates that Option 1 can *describe vox's own existing
   exceptional exit* — a strong internal-consistency check and a dogfooding win.

**Ranking (capability, higher = more of the set):**
Option 1 + Milestone 0 covers 1, 2, 4, 5 fully and 3 partially → **best v1 value**.
Option 2 covers the same set with an edge on 4 but at automation cost → fallback
encoding. Option 3 alone covers 1 partially and nothing else → necessary default,
insufficient alone. Option 0 alone covers the *raise-site* half of 2 and 4 (stops
spurious failures) but adds no handler facts. Option 4 would cover all five plus
effects but is out of scope for v1.

---

## 5. Recommendation and first milestone

**Adopt Milestone 0 immediately, then Option 1 (`raises` clauses) with Option 3
semantics as its zero-annotation default. Hold Option 2 as an internal encoding
fallback and sketch Option 4 only as the effect-handlers bridge.**

Rationale against vox's culture:
- **Grind-first:** Option 1 adds *no* new Lean theory and *no* new automation
  burden — exceptional posts are ordinary predicates, raise-site obligations are
  ordinary `Prove` VCs, the closed-world check is syntactic. Option 2 would add
  a `VoxResult` theory and case-splits; that is why it is a fallback, not the
  default.
- **Honest TCB:** Milestone 0 and Options 1/3 add *no* trusted surface (the
  closed-world check *reduces* what can silently escape). Everything remains
  Lean-checked; a false exceptional post has no proof and fails closed, exactly
  like `[@@vox.lemma]`.
- **`.cmi` for free:** exceptional posts live in the arrow type, riding the
  existing contract channel; no new serialization.
- **Fail-closed default:** closed-world `raises` turns "silently escaping
  exception" into a compile error, matching vox's existing posture, with
  `[@raises _]` as the explicit gradual-adoption escape hatch.

**First milestone (concrete, ~1–2 weeks):**
1. Implement **Milestone 0**: recognize `raise`/`raise_notrace`/`failwith`/
   `invalid_arg`/`assert false` (closed set) as never-returning; assert `false`
   into the continuation's context. Add `mechanics/exn_divergence.ml` (dump +
   solver) proving `raise Not_found; need_false 7` now verifies and that a
   mid-body raise no longer poisons the continuation. This is a standalone
   soundness/precision win landable on its own.
2. Land **Option 3**'s handler-fact injection at `try`/`match exception`
   (negation of the tried expression's translatable normal-return condition),
   with `demo/exn_find_default.ml` (benchmark 1) as the proof of value.

These two ship independently, deliver immediate value, carry no new TCB, and set
up the `raises`-clause parse + `.cmi` storage + raise-site-obligation work that
is Option 1 proper.

---

## Appendix A — probe transcript (this tree)

Compiled with `_build/_bootinstall/bin/ocamlc.opt -vox-solver-path <pinned-lean> -c`.

- `let allraise () : int{ _ = 5 } = raise Not_found` → **verifies** (tail raise
  leaf's result obligation skipped).
- `if b then 5 else raise Not_found : int{ _ = 5 }` → **verifies**;
  `if b then 4 else raise Not_found : int{ _ = 5 }` → **fails** on the `4` leaf
  (`goal 4 = 5`, hypothesis `b`), raise leaf silent → tail obligations are
  leaf-wise under path facts, raise leaf skipped.
- `let need_false (_ : int{ false }) = 0 ;; raise Not_found; need_false 7` →
  **DISPROVED** ("goal is false unconditionally") → non-tail continuation after a
  raise is live, not vacuous. **(Milestone 0 target.)**
- `let x = if b then raise Not_found else 0 in refine_ (x : int{ x = 0 })` →
  **fails**, counterexample `x = 1` → branch join havocs; raise branch is an
  unconstrained join input.
- Interrupted arm reference: `mechanics/mutable.ml:339-372` — exception arm and
  post-match continuation get `hypotheses: <none>` over a havocked version.

## Appendix B — key code locations

- `raise` has no special case: `typing/vox_verify.ml` (`Texp_apply` at 2617;
  generic traversal at 3446).
- Interrupted arms: `Texp_match` 2949-3040 (`exceptionless` 2496, `single_arm`
  2508, `pattern_negation` 2375); `Texp_try` 3414-3445.
- `if` join by havoc: 3041-3095. Function result obligation is leaf-wise via
  tail elaboration (`result_refinement` 2322).
- `assume_` runtime check / exceptional exit: `runtime_check_gate` 1587,
  partial-op-raises note 1516; `demo/lean_assume.ml`.
- Contract channel (`.cmi`): `Trefine` in the type; `param_refinement` 1025,
  `refinement_of_type` 984. Lean sidecar: `typing/vox_module.ml`. Lemma facts:
  4233.
