# Refinement subtyping instead of HM unification in vox

Design-options study. Author: subtyping-quest agent. Date: 2026-07-06.
Branch `vox-subty` (clone of `vox-proof-pane`, tip c60ad6e2f).

Status: **study only.** No compiler change is proposed for immediate
landing; the deliverable is the option analysis, the wart catalog that
motivates it, and a recommended first milestone.

---

## 0. TL;DR

Today a vox refinement `int{ p }` is the type `Trefine (int, maps, p)`
that rides on OCaml's Hindley-Milner (HM) unification: it unifies "like
`int`" with a bolted-on requirement that the two predicates be
**syntactically equal** (`Refinement.equal`). That equality demand is
the wrong relation at almost every edge where two refined types meet.
It is *sound* (equality implies entailment both ways) but *incomplete*:
it rejects programs that are obviously safe, and it is the direct cause
of at least five items on the vox backlog.

The good news, discovered while grounding: vox is **already
half-bidirectional**. At an *expected* refined type (a let annotation, a
function result, a constructor/field argument, an application argument
for a refined parameter) the compiler **strips the refinement to the
skeleton and re-imposes it as an entailment VC** (`implicit_refine.ml`;
`emit_vc` at `typing/vox_verify.ml:2600,2669`). Subtyping already exists
for the *erase* direction at explicit `:>` coercions
(`typing/ctype.ml:8016`). What is missing is (a) subsumption at flow
edges that are not annotation boundaries, and (b) subtyping **under type
constructors** (arrows, data), where `unify`/`moregen` still demand
predicate equality and there is no covariant erase rule.

The recommendation is **Option 2: a bidirectional refinement layer over
HM**, staged so nothing breaks: strip refinements off the unification
graph entirely (HM unifies skeletons only), and let a single
checking/synthesis pass — folded into the `vox_verify` tree walk that
already emits VCs — reconcile refinements by entailment. Subsumption
sites just emit entailment VCs into the existing stream, which is
exactly the property that makes this cheap. The first milestone is the
smallest self-contained slice with an independent payoff: **higher-order
/ under-constructor erasure and covariant arrow subtyping** (fixes the
two hardest warts, W1/W2), implementable as a directional
`moregen`-level rule without touching symmetric `unify`.

---

## 1. How refinement meets unification today (the CHECKED vs UNIFIED map)

There are three distinct relations in the type checker, and refinements
are handled differently in each. Understanding which relation fires at
which program edge is the whole game.

### 1.1 `unify` — symmetric equality, the HM workhorse

`typing/ctype.ml:5132`:

```ocaml
| (Trefine (t1, m1, p1), Trefine (t2, m2, p2))
  when Types.vox_maps_equal m1 m2 && Refinement.equal p1 p2 ->
    unify uenv t1 t2
```

Two refined types unify iff their maps and predicates are
**structurally equal**; then the skeletons unify. If the predicates
differ, this arm's guard fails and control falls to the generic
`link_type` merge (`:5141`). `unify` is symmetric and *merges* the two
type nodes into one — there is no "expected" side, so it fundamentally
cannot express an asymmetric (subtype) relation. Dependent arrows get
extra machinery: the binder of one side is renamed to the other's before
recursing (`:5171`, `Vox_dep.subst_binder id1 ~by:(Pvar id2)`) so that
alpha-equivalent dependent binders survive the merge. This rename is the
source of the errortrace artifact in W4 (#51).

`unify` fires at **every ordinary HM flow edge**: branch joins, the two
sides of an application's function/argument skeleton, `let`-without-
annotation inference, `ref` contents, tuple/record component inference.

### 1.2 `moregen` — directional, signature/expected matching

`typing/ctype.ml:6534`:

```ocaml
| (Trefine (t1, m1, p1), Trefine (t2, m2, p2)) -> (
    match vox_trefine_match env t1 m1 p1 t2 m2 p2 with
    | Some (s1, s2) -> moregen ... s1 s2
    | None -> raise_unexplained_for Moregen)
```

`moregen` (`is [t1] more general than [t2]`) is used for module-
signature inclusion and for the "does the inferred type match the
declared one" check. It is *directional*, and it already does one
genuinely-subtyping-flavored thing: `vox_trefine_match`
(`typing/ctype.ml:4038`) first calls `vox_flatten_view` to **normalize
away abstraction boundaries** (an interface's `t{ p }` where `t` is
abstract `refines (iset)` is flattened to line up with the
implementation's `tree{ bst _ } via elems`), *then* compares maps and
predicates for equality. This is ad-hoc subtyping (W6): a bespoke
directional normalization glued in front of an equality check. But it
stops at equality — it never emits an entailment obligation.

### 1.3 `subtype` — the only real subtyping, gated behind `:>`

`typing/ctype.ml:8009-8021`:

```ocaml
(* vox: refined types.  ... A refined type is a subtype of its
   (unrefined) skeleton: this is the coercion [(e :> ty)] that erases
   the refinement.  The reverse direction is NOT admitted. *)
| (Trefine (u1, m1, p1), Trefine (u2, m2, p2))
  when Types.vox_maps_equal m1 m2 && Refinement.equal p1 p2 ->
    subtype_rec env ... u1 u2 cstrs
| (Trefine (u1, _, _), _) ->
    subtype_rec env ... u1 t2 cstrs        (* erase to skeleton *)
```

Here — and *only* here — a refined type is admitted as a subtype of its
skeleton. `subtype` runs solely at the explicit `(e :> ty)` coercion.
It still requires *equality* when both sides are refined (it does not
call the solver), so even the coercion cannot strengthen or weaken a
predicate; it can only forget one entirely.

### 1.4 The vox VC layer — semantic entailment, driven by intro markers

Separately from all three relations, `vox_verify.ml` walks the typed
tree and emits entailment VCs (`emit_vc`, `typing/vox_verify.ml:1822`):

- **intro forms** `refine_ e` / `assume_ e` carry the vox attribute and
  a refined `exp_type`; the goal is the predicate at the value's logical
  name (`:2600`).
- **application arguments for a refined parameter**: the argument must
  satisfy the parameter predicate, `subst_bound ~by:(name arg) p`, as a
  `Prove` VC (`:2669`) — *this is already a subsumption obligation at a
  flow edge*, generated because typing stripped the refinement and left
  the fact to be proved.
- **function results, constructor args, record fields, let
  annotations**: same pattern — the value is typed at the skeleton and
  the annotation's predicate becomes a VC (`implicit_refine.ml`).

The crucial architectural fact: **the entailment machinery already
exists and already fires at annotation-shaped flow edges.** The VC
stream is the natural place for subsumption to live. Every option below
is really a question of *which edges emit into that stream, and whether
the type checker stops demanding equality first*.

### 1.5 Summary table: what fires where

| Program edge | Relation used | Refinement treated as |
|---|---|---|
| `let x : T{p} = e` (annotation) | strip + VC | **CHECKED** (entailment) |
| fn result `: T{p}` | strip + VC | **CHECKED** |
| arg to refined param `f (e)` | strip + VC | **CHECKED** |
| ctor arg / record field at `T{p}` | strip + VC | **CHECKED** |
| `(e :> T)` explicit coerce | `subtype` | erase-only subtyping |
| `if c then e1 else e2` (synth) | `unify` | **UNIFIED** → erased to skeleton |
| unannotated `let`, tuple/ref infer | `unify` | **UNIFIED** (equality) |
| refined value **under an arrow/ctor** | `unify`/`moregen` | **UNIFIED** (equality; no erase) |
| module sig inclusion | `moregen` | flatten-then-**UNIFIED** |
| dependent-arrow value passed HO | `unify`/`moregen` | **UNIFIED** (equality) |

The "CHECKED" rows are already doing the right thing. Every wart lives
in a "UNIFIED" row.

---

## 2. Wart catalog (empirical, one repro each)

All repros run against the `vox-subty` build
(`_build/_bootinstall/bin/ocamlc.opt -vox-dry-run [-dump-vc] -c f.ml`).
Sources kept in `_probes/`. `-vox-dry-run` elaborates and emits VCs
without invoking Lean, so these show the *type-system* behavior, not
proof success.

### W1 — Higher-order / under-constructor erasure fails (the big one)

First-order erasure works. `_probes/foerase.ml`:

```ocaml
let three : int{ _ = 3 } = 3
let n : int = three          (* OK: expected-type strip drops refinement *)
let plus (a:int) (b:int) = a + b
let s = plus three 4         (* OK: refined arg to plain param *)
```

Under an arrow it does not. `_probes/hof_plain.ml`:

```ocaml
let apply (g : int -> int) (x:int) = g x
let inc (x:int) : int{ _ = x + 1 } = x + 1
let r = apply inc 5
```

```
Error: The value inc has type (x : int) -> int{ _ = x + 1 }
       but an expression was expected of type int -> int
       Type int{ _ = x + 1 } is not compatible with type int
```

Forgetting a refinement is *always* sound (it is exactly the `subtype`
rule of §1.3), but `moregen`/`unify` never apply the erase rule under a
type constructor, so a refined-result function is not accepted where a
plain function is expected. Escape hatch today: manual eta-expansion +
`:>`, `_probes/coerce.ml` (`fun x -> (inc x :> int)`) compiles. Clunky
and viral.

### W2 — Covariant result strengthening rejected (sound program refused)

`_probes/arrow_covar.ml`:

```ocaml
let use (g : (x:int) -> int{ _ > x }) = g 0
let h (x:int) : int{ _ = x + 1 } = x + 1
let r = use h
```

```
Error: ... Type int{ _ = x + 1 } is not compatible with type int{ _ > x }
```

`h` returns `_ = x+1`, which entails `_ > x`, so `h` **is** a valid
`(x:int) -> int{ _ > x }` by covariant-result subtyping. HM equality
rejects it. There is no way to express this without rewriting `h`'s
signature. This is the canonical "subtyping relaxes, unification
refuses" case.

### W3 — Synthesis-position branch join erases even when arms agree

`_probes/branch_same.ml`, inferred with `-i`:

```ocaml
let f c (x : int{ _ > 0 }) (y : int{ _ > 0 }) = if c then x else y
(* val f : bool -> int{ _ > 0 } -> int{ _ > 0 } -> int   <-- result bare int *)
```

Both arms carry `_ > 0`, yet the `if`'s synthesized type is bare `int`
— the common refinement is dropped. Contrast at an *expected* refined
type, `_probes/branch_expected.ml`, which works by entailment (two VCs,
one per arm, each under its branch condition):

```
goal: x > 0   hypotheses: c; ...
goal: y > 0   hypotheses: not c; ...
```

So join-by-entailment already works **downward** (checking against an
expected type) but never **upward** (synthesis): the HM least-upper-
bound of two refined types is "erase to skeleton". (Note: because
scalar refined *locals* bind at the skeleton, `x`/`y` are plain `int`s
whose `>0` is a fact; the erasure here is of the join's *result* type,
which is why `f`'s result loses the refinement. The type-level bite is
sharper for module-level values and data/abstract types that stay
refined — see W1.)

### W4 — Dependent arrows compared by structural equality, confusing trace

`_probes/pp_artifact.ml`:

```ocaml
let use (g : (a:int) -> int{ _ = a + 1 }) = g
let h (b:int) : int{ _ = b + 2 } = b + 2
let r = use h
```

```
Error: The value h has type (b : int) -> int{ _ = b + 2 }
       but an expression was expected of type (a : int) -> int{ _ = a + 1 }
       Type int{ _ = a + 2 } is not compatible with type int{ _ = a + 1 }
```

Binder pairing (`vox_with_binder_pair`, `typing/ctype.ml:3995`) and the
unify merge-rename (`:5171`) correctly align `b` to `a`, so alpha-
equivalence is handled — but the comparison is still `Refinement.equal`,
and the errortrace prints the two predicates after renaming, which for a
genuinely-equal-but-noisy pair renders as the "`p + p`" artifact logged
as backlog #51 (the trace looks like it is rejecting `p` against
itself). Under subtyping, arrow comparison becomes contravariant-domain
/ covariant-codomain with an entailment VC, and the diagnostic becomes
"could not prove `a+2` entails `a+1`" rather than a type-incompatibility.

### W5 — Let-annotation self-capture (#72), scope symptom of the marriage

`_probes/selfcap.ml`:

```ocaml
let x : int{ _ = 7 } = 7
let x : int{ _ = x } = 3     (* checks 3 = 3, not 3 = 7; wrongly accepted *)
```

`-dump-vc` shows the second obligation is `3 = 3` (self-captured
tautology) instead of `3 = 7`. Root cause (from the vox-scopes NOTES and
`typing/typecore.ml:3978`): a constrained `Ppat_var` pushes a `Vox_self`
scope entry so that `(x : int{ _ = x })` resolves `x` to the bound value
— a mechanism designed for dependent *parameters* `(x : int{ _ = x })`,
firing wrongly for a plain `let`. This is an elaboration/scope bug, not a
unification bug, but it belongs in the catalog: it exists because
refinement binder scope is *derived from the HM pattern-binding scope*
rather than given its own discipline. A principled bidirectional
elaboration (Option 2) elaborates the annotation in the **outer**
environment and fixes it as a side effect.

### W6 — `vox_flatten_view` / `vox_trefine_match` are ad-hoc subtyping

`typing/ctype.ml:4018-4043`. The via/refines boundary reconciliation
already normalizes nested `Trefine`s (append maps, push the outer
predicate's bound value through them, conjoin) and compares. This is a
one-off, directional-flavored "these two presentations denote the same
sort" rule bolted into `moregen`. A real subtyping judgment subsumes it:
the flatten becomes a *normalization* step feeding a general entailment
check, and the same code path handles interface-vs-impl reconciliation,
covariant strengthening, and erasure uniformly instead of as three
special cases.

### Scorecard preview

W1 and W2 are the load-bearing correctness/ergonomics warts (sound
programs rejected). W3 is an expressiveness gap (facts silently lost).
W4 is diagnostics. W5 is a soundness *acceptance* bug (a false
annotation accepted) that happens to live nearby. W6 is architectural
debt. §5 scores each option against them.

---

## 3. Literature grounding

Cited from training knowledge; flagged where uncertain (no web access).

- **Liquid Types** (Rondon, Kawaguchi, Jhala, PLDI 2008; and Vazou et al.,
  *Refinement Types for Haskell*, ICFP 2014). The canonical design:
  base types carry a refinement `{v:b | p}`; the subtyping judgment
  `Γ ⊢ {v:b|p} <: {v:b|q}` reduces to the **implication VC**
  `Valid(⟦Γ⟧ ∧ p ⇒ q)` discharged by an SMT solver. Inference of
  refinements uses **refinement variables κ** placed at binders and
  solved by **predicate abstraction** over a fixed, user/heuristic-
  supplied set of *qualifiers* via a Houdini-style greatest-fixed-point
  (start with all qualifiers, drop those that break an implication).
  This is *exactly* vox's `emit_vc`-reduces-subtyping-to-entailment,
  generalized to every edge, plus an inference engine. Key transferable
  idea: **subtyping = entailment VC**; vox already believes this at
  annotation edges.

- **Flux** (Lehmann, Vazou, Jhala, *Flux: Liquid Types for Rust*, POPL
  2023). Refinement types over Rust with **bidirectional** checking and
  subtyping integrated with **ownership/borrows**. Directly relevant to
  vox's borrowing sibling: Flux threads refinements through `&mut`
  using "strong updates" at owned locations, and its subtyping respects
  the ownership discipline. Confirms bidirectional + subtyping is the
  mainstream choice for a refinement layer over an affine/substructural
  base — which is what OxCaml's mode system is.

- **F\*** (Swamy et al.). Refinement subtyping via SMT plus
  **bidirectional** type-and-effect checking; expected types drive
  checking, subsumption inserts SMT queries. F\*'s pragmatic lesson:
  keep subtyping at the *checking* boundary and let the synthesis side
  stay simple; do not try to infer refinements everywhere.

- **DML / ATS** (Xi, Pfenning). Index refinements `int(n)` with a
  separate **constraint domain** (linear arithmetic), where the index
  language is deliberately restricted so constraint solving is decidable
  and inference is index-variable unification, not full entailment.
  Lesson: restricting the refinement logic buys decidable inference —
  vox deliberately does *not* restrict (it delegates to Lean), so vox
  cannot have DML-style complete inference; it must annotate.

- **Stardust** (Dunfield). Refinement + intersection/union types with
  bidirectional checking; notable for showing how union types give a
  *principled* branch-join (the `if` synthesizes `T1 ∨ T2`) rather than
  erasing — relevant to W3.

- **MLsub / Algebraic Subtyping** (Dolan, *Algebraic Subtyping*, 2017;
  Dolan & Mycroft, POPL 2017). Principal types **with** subtyping via
  polar (positive/negative) types and **biunification**. This is the
  headline "subtyping + principality" result. Assessment for vox
  (detailed in Option 4): its guarantees rest on the subtyping order
  being a **decidable, structural lattice** so that constraints close by
  biunification. Refinement entailment is neither structural nor
  decidable (it is Lean). So MLsub's machinery does not transfer as an
  *implementation*; its **conceptual** contribution — tracking each type
  variable's use in positive (produced) vs negative (consumed) position
  — is a useful lens for where subsumption may safely be one-directional,
  and it lines up with vox's `via` (produced image) vs `refines`
  (consumed sort) and the borrowing sibling's now/fin polarity.

- **Bidirectional typing survey** (Dunfield & Krishnaswami, *ACM
  Computing Surveys* 2021). The reference for the check/synth
  discipline, the "annotations at redexes" principle, and where
  subsumption rules belong. The design backbone of Option 2.

Synthesis of the literature: the mainstream, battle-tested design for "a
refinement layer over an existing type system with an external solver"
is **bidirectional checking + subtyping-as-entailment-VC**, with
inference either absent (annotate) or via κ-variables + qualifier
abstraction (Liquid). Algebraic subtyping is the outlier that buys
principality but only for a decidable structural order, which vox's
Lean-backed entailment is not.

---

## 4. The options

Four options, from least to most invasive. Each answers (a) idea +
literature, (b) where subsumption applies, (c) inference/principality/
errors, (d) elaboration architecture + migration, (e) which warts it
fixes, (f) cost/risk, (g) composition with via/refines/modes/dependent
arrows.

Common vocabulary: the **skeleton** of `Trefine (s, maps, p)` is `s`
after stripping refinements. All options preserve the invariant that HM
must at least make skeletons agree — that is non-negotiable and already
correct.

---

### Option 1 — Coercion-site subtyping (minimal: relax the equality guards)

**(a) Idea.** Keep HM and the current tree walk exactly as they are.
Change only the four `Trefine`/`Trefine` comparison sites so that,
instead of demanding `Refinement.equal p1 p2`, they (i) unify/compare
skeletons and (ii) in the *directional* relations (`moregen`,
`subtype`), emit an entailment VC `p_actual ⊨ p_expected` into the vox
stream; and add the missing covariant **erase-under-constructor** rule
so a refined type is a subtype of its skeleton at any covariant
position. This is F\*/Liquid "subtyping = VC" applied at exactly the
sites that today demand equality, and nowhere else.

**(b) Subsumption scope.** Directional relations only. `moregen`
(signature inclusion, expected-type match) and `subtype` (`:>`) gain
entailment; symmetric `unify` keeps equality (it has no "expected" side,
so it *cannot* emit a directional VC without inventing one — see risk).

**(c) Inference / principality / errors.** No refinement inference
added; refinements remain annotation-driven exactly as today.
Principality of *skeletons* is untouched (HM unchanged). The relation
`moregen` becomes non-transitive-looking to users only in that it now
sometimes emits a VC instead of failing — acceptable. Errors: an
arrow/covariant mismatch becomes an entailment VC ("could not prove
`a+2` ⊨ `a+1`", W4) which is strictly better than the type-
incompatibility trace.

**(d) Architecture / migration.** Localized to `typing/ctype.ml`
(4 sites) + a hook to push VCs from `moregen` into the collector that
`vox_verify` drains. The subtlety: `moregen`/`subtype` run *inside* type
checking, before the `vox_verify` pass, so they need a side-channel to
register obligations (a `ref` list drained by `Vox_verify.check_*`, or
routing through the existing `emit_vc` collector). Migration: the
existing test corpus is a *superset* of accepted programs — nothing that
compiles today stops compiling (equality ⇒ entailment), so the corpus
stays green; new programs (W1/W2) start compiling. Expect-test VC dumps
gain new obligations at the relaxed sites; re-promote.

**(e) Warts fixed.** W1 (erase under constructor — the new covariant
rule), W2 (covariant strengthening — the moregen VC), W4 (diagnostic
becomes an entailment). **Not** fixed: W3 (synthesis join still in
symmetric `unify`, still erases), W5 (scope bug, orthogonal), W6
(flatten stays a special case, though now feeding a VC).

**(f) Cost / risk.** *Soundness risk is concentrated and real.* The
gap-C history (`typing/subst.ml:830`) shows how sharp binder/cmi pairing
is; touching the `Trefine` arms of `unify` risks the merge-rename
interacting badly with a newly-relaxed guard. Mitigation: do **not**
touch symmetric `unify`'s guard at all in this option — leave `unify`
demanding equality (so branch-join/infer are unchanged), and put all
relaxation in `moregen`/`subtype`, which are directional and already the
signature-matching path. Performance: entailment VC volume rises only at
sites that previously *failed* (they now succeed with a VC) plus HO
positions; bounded. This is the **cheapest** option and a strict
improvement, but it leaves refinements on the unification graph, so the
structural warts (W3, the `p+p` merge in `unify`) persist and the design
stays a patchwork.

**(g) Composition.** via/refines: `vox_trefine_match`'s flatten stays as
the normalizer feeding the new entailment VC (W6 partially rationalized).
Modes: untouched (modes ride the skeleton/arrow, orthogonal). Dependent
arrows: the moregen arrow rule already pairs binders; add the covariant-
codomain VC under the pushed binder pair — clean, and it is where W2/W4
live.

---

### Option 2 — Bidirectional refinement layer over HM (RECOMMENDED)

**(a) Idea.** Take refinements **off the unification graph**. HM unifies
*skeletons only* — `unify`/`moregen`/`mcomp` never look at predicates
(they treat `Trefine (s, _, _)` as `s`). A separate **bidirectional
refinement judgment** — check `Γ ⊢ e ⇐ T{p}` and synthesize
`Γ ⊢ e ⇒ T{p}` — folded into the `vox_verify` tree walk that already
exists, reconciles refinements: at every *checking* edge it emits the
entailment VC; at every *synthesis* edge it computes the refinement
bottom-up (from literals, `refine_`, callee result types, and branch
joins). This is the Dunfield-Krishnaswami / F\* / Liquid-bidirectional-
core architecture. It is the *completion* of what vox already does at
annotation edges — generalized to all edges and made the sole owner of
refinements.

**(b) Subsumption scope.** At **annotation / checking boundaries**
(the sane middle): infer refinements up, check them down against
expected types, subsumption (entailment VC) fires exactly at the ⇐
boundary — which additionally now includes arrow codomains, data
components, and branch arms against an expected type. Synthesis never
emits a subsumption VC; it either carries the refinement forward or
joins (see W3 handling).

**(c) Inference / principality / errors.** Skeleton principality:
untouched and *cleaner* (HM no longer perturbed by predicate equality;
the `unify` merge-rename for binders, `:5171`, can be deleted — a net
simplification and the real fix for the `p+p` artifact). Refinement
inference: none beyond bottom-up synthesis (this is a *checking* system,
not Liquid inference — annotate function signatures and you are done, as
today). Branch join W3: two principled choices, pick per position — (i)
at a checking position, check each arm against the expected `p` (already
works); (ii) at a synthesis position, either keep erase-to-skeleton (the
current principal, conservative answer) or synthesize the **disjunction**
`p1 ∨ p2` (Stardust-style union), which the ghost logic supports
(`Refinement.Por`) and Lean discharges. Recommend erase-by-default with
opt-in `∨`-join to avoid VC blowup. Errors: uniformly entailment-shaped;
the checker knows the *expected* predicate at every ⇐ edge, so messages
say "value's refinement `q` does not entail expected `p` here", far
better than unification traces.

**(d) Architecture / migration.** The pieces already exist:
`refinement_of_type`, `result_refinement` (`vox_verify.ml:2322` — this
*is* a nascent synthesis judgment, already handling let-body/sequence-
tail/if), `emit_vc`, and the intro/contract obligation sites. Option 2
is largely *reorganizing* these into an explicit `check`/`synth`
mutual recursion and removing the predicate-equality guards from
`ctype.ml`. Changes: `typing/ctype.ml` — make the four `Trefine` arms
skeleton-only (strip and recurse); delete the merge-rename. `typing/
vox_verify.ml` — promote the tree walk to explicit bidirectional
judgments; the VC stream is unchanged in shape. `typetexp`/`typecore` —
the elaboration of `T{p}` is unchanged; the self-name scope (W5) is
fixed by elaborating a `let` annotation's refinement in the outer env
(the bidirectional pass owns scope, so the pattern-binding hack is
removed). **Migration is the delicate part** and must be staged (§6):
stripping predicates from `unify` means any program that *relied* on
`unify` rejecting a predicate mismatch (rather than a VC catching it)
changes its failure mode from a type error to a VC — still rejected, but
at a different layer. The corpus must be re-run and each rejection
re-confirmed to fire at the intended layer (the AGENTS.md
"eyeball promoted expectations" discipline). No *accepted* program
should change.

**(e) Warts fixed.** W1, W2 (subsumption at ⇐ edges under constructors),
W3 (join is now an explicit synthesis rule with a real choice), W4 (no
merge-rename → no artifact; mismatches are entailment VCs), W5 (scope
discipline owned by the pass), W6 (flatten becomes the normalizer of the
one entailment rule). **All six.**

**(f) Cost / risk.** Highest design cost of the "sane" options, but the
soundness argument is *cleaner*, not dirtier: refinements no longer
touch the mutable unification graph, so the whole class of
merge/rename/cmi-pairing hazards (gap-C, `p+p`) is *removed from the
unify path*, not patched. The residual risk is the migration itself
(failure-mode shifts), managed by staging. Performance: VC volume is the
concern — subsumption now fires at more edges. But (i) checking edges
that carry an *identical* refinement forward emit **no** VC (the
`implicit_refine` "flow-through stays rigid" optimization generalizes:
if synthesized `p` is syntactically the expected `p`, skip the VC), and
(ii) most edges are erase-to-skeleton (no VC). Expected net increase is
modest and pays for the warts. Measure before/after on the corpus.

**(g) Composition.** via/refines: the directed hooks *become* the
subtyping rules — `vox_flatten_view` is the normalizer, the boundary
reconciliation is a ⇐ check emitting an entailment VC; interface-vs-impl
is just checking the impl's synthesized refinement against the
interface's expected one. This is the single biggest conceptual
cleanup. Modes: orthogonal (skeleton-level), but the bidirectional pass
should thread mode information as F\*/Flux thread effects, relevant to
the borrowing sibling. Dependent arrows: contravariant-domain /
covariant-codomain checking under a binder pair, with the codomain VC
under the substituted binder — the arrow rule is where dependency and
subtyping compose, and it is clean in a bidirectional setting because
the *expected* arrow supplies the binder.

---

### Option 3 — Liquid Types: subtyping + κ-variables + qualifier inference

**(a) Idea.** The full Rondon/Jhala design. Uniform subtyping
`Γ ⊢ {v:b|p} <: {v:b|q} ⟶ Valid(Γ ∧ p ⇒ q)` at **every** flow edge,
plus **inference** of refinements: place refinement variables κ at let-
binders, function params/results, and branch joins, collect subtyping
constraints `Γ ⊢ t <: κ` / `Γ ⊢ κ <: t`, and **solve** the κ's by
predicate abstraction over a qualifier set (Houdini fixpoint). vox would
infer refinements, not just check them.

**(b) Subsumption scope.** Everywhere (full lattice, entailment VC at
every edge), with κ-variables absorbing the inference.

**(c) Inference / principality / errors.** This is the *only* option
that infers refinements. But: **principality is lost** in the HM sense —
the inferred refinement is the strongest expressible in the qualifier
set, which is a heuristic choice, not a principal type; changing the
qualifier set changes the inferred type. Errors are the classic Liquid
weakness: a failed κ-solve reports "no qualifier assignment satisfies
the constraints", which is notoriously hard to localize — a real
regression against today's per-VC provenance unless heavily engineered.
**This is the type-system half of the SIBLING invariant-inference
quest**: the κ-placement + subtyping-constraint architecture is the
type-system axis (this doc); the qualifier engine + fixpoint solver is
inv-infer's. Interface note in §7.

**(d) Architecture / migration.** Largest. Needs a constraint-collection
phase distinct from both HM and the VC pass, a κ representation in
`Refinement.pred`, and a fixpoint solver that calls Lean/Z3 many times
per definition. The VC generator does *not* just notice — κ-solving is a
new global phase.

**(e) Warts fixed.** All of W1–W4, W6, *and* removes many annotations
(the inference payoff). W5 orthogonal.

**(f) Cost / risk.** High on every axis. Performance: the Houdini
fixpoint issues O(qualifiers × edges) solver calls per SCC — vox's
solver is Lean at ~1s/honest-module and ~6s/quantified-goal (AGENTS.md
cost model), which is **1000× slower per query than the SMT solvers
Liquid was designed around**. A qualifier-abstraction fixpoint over Lean
is likely infeasible at interactive speed without a fast SMT pre-filter
(Z3/CVC5 are on the box per the inv-infer probe). Error locality
regression is a product risk given vox's investment in per-VC
provenance and the editor. Soundness risk: κ-solving must be a *greatest*
fixpoint to stay sound (drop failing qualifiers), and the interaction
with dependent arrows and via-sorts is unstudied.

**(g) Composition.** via/refines: κ's over via-image sorts are possible
but multiply the qualifier space. Modes/borrowing: Flux shows κ-inference
*can* coexist with ownership, but that is a research undertaking.
Dependent arrows: κ's under binders need careful scoping (the same
stamp discipline gap-C fixed).

**Verdict:** the right *long-term* aspiration for the inference story,
but it should be built **on top of** Option 2's subtyping foundation and
**driven by** the inv-infer engine, not adopted wholesale now.

---

### Option 4 — Algebraic subtyping (MLsub / Dolan): principal types with subtyping

**(a) Idea.** Replace HM unification with **biunification** over polar
types, so the type system has subtyping *natively* and still enjoys
principal types. Refinement subtyping would be one more constraint in
the subtyping lattice.

**(b)–(f) Assessment (this option is analyzed to be rejected).**
MLsub's principality theorem requires the subtyping order to be a
**decidable, distributive lattice** with meets/joins computable
*structurally*, so biunification can close a constraint set into a
compact principal scheme. Refinement entailment `p ⇒ q` is (i) not
structural — it depends on arithmetic/inductive facts, not type shape —
and (ii) not decidable — it is delegated to Lean. Therefore the
biunification core **cannot close refinement constraints** without
calling the solver during inference, which reintroduces exactly the
error-locality and performance problems of Option 3 while *also*
requiring a from-scratch replacement of OCaml's unifier (a non-starter
for a fork that must track upstream). The refinement lattice's join/meet
would be `∨`/`∧` of predicates, which do not distribute nicely and grow
unboundedly.

**(g) Salvageable idea.** The **polarity** distinction (a type variable
used only in positive/produced position vs negative/consumed position)
is a genuinely useful lens and *does* transfer conceptually: it explains
*when* one-directional subsumption is safe (erase in positive position,
strengthen-requirement in negative), and it aligns with vox's `via`
(produces an image value — positive) vs `refines` (consumes at a sort —
negative) and the borrowing sibling's `now`/`fin` (the borrow's current
vs prophesied value). Use polarity as a **design check** on Option 2's
variance rules, not as an implementation.

**Verdict:** reject as an implementation strategy; keep polarity as an
analytical tool. Algebraic subtyping solves "principality under a
*decidable structural* subtyping order", which is not the order vox has.

---

## 5. Scorecard

Warts: W1 HO/under-ctor erasure, W2 covariant strengthening, W3
synthesis branch join, W4 arrow diagnostic/`p+p`, W5 self-capture, W6
via/refines ad-hoc. Rating: ✔ fixed, ◑ partial/opt-in, ✗ not addressed.

| | W1 | W2 | W3 | W4 | W5 | W6 | Infers refinements? | Principality | Error quality | Impl cost | Soundness risk | Perf risk |
|---|----|----|----|----|----|----|----|----|----|----|----|----|
| **0. status quo** | ✗ | ✗ | ✗ | ✗ | ✗ | ✗ | no | HM, clean | unification traces | — | — | — |
| **1. coercion-site** | ✔ | ✔ | ✗ | ✔ | ✗ | ◑ | no | HM, clean | entailment @ moregen | **low** | medium (touches unify arms) | low |
| **2. bidirectional (REC)** | ✔ | ✔ | ◑ | ✔ | ✔ | ✔ | no | HM skeletons, clean | **best** (expected known) | medium | **low** (off unify graph) | medium |
| **3. Liquid + κ** | ✔ | ✔ | ✔ | ✔ | ✗ | ✔ | **yes** | lost (qualifier-relative) | poor (κ-solve) | **high** | medium-high | **high** (Lean × fixpoint) |
| **4. algebraic subtyping** | ✔ | ✔ | ✔ | ✔ | ✗ | ✔ | partial | principal *if* decidable (it isn't) | poor | **very high** | high | high |

Reading the board: Option 2 dominates 0 and 1 on coverage while having
*lower* soundness risk than 1 (it removes refinements from the unify
graph instead of patching its arms), at a medium implementation cost. 3
and 4 buy inference/principality-with-subtyping that vox cannot cash at
Lean's query cost, and both regress error locality — the axis vox has
most heavily invested in (per-VC provenance, the editor).

---

## 6. Recommendation and first milestone

**Recommendation: Option 2 (bidirectional refinement layer over HM),
built as a staged migration, with Option 1 as the literal first stage so
value lands before the big reorganization.** Treat Option 3 as the
future inference story layered on top and owned jointly with inv-infer;
keep Option 4's polarity idea as a variance sanity-check only.

Why Option 2 over the cheaper Option 1: Option 1 leaves refinements on
the unification graph, so the structural hazards (the merge-rename, the
`p+p` artifact, the fragile cmi/binder pairing that gap-C had to fix)
persist and every future refinement feature keeps paying the
"bolted-onto-unification" tax. Option 2 pays down the principal by making
skeleton-unification and refinement-checking *separate concerns*, which
is also what makes via/refines, modes, and (eventually) inference compose
cleanly. The literature consensus (F\*, Flux, Liquid's checking core,
Dunfield-Krishnaswami) is squarely here.

### Staged migration (never breaks the working system)

- **Stage 0 — instrument.** Add a counter/log of every site where the
  `Trefine` equality guard *fires* across the full corpus (`make test`),
  to size the change and find programs that depend on equality-rejection.
- **Stage 1 (= Option 1) — directional relaxation, value lands.**
  Relax only `moregen`/`subtype`: add the covariant erase-under-ctor
  rule (W1) and the codomain-strengthening entailment VC (W2/W4). Do
  **not** touch symmetric `unify`. Corpus stays green (equality ⇒
  entailment); new HO/covariant programs compile. This is a
  self-contained, independently-valuable slice and the recommended
  **first milestone** (details below).
- **Stage 2 — strip predicates from `unify`.** Make symmetric `unify`
  skeleton-only; delete the merge-rename (`ctype.ml:5171`). This is the
  risky migration (failure-mode shift); guard it with Stage 0's data and
  the "eyeball every promoted rejection layer" discipline.
- **Stage 3 — explicit bidirectional judgments + scope fix (W5) +
  synthesis join choice (W3).** Reorganize the `vox_verify` walk into
  `check`/`synth`; move annotation-refinement elaboration to the outer
  env; add opt-in `∨`-join.
- **Stage 4 — rationalize via/refines (W6)** onto the one entailment
  rule.

### First milestone (concrete)

**Covariant erasure + arrow-codomain subtyping at directional edges**
(Stage 1). Deliverable:

1. In `typing/ctype.ml`, add to `moregen` (and mirror in `subtype`):
   `Trefine (u1, m1, p1)` vs a non-refined or differently-refined `t2`
   at a covariant position → compare skeletons and, when `t2` is refined
   with `p2`, register an entailment obligation `p1 ⊨ p2` instead of
   requiring `Refinement.equal`. Reuse `vox_flatten_view` as the
   normalizer.
2. A side-channel `ref` of pending obligations drained by
   `Vox_verify.check_structure`, feeding the existing `emit_vc` stream
   (so the VC generator "just notices" — no new VC plumbing).
3. Tests: promote `_probes/hof_plain.ml`, `arrow_covar.ml` from
   *rejected* to *accepted-with-VC*; add a negative
   (`int{_=x+2}` where `int{_>x}` expected fails the VC in Lean, not the
   type checker); confirm the full corpus stays green and every
   pre-existing rejection still fires at its intended layer.

Why this milestone: it fixes the two warts that reject *sound* programs
(W1, W2) and improves W4's diagnostic, it is confined to the directional
relations (lowest soundness risk — symmetric `unify` untouched), it
requires no reorganization, and it validates the "subsumption sites just
emit into the existing VC stream" thesis that the rest of Option 2 rests
on. If the branch-join probe had shown synthesis-join to be the top
user pain it would be the milestone instead; the probes show synthesis-
join (W3) is a *silent fact loss* (annoying) whereas W1/W2 are *hard
rejections of correct code* (blocking), so W1/W2 win.

---

## 7. Interface notes for sibling quests

- **invariant-inference (inv-infer).** Option 3 (Liquid κ-variables +
  qualifier inference) is the shared frontier. This doc owns the
  **type-system axis**: where κ-variables are placed (let-binders,
  params, results, joins), the subtyping-constraint form
  `Γ ⊢ t <: κ`, and the requirement that κ-solving be a *greatest*
  fixpoint for soundness. inv-infer owns the **engine**: the qualifier
  set, the Houdini fixpoint, and the fast-SMT-prefilter (Z3/CVC5 on the
  box) needed because Lean at ~1s/query cannot drive a fixpoint. The
  clean division: Option 2 must land *first* to give κ-inference a
  subtyping foundation to produce constraints against.

- **borrowing (now/fin).** Flux is the shared reference: refinement
  subtyping integrated with ownership. The polarity lens from Option 4
  (produced/positive vs consumed/negative) maps onto now/fin; the
  bidirectional pass of Option 2 should thread borrow state the way
  Flux threads ownership, so the two quests should agree on where the
  refinement judgment carries substructural state.

- **shared-mutation, exceptions, stdlib.** Orthogonal to the type-system
  axis; each will *consume* whatever subsumption discipline Option 2
  establishes (e.g. a refined array element flowing into a plain API is
  W1 at the element type). No design coupling now; they benefit
  automatically once erasure-under-constructor lands.

- **modes.** Modes ride the skeleton/arrow and are unaffected by
  refinement subtyping, but the *bidirectional pass* is the natural place
  to also thread mode expectations; coordinate the judgment's shape with
  whoever owns mode checking so refinements and modes share one
  check/synth traversal rather than two.

---

## 8. Appendix: key code references

- `typing/ctype.ml:5132` — `unify` Trefine arm (equality guard).
- `typing/ctype.ml:5171` — dependent-arrow binder merge-rename (delete
  in Stage 2; source of `p+p` W4).
- `typing/ctype.ml:6534` — `moregen` Trefine arm (Stage 1 target).
- `typing/ctype.ml:8009-8021` — `subtype` erase-to-skeleton (the one
  real subtyping rule today).
- `typing/ctype.ml:4018-4043` — `vox_flatten_view` / `vox_trefine_match`
  (the ad-hoc subtyping normalizer, W6).
- `typing/ctype.ml:3995-4003` — `vox_with_binder_pair` /
  `vox_arrow_has_binder` (alpha-equivalence for dependent binders).
- `typing/vox_verify.ml:1822` — `emit_vc` (the VC stream all options
  emit into).
- `typing/vox_verify.ml:2322` — `result_refinement` (nascent synthesis
  judgment; if-join demands equal preds today).
- `typing/vox_verify.ml:2600,2669` — intro-form and application-argument
  entailment VCs (subsumption that already works).
- `typing/typetexp.ml:1913-1973` — `T{p}` elaboration into `Trefine`.
- `typing/typecore.ml:3978-3993` — constrained-pattern `Vox_self` push
  (self-capture W5 site).
- `typing/subst.ml:830-880` — gap-C cmi binder freshening + Trefine path
  remap (the cautionary tale for touching binder identity).
- `typing/refinement.ml:110-185` — `with_binder_pair`, `equal_var`,
  `equal` (the syntactic predicate equality all options replace with
  entailment at flow edges).
- Repros: `_probes/{foerase,hof_plain,arrow_covar,pp_artifact,
  branch_same,branch_diff,branch_expected,selfcap,coerce}.ml`.
