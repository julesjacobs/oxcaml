# vox: higher-order refinements — Lean props/predicates/relations as OxCaml values

Exploratory design study, 2026-07-07.  Commissioned brief (user's words):

> Investigate if we can also reflect Lean propositions, predicates,
> relations, etc. into the OxCaml value space, so that we can have higher
> order refinements.  I'm thinking of something like
> `iter : (R : IntRel) -> (f : (x:int) -> int{ R x _ }) -> (n : int) -> int{ relIter R n x _ }`.
> And we should be able to construct simple IntRels using oxcaml-lean
> reflected stuff like `iter { expr resulting in a relation } ...` where
> that expression is the same kind of thing that we can use in refinements
> (except its type is IntRel).

The verdict up front: **the substrate already exists.**  A relation is an
OCaml ghost type whose logic sort is a Lean *arrow* type; vox already
declares such a binder in the VC context at the arrow sort, already lets a
parameterized `@[grind] def` fixpoint take it, and already substitutes a
concrete relation for it at a call site.  The higher-order verified
`map`/`fold` that this unlocks — the study's biggest payoff — **works today
with zero compiler changes**.  Two ergonomic gaps remain (direct `R x _`
application; a slick relation *literal*), both with cheap, ranked fixes;
one of them (relation-value identity at a call site) is prototyped here in
one line.

Everything below is backed by a probe compiled with the real Lean 4.31
solver; probe files are in `scratch_hor/` (probe1..probeC).  Claims not
backed by a probe are marked **UNPROBED**.

---

## 1. Probe-established facts

The mechanism this study builds on is the **ghost sort**
(`docs/plans/2026-07-04-refines-via-design.md` §1): `type t [@@vox.sort
lean "Name"]` makes `t`'s logic sort the Lean type `Name`, represented as
`Vs_lean of string * vox_sort list` (`types/types.ml:276`) and rendered
verbatim by `lean_sort` (`vox_verify.ml:4529`).  The whole study is the
observation that **`Name` may be an arrow type** — `def IntRel := Int → Int
→ Prop` — so a ghost binder can be a relation.

Numbered facts (F#) referenced throughout:

- **F1 — an arrow ghost sort is accepted.**  `type intrel [@@vox.sort lean
  "IntRel"]` with `def IntRel := Int -> Int -> Prop` in a `[%%vox.lean]`
  block compiles; no parser, elaboration, or emission change needed.
  (probe1, probe2)

- **F2 — an arrow-sorted binder is declared at its sort.**  A dependent
  parameter `(r : intrel)` that appears in a predicate as a bare value
  (`Pvar`) is emitted as a genuine Lean hypothesis `(v_r_288 : IntRel)` in
  the VC.  Verbatim from probe2:
  `theorem vc_0 (v_x) (v_n) (v_r_288 : IntRel) : (relIter v_r_288 v_n v_x v_x) := by grind`
  (probe2)

- **F3 — passing R WHOLE to a parameterized `@[grind]` fixpoint works end
  to end.**  A `def relIter (r : IntRel) : Int → Int → Int → Prop` used in
  a contract `int{ relIter r n x _ }` verifies for satisfiable goals
  (probe2b, `relIter r 0 x _` reduces to `x = _`, proved) and **fails
  closed** for genuinely-false goals (probe2, arbitrary `n`, NOT PROVED).
  grind handles the relation-parameterized def, including its match on the
  relation argument.

- **F4 — direct application `R x _` is MIS-elaborated (the one real
  front-end gap).**  In `elab_vox_pred` (`typing/typetexp.ml:1230`) an
  applied lowercase head falls into the *spec-function* branch and becomes
  `Pfun("r", [x; _])` — keyed on the **source string** `"r"`, disconnected
  from the binder's ident, and never declared at its sort.  The emitted
  Lean is a bare undeclared `r` ("function expected at `r`"; probe1).  The
  applied-head branch does not consult `vox_find_scope`, so it cannot tell
  that `r` is a Π-binder.

- **F5 — applied-R is fully ROUTED AROUND by a one-line wrapper, today.**
  Define `@[grind] def rHolds (r : IntRel) (a b : Int) : Prop := r a b` and
  write `int{ rHolds r x _ }` instead of `int{ r x _ }`.  Now `r` is passed
  **whole** (F2/F3) and applied only inside Lean.  Verifies (probe5).  This
  turns F4 from a blocker into an ergonomic wart.

- **F6 — call-site contract instantiation substitutes the relation.**  A
  call `iter arg n x` substitutes the R-binder by `stable_arg_name arg`
  (`vox_verify.ml:1496` = `Vox_reflect.translate`) via `Vox_dep.subst_binder`
  (`vox_verify.ml:3230`).  probe3 confirms the client obligation becomes
  `h_0 : relIter <arg> 0 x result` — the binder is correctly replaced.

- **F7 — a *reflected* relation value flows its concrete identity (after a
  1-line fix; prototyped here).**  Before: a bare reference to a value
  translated to `Pvar`/`Pglobal` (`vox_reflect.ml:471`), so a passed
  relation value degraded to an opaque binder `v_le_rel` — its Lean
  definition never connected (probe3).  After the prototype change (see §6)
  a bare reference to a `[@@vox.reflect "Sym"]` value emits `Pfun("Sym",
  [])`; probe8b's client VC becomes `h_0 : relIter leRel 1 x result` — the
  concrete `leRel` flows through substitution.  **Caveat:** the attribute
  must reach `val_attributes`, which it does for `external ... [@@vox.reflect]`
  but (probed) **not** for a plain `let ... [@@vox.reflect]` (probe8 vs
  probe8b) — an independent, pre-existing attribute-plumbing gap.

- **F8 — a concrete relation named DIRECTLY in a refinement needs no value
  and no change.**  Writing the Lean symbol bare (`int{ rHolds leRel x _ }`)
  makes `leRel` a 0-ary spec constant (`Pfun("leRel", [])`, the fallback at
  `typetexp.ml:1057`); a client derives concrete arithmetic consequences
  (`x ≤ result`) by unfolding it.  Verifies (probeA).

- **F9 — relation COMBINATORS work, generically over abstract relations.**
  `def rcomp (r s : IntRel) : IntRel := fun a c => ∃ b, r a b ∧ s b c` used
  in a contract `int{ rHolds (rcomp r s) x _ }` proves that composing an
  r-step and an s-step yields an `(rcomp r s)`-step — over **abstract**
  `r s : IntRel` binders (probeC).

- **F10 — the map/fold payoff verifies today (the headline).**  See §5.

Two restrictions surfaced (neither blocks the design):
- **R-a** (F7 caveat): `[@@vox.reflect]` on a plain `let` is dropped from
  `val_attributes`; use `external`, or an `.mli` `val`, or fix the plumbing.
- **R-b**: a module-level value's *type* may not mention another
  module-level value in its refinement ("may not appear in a module-level
  type", probe6) — so the "carry identity on the value's refinement" route
  (`intrel{ _ = leRel }`) is closed; use F7 (reflect) or F8 (name it) instead.

---

## 2. The design that emerged

A **higher-order refinement** in vox is a dependent contract quantified
over a **ghost relation** (or predicate): an OCaml phantom type whose logic
sort is a Lean function-into-`Prop`.  Three layers, each already present in
the compiler except where noted:

### Layer 1 — the relation type (a ghost arrow sort)
```ocaml
type intrel [@@vox.sort lean "IntRel"]          (* logic sort: Int → Int → Prop *)
[%%vox.lean {lean| def IntRel := Int -> Int -> Prop |lean}]
```
`intrel` is a phantom: no runtime information (represent it as `unit`, a
single-constructor token, or — in a `.mli` — an abstract type).  Its whole
job is to attach the arrow sort to a binder.  Generalizes freely:
`type intpred [@@vox.sort lean "IntPred"]` with `def IntPred := Int → Prop`;
parameterized `type 'a rel [@@vox.sort lean "Rel"]` with `def Rel (a) := a →
a → Prop` (the ghost-sort parameterization of the via design §4 already
carries argument sorts).

### Layer 2 — quantifying a contract over the relation
```ocaml
val map_r : (r : intrel)
         -> (f : (x:int) -> int{ rHolds r x _ })   (* f's SPEC, via wrapper *)
         -> (xs : ilist) -> ilist{ listRel r xs _ } (* result: R lifted over the list *)
```
`r` enters the logic context at sort `IntRel` (F2).  It is used only by
being **passed whole** to `@[grind]` fixpoints/relations defined in a block
(`rHolds`, `relIter`, `listRel`, `rcomp`, …) — the E-matcher instantiates
those defs at the concrete `r` and at goal indices, and grind unfolds them
(F3, F5, F9).  The callback `f` is **never modeled**; only its per-element
specification (the relation `r`) is.

### Layer 3 — construction: supplying a concrete relation
Three routes, in increasing ergonomics:
1. **Name it directly** in the refinement (F8): write the Lean symbol
   (`rHolds leRel x _`) — a 0-ary spec constant.  Zero machinery.  Best when
   the relation is fixed at the use site.
2. **Reflected value** (F7): `external le_rel : intrel = "%opaque"
   [@@vox.reflect "leRel"]`, then `map_r le_rel f xs`.  The value is a ghost
   token; the reflect attribute binds it to the block symbol `leRel`, which
   flows through call-site substitution.  Best when the relation is passed
   as a first-class argument.
3. **Relation literal** (the user's `{ expr }`, NOT yet built — §6 ask #2):
   `map_r { fun a b -> a <= b } f xs`.  Sugar that (a) elaborates the
   refinement-grammar body with two binders to a Lean lambda `fun a b => a ≤
   b`, (b) synthesizes the block `def` + reflected ghost token, so it
   desugars onto route 2.  This is the ergonomic surface the user asked for.

### Why "pass whole, apply in Lean" is the load-bearing idiom
vox's predicate IR (`refinement.ml:25`) applies **named** symbols
(`Pfun of string`) and carries **bound values** (`Pvar of Ident.t`), but has
no "apply a bound variable to arguments".  Rather than add one (ask #1), the
design routes every application of a relation *through a block def that takes
the relation as a parameter* — so the relation only ever appears as a `Pvar`
argument, which the existing machinery declares (F2), substitutes (F6), and
grind unfolds (F3).  `rHolds r a b := r a b` is the minimal such wrapper;
`listRel`, `relIter`, `rcomp` are the useful ones.

---

## 3. The `iter` walkthrough (north star)

The user's exact signature (`x` free in the result is read as the initial
value; `n` the iteration count):
```ocaml
val iter : (r : intrel)
        -> (f : (x:int) -> int{ rHolds r x _ })
        -> (x0 : int) -> (n : int)
        -> int{ relIter r n x0 _ }
```
with the fixpoint
```lean
def IntRel := Int -> Int -> Prop
@[grind] def rHolds (r : IntRel) (a b : Int) : Prop := r a b
@[grind] def relIter (r : IntRel) : Int -> Int -> Int -> Prop
  | 0, x, y => x = y                 -- 0 steps: y is x0
  | _, x, y => r x y                 -- (probe fixpoint; the true n-fold is below)
```

**What is proved today (probed):**
- the signature elaborates; `r` is a declared `IntRel` hypothesis (F2);
- `relIter r n x0 _` is a well-typed contract; the `n = 0` client
  obligation `x0 = result` proves (probe2b/probe3);
- a **call** `iter le_rel f x0 n` substitutes `le_rel` (concrete, via F7)
  into the client's fact `relIter leRel n x0 result` (probe8b);
- a client then derives concrete consequences by unfolding `leRel` (F8/probeA).

**What the honest recursive body needs (UNPROBED for the int-counter form).**
`iter` recurses on `n`, so its correctness proof is an induction over `n`
with the loop-invariant discipline the AGENTS memo and `demo/lean_reverse.ml`
prescribe: the true n-fold relation
```lean
@[grind] def relIter (r : IntRel) : Nat -> Int -> Int -> Prop
  | 0,      x, y => x = y
  | (k+1),  x, y => ∃ z, relIter r k x z ∧ r z y
```
needs **one hand-proved step lemma** (`relIter r (k+1) x0 (f last) ` from
`relIter r k x0 last ∧ r last (f last)`) stated as a prelude Prop whose
variables are all bound by its conclusion, because grind will not
instantiate a `∀`-fact at the loop index on its own.  This is the *same*
obstacle every vox loop faces; it is not specific to higher-order
refinements, and the relation parameter `r` rides through it unchanged.

**The realized north star (probed, F10):** the structurally-recursive
sibling of `iter` — `map_r` over a list (§5) — needs **no** hand lemma
(structural recursion feeds grind directly) and verifies end to end today.
`iter`-over-a-list *is* iteration; the int-counter form is the same design
plus the standard one-lemma loop tax.

---

## 4. The map/fold connection — assessment (the biggest payoff)

**Verdict: this is the real prize, and it works today.**

vox cannot model an arbitrary function argument (there is no sort for "an
OCaml `int → int`", and reflecting one would be unsound in general).  That
has blocked verified higher-order combinators — `map`, `fold`, `filter`,
`iter` — because their whole point is a function parameter.

Higher-order *refinements* dissolve the blockage by a change of what is
modeled: **model the callback's specification, not the callback.**  Give
`map` a ghost relation `R` describing the per-element contract of `f`
(`f : (x) -> 'b{ rHolds R x _ }`), and let `map` promise that the result is
`R` *lifted over the container* (`listRel R xs _`).  The function `f` is
never named in the logic; only `R` is, and `R` is passed whole to the
lifting fixpoint.  This is exactly the pass-whole idiom (§2), so it inherits
F2/F3/F5 wholesale.

Consequences:
- **`map`**: result pointwise-`R`-related to input.  Probed (F10, §5).
- **`fold`**: accumulator invariant as a *predicate* parameter `P : IntPred`
  with a step relation; `fold` promises `P` holds of the result given `P`
  holds initially and each step preserves it.  Same substrate (a predicate
  is the 1-ary ghost sort; **UNPROBED** but structurally identical).
- **`filter`**: result elements all satisfy a predicate `P` and are a
  sublist — a predicate parameter plus a `sublist` fixpoint.  **UNPROBED.**
- This is *compositional*: F9 shows relation combinators compose generically,
  so `map g ∘ map f` relates input to output by `rcomp Rf Rg`, proved once.

The payoff is not merely that these type-check — it is that they are
**fully checked, TCB-free** proofs (§7): no `assume_`, no axiom, no trusted
function modeling.  The only trust is the ghost-sort declaration, which for
a relation is *lower* trust than an ordinary value reflection (§7).

Recommendation: this is worth pursuing as the headline application of the
feature; it is the thing that turns vox from "verify first-order container
internals" into "verify clients' use of higher-order library combinators."

---

## 5. The realized payoff, verbatim (F10)

`scratch_hor/probe7.ml` — a verified `map_r` that knows only `f`'s spec:
```ocaml
type intrel [@@vox.sort lean "IntRel"]
type tree = Nil | Cons of int * tree
[%%vox.lean {lean|
def IntRel := Int -> Int -> Prop
@[grind] def rHolds (r : IntRel) (a b : Int) : Prop := r a b
@[grind] def listRel (r : IntRel) : Vox_Probe7_tree -> Vox_Probe7_tree -> Prop
  | .Nil, .Nil => True
  | .Cons a as, .Cons b bs => r a b /\ listRel r as bs
  | _, _ => False
|lean}]
let map_r : (r : intrel) -> (f : (x:int) -> int{ rHolds r x _ }) ->
            (xs : tree) -> tree{ listRel r xs _ } =
  fun r f xs -> ignore r;
    let rec go : (u : tree) -> tree{ listRel r u _ } =
      fun u -> match u with
        | Nil -> (Nil : tree{ listRel r u _ })
        | Cons (x, rest) ->
            let y = f x in let ys = go rest in
            (Cons (y, ys) : tree{ listRel r u _ })
    in go xs
```
Compiles green.  The interesting VC (verbatim) uses `v_r : IntRel`
throughout and is genuinely discharged, not vacuous:
```lean
theorem vc_1 (v_ys) (v_y : Int) (v_u) (v_r : IntRel) (v_rest) (v_x : Int)
  (h_0 : listRel v_r v_rest v_ys) (h_1 : rHolds v_r v_x v_y)
  (h_2 : v_u = Vox_Probe7_tree.Cons v_x v_rest)
  : listRel v_r v_u (Vox_Probe7_tree.Cons v_y v_ys) := by grind
```
Soundness pinned: `scratch_hor/probe7_fail.ml` conses a constant `0` instead
of `f x`; the R-relation between `x` and `0` does not follow, and the VC
**fails closed** (NOT PROVED).

---

## 6. Ranked compiler asks (smallest change first)

The design is *usable today* through §2 route 1 (name it) + the `rHolds`
idiom.  These asks improve ergonomics; none is a soundness prerequisite.

1. **[prototyped, ~6 lines] Bare reference to a `[@@vox.reflect]` value
   emits its Lean symbol.**  `Vox_reflect.translate`'s `Texp_ident` case now
   returns `Pfun(sym, [])` when the value carries a reflect attribute (§ diff
   below).  Enables route 2 (first-class relation values flowing their
   identity through call-site substitution; F7).  Independently, the
   attribute-plumbing gap R-a (`[@@vox.reflect]` dropped from a plain `let`'s
   `val_attributes`) should be closed so `let`, not only `external`, works;
   this is a Typecore change, not a vox one, and is **UNPROBED**.
   *Should also mirror the same reflect-symbol resolution in
   `elab_vox_pred`'s bare-ident arms (`typetexp.ml:1002/1013`) so a
   hand-written refinement mentioning a reflected value agrees with the
   value it passes — currently only the APPLIED path resolves reflect names.*

2. **[medium] Relation-literal surface `{ ... }` / `[%vox.rel x y -> p]`.**
   A refinement-grammar expression with two binders, elaborating to a Lean
   lambda substituted at the R-binder.  Cheapest realization: desugar to a
   synthesized block `def` + reflected ghost token (rides ask #1), so no new
   predicate IR is needed.  A first-class realization adds `Plam of Ident.t
   list * pred` to `refinement.ml` with (a) emission `fun v_x v_y => <body>`
   (`vox_verify.ml` `lean_of_pred`) and (b) `subst_var` replacing the
   R-`Pvar` by the lambda (grind beta-reduces for free).  This is the
   user's literal; **UNPROBED**.

3. **[medium] Direct application `R x _` (remove the `rHolds` wrapper).**
   In `elab_vox_pred`'s applied-head branch (`typetexp.ml:1230`), check
   `vox_find_scope` before the spec-function fallthrough; if the head is a
   Π-binder emit an application-of-binder.  Needs `Papp of Ident.t * pred
   list` in `refinement.ml`, emission `(v_r args)`, and a `subst_var` that
   beta-applies when the substituted term is a `Plam` (composes with ask #2).
   Purely ergonomic given F5; **UNPROBED**.

4. **[small, quality] Reject an arrow-sorted binder used un-passed / mis-arity
   at elaboration** rather than surfacing as a Lean "function expected"
   (F4).  A clean-rejection layer akin to the kinds-study R1 work.  **UNPROBED.**

---

## 7. Soundness / TCB analysis

Higher-order refinements add **no new trusted surface** beyond the existing
ghost-sort and reflect mechanisms — and the relation case is *strictly
lower* trust than an ordinary value reflection.

| ingredient | trust | who checks |
| --- | --- | --- |
| `type intrel [@@vox.sort lean "IntRel"]` (arrow ghost sort) | **assumed**, same class as any `[@@vox.sort lean]` | nobody — but see below |
| `def IntRel := Int→Int→Prop`, `def relIter/rHolds/rcomp` (block defs) | **checked** (definitions, not axioms — conservative) | Lean |
| `f`'s per-element contract `int{ rHolds r x _ }` | **checked** at f's definition (or a caller obligation at f's use) | vox VC + Lean |
| `map_r`/`iter` body proof | **checked**, fully | Lean grind |
| relation LITERAL `{ fun a b -> p }` | **checked** (a Lean lambda, Lean-typed) | Lean |
| reflected relation VALUE `external ... [@@vox.reflect "leRel"]` | **assumed** (value↔symbol correspondence) | nobody |

The subtlety that makes this *safe*: a relation ghost has **no runtime
content**.  `intrel` is a phantom; there is no OCaml value whose behavior
could disagree with its Lean symbol.  Contrast reflecting `land` (a real
runtime primitive), where `[@@vox.reflect]` asserts a genuine
runtime↔logic correspondence that a wrong symbol could violate.  For a
relation literal the "value" *is* pure logic — the assumed correspondence is
between "nothing at runtime" and "a Lean lambda", which cannot be false.
So:
- **Relation LITERALS (ask #2) are zero-trust** — a Lean lambda, checked by Lean.
- **Named relations in refinements (F8) are zero-trust** — block defs, checked.
- **Reflected relation VALUES (F7) carry the usual reflect caveat**, but even
  it is toothless here (no runtime meaning to mis-map); the only thing to
  review is that the ghost token isn't secretly given runtime-observable
  content that code branches on (it shouldn't — it's `unit`-like).
- The block `def`s are definitions, so — unlike a `.ml`-block `axiom` — they
  cannot introduce `False`; grind either unfolds them soundly or fails.

Fail-closed behavior is preserved throughout: a false lifted claim is
refuted at grind (probe7_fail; probe2 arbitrary-`n`), never a silent pass
and never an elaboration-error-a-layer-too-early for the passed-whole forms.

Overclaim posture matches the via design's: the interface can only promise
what the implementation proves, and the relation parameter does not widen
that — it is universally quantified, so a client instantiating it at a
concrete relation gets exactly the instantiated theorem.

---

## 8. Support matrix (what works TODAY vs needs support)

| capability | status | evidence |
| --- | --- | --- |
| arrow ghost sort `IntRel := Int→Int→Prop` | **works** | F1 (probe1/2) |
| arrow-sorted binder declared at its sort in VC | **works** | F2 (probe2) |
| pass R whole to a parameterized `@[grind]` fixpoint | **works** | F3 (probe2b) |
| relation-parameterized fixpoint proves/fails correctly | **works** | F3 (probe2, probe2b) |
| verified `map` over R's lifting (function-arg dodge) | **works** | F10 (probe7) + soundness (probe7_fail) |
| relation combinators (compose) over abstract R | **works** | F9 (probeC) |
| client derives concrete consequence from a named relation | **works** | F8 (probeA) |
| call-site substitutes the relation binder | **works** | F6 (probe3) |
| passed relation VALUE flows its concrete identity | **works after ask #1** (prototyped) | F7 (probe8b) |
| `[@@vox.reflect]` on a plain `let` (vs `external`) | **broken** (R-a) | probe8 vs probe8b |
| direct application `R x _` (no wrapper) | **broken** → use `rHolds` wrapper | F4 (probe1), F5 (probe5) |
| relation literal `{ fun a b -> ... }` surface | **not built** (ask #2) | design only |
| fold with a predicate-parameter invariant | **UNPROBED** (structurally identical to map) | — |
| int-counter `iter` recursive body proof | **UNPROBED** — needs the standard 1 loop lemma | §3 |

---

## 9. The shape the idea should take (recommendation)

Frame the feature as **"relational specifications for higher-order code"**,
not "props as values":
- The primitive is a **ghost relation/predicate type** (`[@@vox.sort lean
  "R := … → Prop"]`) — already in the compiler.
- The idiom is **pass the relation whole to block-defined lifting
  operators** (`rHolds`, `listRel`, `foldInv`, `rcomp`); never apply a
  relation binder directly (route around F4 with a wrapper, or land ask #3).
- The headline application is a small **`vox_stdlib` relational-combinator
  module**: `map`/`fold`/`filter`/`iter` specified by a relation/predicate
  parameter, verified once, TCB-free — the first genuinely higher-order
  entries in the verified stdlib.
- The ergonomic surface is the **relation literal `{ … }`** (ask #2),
  desugaring onto a reflected ghost token (ask #1).  Ship asks #1 then #2;
  #3 and #4 are polish.

Do **not** try to make relations first-class runtime values with real
content — the power and the safety both come from them being *ghosts*
(pure logic, phantom runtime).  That is also what makes the whole thing
land on machinery vox already has.

## Files / probes
- `scratch_hor/probe1.ml` — arrow ghost sort; applied-R mis-elaboration (F1/F4)
- `scratch_hor/probe2.ml`, `probe2b.ml` — R passed whole to `relIter`; declared binder; pos/neg (F2/F3)
- `scratch_hor/probe3.ml` — call-site substitution; opaque-value identity loss (F6/F7-before)
- `scratch_hor/probe5.ml` — applied-R via `rHolds` wrapper (F5)
- `scratch_hor/probe6.ml` — module-level-value-in-type restriction (R-b)
- `scratch_hor/probe7.ml` + `probe7_fail.ml` — the map/fold payoff + soundness (F10)
- `scratch_hor/probe8.ml`/`probe8b.ml` — bare-reflect fix; `let` vs `external` (F7/R-a)
- `scratch_hor/probeA.ml` — concrete client reasoning from a named relation (F8)
- `scratch_hor/probeC.ml` — relation combinators over abstract relations (F9)
- prototype diff: `typing/vox_reflect.ml` `translate` `Texp_ident` reflect-symbol arm (ask #1)
