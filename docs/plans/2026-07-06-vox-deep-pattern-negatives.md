# Deep variable-bearing match negatives — design options (task #38 part 2)

Status: DESIGN ONLY. No implementation. New logic primitives require the
user's ratification; this note lays out options so that gate has something
concrete to weigh.

## The gap

When control reaches a match arm, every earlier guard-free arm failed to
match. vox surfaces that as a negative fact today only for:

- a shallow single-constructor-over-variables arm → `not (s is C)`
  (`pattern_negation` / `head_negation`); and
- a fully GROUND arm → `not (s = <ground term>)` (task #22, commit 467ed2fdc).

A **deep pattern with variables in sub-positions** — the Okasaki
`Node (Red, Node (Red, a, x, b), y, c)` — contributes NOTHING. Its sound
negation must talk about the *refuting field position* (`s`'s left child is
not a red-red node), and the refinement logic has no constructor-field
selector (only `Pfield`/records, `Pproj`/tuples, `Pis`/top-constructor test).

This blocks the natural-form Okasaki `balance`. Note (task #38 part 1 finding,
commit d9421248e): deep constructor TERMS alone make only the FIRST rotation
arm verify (the one aligned with the model's first case). Arms 2–4 are
ENTANGLED with these negatives too, because the reflected model `balance` is
defined by *overlapping* deep cases: reducing `balance` on
`l = Node (Red, a, .., Node (Red, ..))` requires ruling out model case 1
(`a` is not a red-red node), i.e. exactly the earlier arm's deep negative.

## Soundness frame (common to all options)

Matching a linear (no or-pattern), guard-free pattern `p` against `s`:

```
matches(s, C(p0..pn)) = (s is C) ∧ ⋀ᵢ matches(fieldᵢ(s), pᵢ)
matches(s, k)         = (s = k)              -- literal
matches(s, x) / (s,_) = True                 -- variable / wildcard
```

The earlier-arm negative is `¬ matches(s, p)`. This is sound regardless of how
`fieldᵢ` is realised because it is exactly the negation of the pattern's
success condition; the only design question is how to *express* `fieldᵢ` (or
avoid it) in a form grind can use. Guarded arms and or-patterns still
contribute nothing (unchanged).

## Option 1 — scoped existential naming (NO new primitive)

Express `matches` with existentials over the field positions, reusing the
EXISTING `Pquant (Exists, _, _)`, `Pconstr`, `Pand`, `Pbinop (Eq,..)`:

```
matches(s, C(p0..pn)) = ∃ f0..fn, s = C(f0,..,fn) ∧ ⋀ᵢ matches(fᵢ, pᵢ)
```

with a fresh `Scoped` binder `fᵢ` per field; variable/wildcard sub-patterns
add no conjunct (the field is unconstrained), literals add `fᵢ = k`, nested
constructors recurse. The negative fact is `Pnot (matches …)`.

For Okasaki case 1:
`¬ ∃ f0 f1 f2 f3 g0 g1 g2 g3, l = Node (f0,f1,f2,f3) ∧ f0 = Red
   ∧ f1 = Node (g0,g1,g2,g3) ∧ g0 = Red`.

- **Lean encoding**: none new. `Pquant` already emits a genuine binder;
  `Pis` already relies on the existential encoding, so the machinery is
  exercised. Internally-minted nested `Pquant` needs confirming through the
  encoder (unverified).
- **grind-instantiability**: THE open question. A `¬∃` is a `∀¬` fact; the
  AGENTS.md caveat is that grind cannot instantiate forall-facts at goal
  indices. BUT here the witnesses are concrete: when grind `split`s the
  reflected model's decision tree, the branch that needs refuting has `l`
  already destructured (e.g. `l = Node (Red, Node (Red, b', y', c'), ..)`),
  so the existential's witnesses are present in the branch context and
  e-matching should fire. This is materially more hopeful than the
  loop-invariant forall case (which had no witnesses). UNPROVEN — must probe.
- **soundness / non-matching constructor**: trivial — pure negation of the
  match predicate, no partial functions, no default values.
- **ratification weight**: LIGHTEST. Uses only existing primitives; the new
  thing is an internal VC-minted fact shape, like `Pis` already is. Arguably
  no new user-facing language surface.

## Option 2 — per-constructor-per-field selector primitive

New `Psel (path, C, i, t)` = "field `i` of `t` viewed as `C`". Negative =
`¬(s is C) ∨ ⋁ᵢ ¬ matches(Psel(path,C,i,s), pᵢ)` (drop the var/wildcard
disjuncts).

- **Lean encoding**: per selector,
  `def sel_T_C_i (t : T) : Fᵢ := match t with | C x0..xn => xᵢ | _ => dflt`.
  The `_ => dflt` needs a value of `Fᵢ` — an `Inhabited Fᵢ` instance (Lean
  derives it for most inductives, but NOT all: e.g. an empty type, or a type
  whose Inhabited needs work). That derivation is the sharp edge.
- **grind-instantiability**: BEST. `sel_T_C_i (C x⃗) = xᵢ` is a ground
  rewrite; grind consumes it directly, no quantifier.
- **soundness / non-matching constructor**: the `dflt` value is unconstrained
  but every use of `Psel(_,C,_,s)` sits under the guarding `¬(s is C)`
  disjunct, so when `s` is not `C` the disjunct is already true and the
  selector's value is irrelevant. Sound.
- **ratification weight**: HEAVY. New pred constructor + printer + `equal` +
  `subst_*` + `free_vars`/`map_paths`/`constr_paths` + `register_pred_paths`
  + Lean codegen (emit + Inhabited derivation + reduction lemmas).

## Option 3 — match-to-Prop decision predicate

New `Pmatches (subject, shallow-pattern-encoding)` emitted as a single Lean
`match` returning `Prop`:
`(match s with | C (C' _ _ _) _ _ => True | _ => False)`, negated.

- **Lean encoding**: a new primitive carrying enough of the pattern to emit
  the match; total (fallthrough `_ => False`), so no default-value problem.
- **grind-instantiability**: GOOD — `split` on the emitted match gives the
  structural branches directly (this is exactly how the reflected model
  `balance` is itself reduced), so it composes with the model split.
- **soundness / non-matching constructor**: total, returns `False`; clean.
- **ratification weight**: MEDIUM. One new primitive, but it carries a pattern
  shape (needs a small pattern IR in `pred`), which is a bigger surface than
  `Psel` conceptually even if the Lean side is cleaner.

## Recommendation

Probe **Option 1 first** — it needs no ratified primitive, so it can be tried
and measured immediately; the entire risk is the one empirical question
(does grind instantiate the split-local existential witnesses?). If Option 1
closes the Okasaki overlapping-model cases, we are done with no language
addition. If grind will not instantiate, fall to **Option 3** (total, splits
like the model) over Option 2 (avoids the `Inhabited` default hazard).

## No free subset from part 1

Checked the lead's hint: part 1's nested terms do NOT make any
variable-bearing negative expressible without one of the above. A pattern
whose spine is determined but whose leaves are variables (e.g.
`Node (Red, Node (Red, _, _, _), _, _)`) still needs to say "field is not
Red" at a sub-position, which needs field access (Option 2) or the
existential (Option 1); it is not ground, so the task-#22 ground path does not
fire. Nothing to add to part 1.

## OUTCOME (implemented, task #38 part 2)

Option 1 was probed and built. The empirical question resolved with a twist:

- **grind will NOT instantiate the existential negative under a plain
  `by grind` goal.** Every shape was tried against the real solver on the
  natural-Okasaki arm-2 VC — whole-tuple `¬∃`, component `¬∃`, component
  `∀`-NNF disequality, and even the ideal negative stated directly on the
  split sub-position. grind derives the conditional match-reduction fact but
  does not discharge its `∀`-antecedent. `grind (splits:=100) (ematch:=100)`,
  `+splitImp`, `+splitIndPred` do not help.
- **The negatives ARE sufficient once the spec function's match is split.**
  The tactic `unfold <spec fn>; split <;> grind` (uniform, no variable names)
  closes all four rotation arms and the catch-all. So Option 1 works, but a VC
  that carries a deep existential negative must be emitted with the proof
  fallback `by first | grind | (unfold <goal spec fns>; split <;> grind)`
  instead of `by grind`. `first | grind | …` keeps every already-passing VC
  byte-identical; the fallback fires only on grind failure, and `split <;>
  grind` is sound, so no false goal becomes provable.

No new logic primitive was needed (existing `Pquant`/`Pconstr`/`Pand`/`Pbinop`;
the Lean encoder already renders `Pquant` as `(∃ v, …)`). The only additions
are `pattern_negation`'s existential fallback (single-constructor + tuple) and
the per-VC split fallback in `lean_theorem`.

**Soundness knife-edge:** the tuple negative must be the negation of the FULL
conjunction over pinned components, INCLUDING every nullary-constructor pin
(`c = Black`). Dropping a pin over-strengthens the negation and is unsound
(the catch-all has `c` free). Guarded by `mechanics/lean_patneg_fail.ml`.

Gate met: `lib/rbt.ml` now uses the natural four-rotation `balance` (dropped
the explicit-colour scrutinees and the `balance_r` helper) and verifies
against the sealed `lib/rbt.mli`. Tests: `mechanics/lean_patneg.ml`
(single-scrutinee positive), `mechanics/lean_patneg_fail.ml` (soundness),
`mechanics/lean_rbt.ml` (the natural gate, via the updated `lib/rbt.ml`).
