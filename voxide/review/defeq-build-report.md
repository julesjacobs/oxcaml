# Definitional equations for total functions — build report

First cut of `let[@vox.def]`, per `design/definitional-equations-synthesis.md`.

- Worktree: `/usr/local/home/jujacobs/oxcamls/vox2/worktrees/defeq-build`
- Branch: `defeq-build`, base `ebedc4dec1` (branch `refine-types-emit`, the live compiler)
- Commit: `4453b5bc0795dc17ec118cd8d7026ed7caefaff8`
- Built binary: `/usr/local/home/jujacobs/oxcamls/vox2/worktrees/defeq-build/_install/bin/ocamlc.opt`

## What changed (5 files)

| File | Change |
|---|---|
| `typing/typecore.ml` | Part 1: drop `%divint`/`%modint` from `primitive_is_total` (1 line). |
| `typing/vox_defeq.ml` / `.mli` | New module: the `let[@vox.def]` → `f` + `f_def` structure expansion. |
| `typing/typemod.ml` | 1 line: `let sstr = Vox_defeq.expand_structure sstr` at the top of `type_structure`. |
| `typing/vox_verify.ml` | Skip verifying a generated-lemma body (still register it), recognised by expander-minted **ghost-location provenance** — never a forgeable attribute. |
| `dune` | List the new `vox_defeq` module. |
| tests | `refinement/defeq_totality.ml`, `refinement-lean/defeq_reflection.ml` (new); `refinement/printer_source_like.ml` (Part-1 flip). |

## Part 1 — integer `/` and `mod` are partial in the totality mode

`primitive_is_total` is the allowlist of primitives forced total when referenced
as a value (`type_ident`); anything not on it keeps the default partial mode
(this is exactly why `raise`, `List.hd`, `Array.get` are already rejected at
`@ total` — none of them is on the list). Integer `/` and `mod` were the one
leak: they type-check total yet trap on a zero divisor, and the logic models
total division (`x / 0 = 0`) which disagrees. Removing `%divint`/`%modint` from
the list makes them partial, so:

- a function using `/` or `mod` cannot be `@ total`, hence cannot be reflected;
- a refinement predicate (checked at `total`) using `/` or `mod` is rejected at
  type-check time.

`%divint`/`%modint` are **not** in `Vox_lean.primitive_builtin` (already opaque
to the solver), so this is purely a totality-mode tightening — it does not change
how div/mod are modelled, only that they are now excluded from `@ total`.

## Part 2 — `let[@vox.def] f x y = rhs`

`Vox_defeq.expand_structure` runs on every `Parsetree.structure` at the top of
`type_structure` (so batch, toplevel, and nested modules are all covered), before
type checking. For a `let[@vox.def] f p1 ... pn = rhs` item it emits two items:

1. `f`, with `[@vox.def]` stripped and `@ total` forced (via `pvb_modes`), so the
   totality mode itself rejects a partial or recursive body — and referencing `f`
   in its own equation (a `total` predicate) requires `f` total, which the forcing
   supplies.
2. a companion `let f_def p1 ... pn = (() : unit{ f p1 ... pn = rhs })`, tagged
   `[@vox.def.axiom]`, where `rhs` is **`f`'s own source body** and `p1 ... pn` are
   `f`'s own parameter patterns.

`f` stays an uninterpreted solver symbol; nothing about its body reaches the
solver except through `f_def`. Writing `let () = f_def a1 ... an` deposits the
ground equation `f a1 ... an = rhs[a1,...,an]` as a fact via the **existing**
`Vox_verify.check_application` path (`f_def` is registered as a dependent
definition, so its refined result is instantiated and its parameters substituted
by the actual arguments). No new predicate-citation surface, no Lean-emitter
change.

### Why the equation is faithful (and self-tying)

The predicate `f p1 ... pn = rhs` is elaborated with `f` already in scope at its
real (total) type. The application `f p1 ... pn` therefore pins each `pi` to `f`'s
parameter type and pins `rhs`'s type to `f`'s result type through unification. So
`rhs` — literally `f`'s source body, re-elaborated in the same scope where the
only newly-bound name is `f` itself (which a non-recursive body never mentions) —
denotes exactly `f`'s body, and any type divergence makes the equality ill-typed
and is rejected (fail-closed, never unsound).

### Trusted-lemma soundness argument

`f_def`'s refinement is a **trusted axiom**: the compiler *asserts*
`f p1 ... pn = rhs` because it generated `rhs` from `f`'s actual checked body —
the unit body `()` cannot prove it. So `Vox_verify` must not emit a verification
condition for the lemma body; `is_def_axiom_binding` recognises the generated
lemma (see **Provenance** below) and skips walking that body (while still running
`register_definition`, so the equation still reaches callers as a fact). The
trust anchor is faithfulness, which rests on three checked properties, all of
which fail closed:

- **Totality** — `f` is forced `@ total`, and `@ total` already excludes
  recursion and loops; a partial body is rejected at `f`'s definition.
- **Partial-operation freedom** — Part 1 makes `/` and `mod` partial, so a body
  using them cannot be `@ total` (join with the already-partial
  `raise`/`hd`/`get`); the equation is never emitted for such a body.
- **Lowerability** — a body outside the predicate fragment fails to elaborate as
  the predicate `f p1 ... pn = rhs`, so `[@vox.def]` is rejected rather than
  emitting a possibly-unsound lemma.

No behaviour changes for code that does not use `[@vox.def]`, other than Part 1's
div/mod totality tightening.

### Provenance — the verify-skip is unforgeable (round-2 fix)

The first cut keyed the verify-skip on a `[@vox.def.axiom]` **attribute**, which is
ordinary user-writable surface syntax — so a hand-written
`let bad x = (() : unit{ 0 = 1 }) [@@vox.def.axiom]` skipped verification and let
`0 = 1` (hence anything) through. That was a regression versus baseline and is
fixed:

- the expander mints a **fresh ghost `Location.t`** per generated lemma and
  records the physical object in `Vox_defeq.generated_lemma_locations`;
- `is_def_axiom_binding binding = Vox_defeq.is_generated_lemma_loc binding.vb_loc`
  tests membership by **physical identity** (`==`);
- no attribute is emitted or consulted at all.

A hand-written binding can never carry one of those heap objects, so it is always
verified normally. The channel is also fail-closed the other way: a false
negative (identity lost to a hypothetical copy) would only over-reject a genuine
lemma — never admit an unverified one. Change is localised to
`make_lemma_binding` (mint+record) and `is_def_axiom_binding` (consult).

### Scope / limitations (first cut)

- Non-recursive functions with ≥1 plain parameter; `function` syntax and
  labelled/optional/pattern parameters are rejected.
- Cross-module reflection is not wired (the lemma lives in its defining scope);
  same-module use is the first cut.
- The **spec surface `let () = f_def a b` works at module / top level** (verified
  below). The *inner-expression* form `let () = f_def a b in e` fails, because
  the expression-let path forces the RHS's refined type onto the `()` pattern and
  non-variable patterns do not yet forget refinements to their skeleton (a
  pre-existing gap; `unify_exp` already does the analogous forgetting for
  expressions and var patterns). Binding to `_`/a name works in every position.
  Not a soundness issue; a follow-up could extend the covariant forget to
  non-variable patterns.

## Validation evidence (all against the built `_install` compiler)

### (1) Part 1 — div/mod now rejected at `@ total`

`refinement/defeq_totality.ml` (green): `(/)` and `mod` rejected as `"partial"`;
`+`, `*`, `land` accepted as total; `raise`, `List.hd` rejected (unchanged);
predicates `int{ _ mod 2 = 0 }` and `int{ _ / 3 = 1 }` rejected at totality.

**Part-1-flipped tests (2):**
- `refinement/defeq_totality.ml` — new.
- `refinement/printer_source_like.ml` — its `type modulo = int{ _ mod 2 = 0 }`
  and `type divide = int{ _ / 3 = 1 }` cases, previously printer-coverage, now
  correctly error with `The value "\#mod"/"(/)" is "partial"`. Comment updated to
  record the tightening; `land`/`lsl` bitwise printer coverage retained.

No other test in any suite changed verdict.

### (2) Part 2 — unproved-while-opaque becomes provable after the equation

`refinement-lean/defeq_reflection.ml` (green), key phrases:

```
let[@vox.def] double x = x + x
  ⇒ val double : int -> int
    val double_def : int @ total -> unit{ double x = x + x }

let opaque_is_unproved = (double 5 : int{ _ = 10 })   ⇒ not-proved
let () = double_def 5
let after_def_is_proved = (double 5 : int{ _ = 10 })  ⇒ val ... : int{ _ = 10 } = 10
let () = double_def 5
let false_consequence  = (double 5 : int{ _ = 11 })   ⇒ disproved

let[@vox.def] add3 x y = x + y + 3
let () = add3_def 10 20
let add3_used = (add3 10 20 : int{ _ = 33 })          ⇒ val ... = 33
```

The same goal `double 5 = 10` is **not-proved** while `double` is opaque and
**proved** once `double_def 5` deposits `double 5 = 5 + 5`; a false consequence is
**disproved** (verification is genuinely running, not skipped).

### (3) Fail-closed

All in `defeq_reflection.ml`, each rejected with a clear message:
- `let[@vox.def] bad_div x = 100 / x` → `The value "(/)" is "partial" …`
- `let[@vox.def] bad_raise b = if b then raise Not_found else 0` → `raise` partial
- `let[@vox.def] rec bad_rec x = bad_rec x` → `[@vox.def] cannot be used on a recursive binding …`
- `let[@vox.def] not_a_function = 42` → `[@vox.def] requires a function binding with explicit parameters`

**Forgeability guard (round-2 negative tests, in `defeq_reflection.ml`):**
- `let[@vox.def.axiom] forged (x:int) = (() : unit{ 0 = 1 })` → **disproved**
  (hand-written attribute does not skip verification).
- The verifier's exact exploit
  `let bad (x:int) = (() : unit{ 0 = 1 }) [@@vox.def.axiom]  let () = bad 0  let exploit = (7 : int{ _ = 99 })`
  → **rejected at `let bad` (disproved)** — it never deposits `0 = 1` or reaches
  the `7 = 99` goal.

### (4) `test-one DIR=refinement` real counts + verdict-invariance

`make test-one DIR=refinement` → **24 passed / 0 failed**. Cross-checked no-rebuild:

| Suite | passed | failed |
|---|---|---|
| refinement | 24 | 0 |
| refinement-lean | 4 | 0 |
| typing-modes | 37 | 0 |
| refinement-acceptance | 14 | 0 |
| refinement-examples | 6 | 0 |

Only the two Part-1 flips changed; every non-`[@vox.def]` refinement test keeps
its verdict, and the totality-mode suite (`typing-modes`) is unaffected.
