# vox: nested refined expressions without let-binding (C1)

Direct support for nested refined expressions in refined/dependent argument
positions, so `f (g x)` verifies where today one must write
`let t = g x in f t`. The feature synthesizes a logical name for the
subexpression's value (logical ANF): no term rewriting, no runtime change; the
argument still evaluates once at its position, and we name that single value the
same way the manual let-binder does.

## Phase 1 — the boundary that actually fails today (measured on tip 09f06eb7b)

Probes compiled directly with `_install/bin/ocamlc.opt -vox-solver-path <lean>
-c` (about 1 s each). Helper contracts:
`g : (x:int) -> int{ _ <= x }` (inequality result, NOT nameable),
`h : (x:int) -> int{ _ = x + 1 }` (exact result, nameable via tier-2),
`consume : (y : int{ y <= 10 }) -> int` (precondition-only refined param),
`fd : (y:int) -> int{ _ = y + 1 }` (dependent result — binder in result).

| # | form | position | before | why |
|---|------|----------|--------|-----|
| a | `consume (g 10)` | precondition-only refined param | **FAIL (silent DISPROVED)** | `g`'s postcondition dropped; precondition VC gets a fresh unknown, `Hypotheses: <none>` |
| a' | `consume (h 8)` | precondition-only, exact-result call | PASS | tier-2 `call_result_name` already names it |
| a'' | `let t = g 10 in consume t` | (let control) | PASS | binder facts carry `t <= 10` |
| b | `consume (5 + 1)` / `consume (x + 1)` | arith into param | PASS | arithmetic is in the `translate` fragment |
| c | `wrap 1 (Cons (2, Nil))` | constructor into dependent param | PASS | constructors are nameable (`translate_surface`/`translate_nameable`) — **the smoke_vrel fold C1 note predates this; already works** |
| d | `fd (g 10)` | dependent-result param | **FAIL (typing ERROR)** | `vox_open_dependent_arrow` cannot name `g 10` → "bind it with a let first" |
| e | `fd (h 10)` | dependent-result, exact call | PASS | tier-2 |
| f | `consume (if x<5 then 1 else 2)` | if-expr into param | **FAIL (silent DISPROVED)** | if with unequal branch refinements has no `result_refinement`, and is not nameable |
| g | `consume (match x with ..-> g 3)` | match-expr into param | **FAIL (silent DISPROVED)** | match value not nameable, refinement dropped |
| h | `consume (g (g 10))` / `fd (g 10)` chains | either | **FAIL** | inner non-exact call not nameable (typing error on the dependent path) |
| i | `match g 10 with ..` | call-result as scrutinee | PASS | `Texp_match` already names the scrutinee |

**Boundary in one line:** the failing cells are exactly the *non-nameable*
arguments — a call with a non-exact result contract, an `if`/`match`
expression, or a chain of these — in a refined or dependent argument position.
Everything nameable (variables, literals, arithmetic, constructors, records,
tuples, field reads, reflected calls, exact-result calls) already works. There
are two distinct failure surfaces:

- **Surface 1 (silent DISPROVED, VC-time):** the param is refined but its binder
  is *not* used in the callee's result (a plain precondition). typing does not
  error; the walker discharges the precondition at a fresh unknown with no fact
  about it, so a true precondition spuriously fails. Cells a, f, g.
- **Surface 2 (typing ERROR):** the param binder *is* used in the callee's
  result type. `vox_open_dependent_arrow` needs a logical name to substitute the
  binder and raises "bind it with a let first". Cells d, h.

## Phase 2 — design

### Naming scheme: location-keyed synthetic idents (logical ANF)

A non-nameable argument's value is named by a synthetic ident derived from the
argument's **source location**. `Vox_reflect.arg_anf_ident : Location.t ->
Ident.t` memoizes loc → ident in a process-global table so that the two passes
that must agree — the type checker's dependent-arrow opening and the verifier's
walker — mint the *same* stamp for the same argument (solver names go through
`Ident.unique_name`, which is stamp-sensitive; a shared string is not enough).

Rendering: the ident is named `*arg*` so it prints like the existing
`*unknown*`/`anon` synthetic names in VC dumps and the editor pane, and it is
registered in `synthetic_names` (hence in scope everywhere, exactly like a
fresh unknown) and in `name_sorts` (from the argument's skeleton type) so the
solver declares it.

Why loc-keying is sound for effects: each *syntactic* occurrence has one
location, so two textually-identical subexpressions at different sites get
distinct idents and are never equated — we never assume purity or hoist. A
single occurrence evaluates to a single value; naming that value is precisely
the manual let-binder's semantics.

### The shared helper

Both surfaces route through one walker helper:

```
dep_arg_name_and_facts env a : Refinement.pred * Refinement.pred list
  = match stable_arg_name a with
    | Some by -> by, []                       (* already nameable — unchanged *)
    | None    -> let n = anf_name env a in    (* loc-keyed, registered *)
                 match result_refinement env a with
                 | Some p -> n, drop_trivial [ p[_:=n] ]
                 | None   -> n, []             (* fact-drop: sound, weaker *)
```

- **Surface 1:** the spine handler discharges the precondition at `n` and adds
  the returned facts to the argument's child context. Walker-only; no lockstep
  concern (the binder is not in the result).
- **Surface 2:** `vox_open_dependent_arrow` substitutes the binder by
  `Pvar (arg_anf_ident loc)` instead of erroring; the walker's
  `apply_result_type` and spine substitute by the same memoized ident, and the
  intro-form re-proof + spine inject the argument's facts at that ident. The
  result type of the whole call then mentions `n`, which is in scope (synthetic)
  and carries its fact.

### Invariants preserved

- **Namer lockstep (#53/#67), surface ⊆ typed:** unchanged for the nameable
  fragment. For the new synthetic fragment lockstep is *by construction* — the
  memo returns one ident per loc to both passes.
- **Fact-drop soundness:** when `result_refinement` yields nothing (or the fact
  is trivial), we drop it. The call still typechecks and its contract VCs are
  still emitted; hypotheses are only weaker, never fabricated.
- **Effect/purity:** loc-keying gives distinct occurrences distinct names.

### Excluded (fail as today, documented)

- **Lambda / relation arguments (arrow-sorted):** a lambda supplied at a
  ghost-arrow dependent parameter is denoted by its reflected form (`Plam`), not
  a first-order scalar name. An *unreflectable* lambda (body not translatable)
  is NOT auto-named — that would mis-sort into the relation position; it keeps
  the clean "bind it with a let first" error. `vox_open_dependent_arrow` gates
  on `Pexp_function`. (Keeps `lean_lambda_rel_fail.ml` unchanged.)
- **`if` / `match` value arguments:** vox does not refine the *value* of an
  `if`/`match` (branch refinements do not unify to one type-level refinement),
  so `result_refinement` yields nothing and the ANF fact is dropped. This is
  exact PARITY with the manual let-bind, which also drops it — verified: both
  `consume (if x<5 then 1 else 2)` and `let n = (if ..) in consume n` fail
  identically (sound). A separate, pre-existing limitation, not C1.
- **Mutable-variable arguments:** already rejected earlier in
  `vox_open_dependent_arrow` with a dedicated message; unchanged.
- **Module-level leak:** once a nested argument is named, if the resulting
  refinement mentions a caller parameter it still hits the pre-existing
  module-level escape rule (annotate with a dependent arrow). Unchanged; only
  the diagnostic a user sees moves from "cannot name" to "escape".

## Phase 3 — results (verified)

- Boundary retest: every previously-failing first-order cell (a, d, chains)
  now PASSES; negatives (`d_dep_neg`, `neg_wrong`, `bad_precond`, `bad_dep`)
  fail closed; if/match stay at let-bind parity (REFUTE).
- Rendering: synthesized names print as `*arg*` (and `*arg*#2`, … for a chain)
  in `-dump-vc`, provenance spans point at the argument; `tools/vox-editor`
  `build_index` ingests them with no crash and correct per-VC statuses.
- New tests: `mechanics/nested_refined.ml` (dump-vc mechanics),
  `mechanics/lean_nested.ml` (end-to-end Lean, positives + negatives),
  `demo/lean_nested_demo.ml` (pipeline living proof). `smoke_vrel`'s fold
  verifies with its `let l = …` removed.
- Feature-surface test updates (the dependent-arg naming boundary, not
  unrelated churn): `errors.ml` (`lt (abs x) x` now named → caught by escape),
  `tuples.ml` and `reflect_deparg_residue.ml` (`g (cheat v)` / `f (opaque x)`
  now accepted with a dropped fact). `lean_lambda_rel_fail.ml` unchanged.

## Phase 3 — tests

Mechanics (`-dump-vc -vox-dry-run`, exact expected dumps) for every failing
cell now passing, negative controls (wrong nested contract still refutes,
fact-drop stays fail-closed), a smoke demo mirroring smoke_vrel's fold without
the let, one editor `build_index` sanity pass, and the full vox suite green.
