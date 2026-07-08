# Nested-refined-expressions sweep — findings & fix (2026-07-08)

Sweep of the vox stdlib, demos, and testsuite to exploit the just-landed
nested-refined-expressions feature (commits `6c23b2ea3` / `d5226f5a3` /
`e538d02b9`, design note `2026-07-08-vox-nested-refined-exprs.md`), plus a
root-cause and fix of the depth-3 chain boundary. Base tip `920bb5e74`, branch
`vox-nestsweep`.

## 1. Depth-3 chain boundary — ROOT-CAUSED and FIXED

**Symptom (seeded finding #0, reproduced).** A chain at depth ≥ 3 dropped the
innermost argument's contract:

```
let g (x : int) : int{ _ <= x } = refine_ x
let consume (y : int{ y <= 10 }) : int = (y :> int)
let use_d3 () : int = consume (g (g (g 10)))
```

gave hypotheses `*arg* <= *arg*#2`, `*arg*#2 <= *arg*#3` but **not**
`*arg*#3 <= 10`, so a true goal was DISPROVED. Depth 2 (`consume (g (g 10))`)
worked; the design note's boundary table only ever probed depth 2 (cell h =
`g (g 10)`), which is why the boundary was not caught.

**Root cause: shallow fact collection in TWO code paths of `Vox_verify.walk_expr`.**
Both collect only *one level* of ANF facts and drop everything established
deeper inside a nested argument's own walk:

- **Precondition / dependent-binder path** (the argument loop, ~line 3197). The
  application's return threaded only `!dep_facts`, which accumulated each
  argument's *own* result refinement (`afacts`) but not the transitive ANF
  facts established while *walking* that argument. At depth ≥ 3 the innermost
  fact lives in a sub-argument's walk and was discarded when the inner
  application returned `{ ctx with cfacts = !dep_facts @ ... }` (the inner
  `ctx`, not the walked argument's output ctx).
- **Self-refinement / intro VC path** (`dep_hyps`, ~line 3137). Used when the
  application is itself in a refined position (e.g. it is a function body with a
  refined return type). It collected `snd (dep_arg_name_and_facts a)` per direct
  argument — again one level, never recursing into the argument's arguments.

**Fix (isolated first commit, compiler-only).**

- Precondition/binder path: after walking each argument, capture the facts its
  walk *added* over its child context (`arg_walk_facts`, the length-delta of the
  returned cfacts) and thread them out alongside the argument's result fact.
  Delta facts are always true post-evaluation facts, so this is sound; it is
  guarded (`extra > 0`) so a walk that returns fewer facts degrades to the old
  behaviour rather than misbehaving.
- Self-refinement path: new `arg_chain_facts env a` recurses through the
  application spine, collecting each nested application argument's result
  refinement at its ANF name. `dep_hyps` now uses it.

Both fixes only ADD true hypotheses. A genuinely false chain still fails closed
with a validated counterexample (verified: `consume5 (g (g (g 10)))` with
`consume5 : int{ _ <= 5 }` → DISPROVED, all three chain hypotheses present).

**Validation of the fix.**

- Depth-2 dumps unchanged (byte-identical to the pre-fix `nested_refined.ml`
  expectations).
- Depth-3 and depth-4 now carry the full chain and prove end-to-end in Lean,
  including a variable-terminated chain whose innermost fact mentions a real
  parameter (`atleast (bump (bump (bump n)))`, the exact seeded-finding shape).
- Regression tests added: `nested_refined.ml` (depth-3 dump), `lean_nested.ml`
  (depth-3 positive + variable-terminated + depth-3 negative fail-closed),
  `demo/lean_nested_demo.ml` (a depth-3 transitivity showcase `chain3`).
- Full vox suite: **195 / 0**.

## 2. Sweep — workaround lets removed

All removals verified against current artifacts via the BUILD.md recipe (each
`EXIT=0`). **9 C1 lets removed across 4 files.**

| file | function | removed | enabled by |
|------|----------|---------|-----------|
| `vox_stdlib/clients/smoke_vrel.ml` | `fold_le` | 1 (`let l = Icons ..`) | feature (constructor into dependent param) |
| `vox_stdlib/vmap.ml` | `keys` | 1 (`let l = go r`) | feature/fix (recursive call into `Vlist.cons` dependent param) |
| `vox_stdlib/clients/client_set_elements.ml` | `roundtrip` | 2 (`s'`, `es`) | **the phase-1 fix** — `Vlist.mem x (Vset.elements (Vset.add x s))` needs `vs_addspec` from two levels in; FAILED before the fix, PASSES after |
| `vox_stdlib/clients/client_opt_result.ml` | `ok_then_some` | 3 (`r`, `v`, `o`) | feature/fix (was tagged "C1 KEPT — every inline variant DISPROVES"; now verifies fully inline) |
| `vox_stdlib/clients/client_opt_result.ml` | `ok_and_some` | 2 (`ok`, `some`) | feature (`&&` operands are dependent-arg calls on inline constructors; `decompose_bool` exposes each operand's fact) |

`roundtrip` is the clearest proof the depth fix pays off in real stdlib code: I
verified it FAILS ("NOT PROVED", missing `vs_addspec`) on the build with only
the precondition-path fix and PASSES once the self-refinement-path fix is added.

**Kept deliberately (K):** the `let refine_ ..` / skeleton-threading binders in
`vmap.keys`, `vmap.remove`, and `Vpset.union` are **#31 (transparent-via)
workarounds, not C1** — nesting does not touch them; removing them is a separate
gap. `client_dedup.of_list`/`dedup_elems` keep `let rest`/`let s`: those name a
recursion result at an *unrefined* parameter (readability, not a workaround).
`Vpset.union`'s `let c = un c1 c2 in (c : ..)` and `let r = un xs q` feed a
constructor field / an ascription, not a refined function parameter — nesting is
irrelevant there.

## 3. Findings ledger — where nesting still falls short

**F1 — Depth boundary. FIXED-BY-ME.** See §1. Was a genuine limitation
contradicting the design note's own (depth-2-only) boundary table; now works at
arbitrary depth in both VC paths.

**F2 — `if`/`match`-valued argument in a refined position. GENUINE LIMITATION
(by design), with a UX sharp edge.** Excluded by the feature. The fact is
silently DROPPED and a TRUE goal DISPROVES with `Hypotheses: <none>`:

```
consume (if b then g 5 else g 8)   (* both branches ≤ 8 ≤ 10, yet DISPROVED, no hyps *)
consume (match o with Some _ -> g 5 | None -> g 8)   (* same *)
```

Unlike the lambda and mutable-variable cases (which emit a clean *"bind it with
a let first"* error), an `if`/`match` argument proceeds to a spurious DISPROVED.
**Small-fix-possible:** in a refined argument position, detect a non-nameable
`if`/`match` value with no recoverable `result_refinement` and emit the same
let-first hint the lambda/mutable cases give, instead of discharging against an
empty hypothesis set. (The let-bind workaround is itself fiddly when the
branches carry unequal refinements — that is `result_refinement`'s
both-branches-agree rule, a separate concern from nesting.)

**F3 — Mutable-variable argument. GENUINE LIMITATION (by design), clean error.**
`consume (g m)` for a mutable `m` errors: *"the argument for a dependent
parameter must be an immutable variable (let-bind it first)"*. Good UX; the
let-bind is the intended spelling.

**F4 — Comparisons / field reads inline. IMPROVED (feature, not a gap).** The
`commute.ml` note *"a compound dependent argument is one honest let away …
comparisons, field reads … reach the dependent parameter through their let-bound
name"* is now **outdated**: `use (p < q)` verifies inline (the comparison is
reflectable, named `*unknown* = (p < q)`). Not changed in this sweep (it would
churn a mechanics test documenting the old boundary), but flagged for a future
mechanics update.

**F5 — No weaker-hypotheses (completeness) regressions found.** Every removed
let produces the SAME facts inline as the let form did: `roundtrip` inline
carries both `vs_addspec` and `vs_elements_spec` (checked against the let-form
dump); the chains carry the full transitive fact set. The only dump difference
is the `*arg*`/`*unknown*` naming of the intermediate value, which is expected.

**F6 — via / skeleton (#31) is untouched.** Nesting names a *value*; #31 is
about threading an opaque *skeleton* through a transparent-via binder. The
`let refine_` binders are orthogonal and remain required.

## 4. Editor rendering — spot-checked

Ran `tools/vox-editor/vc_index.build_index` on the depth-3 chain. The VC comes
back fully structured:

```
goal:        *arg* <= 10
hypotheses:  ['*arg* <= *arg*#2', '*arg*#2 <= *arg*#3', '*arg*#3 <= 10']
status:      proved
scope names: ['*arg*', '*arg*#2', '*arg*#3']
```

So the editor's proof pane renders all three chain hypotheses (the innermost one
included, post-fix) with the synthetic `*arg*` names listed in scope, exactly as
the `-dump-vc` output shows them.

## 5. Artifacts

- Compiler fix: `typing/vox_verify.ml` (`arg_walk_facts` in the argument loop;
  `arg_chain_facts` + `dep_hyps` rewire).
- Fix regression tests: `testsuite/tests/vox/mechanics/{nested_refined,lean_nested}.ml`,
  `testsuite/tests/vox/demo/lean_nested_demo.ml`.
- Sweep: `vox_stdlib/vmap.ml`,
  `vox_stdlib/clients/{smoke_vrel,client_set_elements,client_opt_result}.ml`.
- Full vox suite green at **195 / 0**.
