# Vpmap — language/compiler needs (build notes)

Module: the **'v-VALUED, int-keyed** polymorphic association map — the
parameterized generalization of `Vmap` on its VALUE side (poly study C-tier-1,
F-C1: "ships now, zero new trust"). Genuine CONS assoc-list repr
(first-binding-wins; `add` = prepend/shadow). Via-abstract face over the
PARAMETERIZED model `MList v`; find results are the exposed PARAMETERIZED ADT
`'v mopt` (`Vox_Vpmap_mopt v`). Model ops exported OPAQUE (oset/Vmap obligation
pattern), probe-confirmed still required at the parameterized value sort.
Genericity mechanism = parameterized ghost sort + parameterized Lean model (the
`Pset` mechanism, `testsuite/tests/vox/lib/pset.{ml,mli}`), NOT a `DecidableEq`
route (values are only stored — keys are `Int`, Lean-native equality).

Verified green: `.mli` + `.ml` seal + `clients/smoke_vpmap.ml` (both int- AND
string-valued instantiations). Compiler: `_install/bin/ocamlc.opt`, pinned Lean
4.31.0. LEAF module (no deps): the `keys` eliminator (Vmap's Vlist-returning op)
is NOT shipped, so no Vlist/Vplist dependency.

Ops as shipped: `empty` (UNSPECCED, see below), `is_empty`, `find`, `add`,
`remove`.
Laws as shipped: `m_isempty_empty`, `m_find_empty`, `m_find_add_eq`,
`m_find_add_ne` (k≠k') + `m_remove_spec`. Client-LIVE: `m_find_add_eq`,
`m_find_add_ne`, `m_find_empty`, `m_remove_spec` (each proven load-bearing by
deletion → exactly its smoke goal fails). NOT client-reachable:
`m_isempty_empty` (consequence of the unspecced-`empty` gap, below).

---

## The 'v-result-ADT boundary — WORKS (no universe/param wall)

- **site:** `Vpmap.mli`/`Vpmap.ml` `type 'v mopt = MMiss | MFound of 'v`;
  `m_find {v} : Int -> MList v -> Vox_Vpmap_mopt v`; `find : … -> 'v mopt{…}`.
- **milestone/gap:** the pair/option-RESULT friction pre-flagged as "the
  boundary the original Vmap dodged".
- **what I tried:** return a *parameterized* exposed ADT `'v mopt` from `find`,
  with the model op `m_find` valued in the auto-generated `Vox_Vpmap_mopt v`.
- **error:** NONE. Both `.mli` and `.ml` seal on the first attempt; `find`
  returns `'v mopt` and clients pattern-match/compare it at int AND string. The
  compiler auto-generates the exposed ADT as a `(v : Type)`-parameterized Lean
  inductive (`Vox_Vpmap_mopt v`) and applies the value sort correctly at each
  instantiation. This CONFIRMS poly study F-C1's `'v mopt` claim end-to-end and
  refutes any universe/parameter wall on a `'v`-carrying value-ADT result.
- **workaround used:** none — the F-B5 "wrap the element in a value-ADT" rule is
  exactly this shape and it just works when parameterized.
- **removed by:** n/a (positive result).
- **severity:** none (positive result — the poly value-ADT result is a shippable
  pattern today).

  **#63 cross-reference (universe-metavar bug — DOES NOT bite here).** Task #63
  is the "constructor field at a via/named sort → `contains universe level
  metavariables … Sort ?u` at the seal" bug (confirmed NOT fixed on this base).
  Vpmap seals FULLY GREEN with zero universe errors, and I could not reproduce
  #63 with any of Vpmap's field shapes or three deliberate cousins:
  - `MFound of 'v` — field = bare type parameter (S_param payload). GREEN.
  - `ACons of int * 'v * 'v alist` — recursive field = the CONCRETE repr
    inductive (not the via type `t`). GREEN. (The via type `'v t` never appears
    as a constructor field anywhere in Vpmap — this is *why* #63 isn't
    exercised.)
  - probe `W of 'v mlist` — field AT the parameterized NAMED/ghost sort. GREEN.
  - probe `Box of t` — field at a MONOMORPHIC via type. GREEN.
  - probe `NCons of 'v t * 'v nest` — a parameterized via type nested as a field
    of ANOTHER via type. GREEN.
  So the parameterized value-ADT payload (`MFound of 'v`, S_param) is NOT the
  #63 bug — an S_param-typed field emits cleanly, and even a via-typed field
  emits cleanly in these shapes. Whatever #63's exact trigger is, it is NARROWER
  than "any via/named-sort field"; Vpmap contributes a confirmed-GREEN negative
  row to the #63 test matrix (S_param payload + concrete-recursive repr, via
  type never a field). Fallback (get_or-style with a default) was NOT needed.

## Opaque model ops STILL required at the parameterized value sort (probed)

- **site:** `Vpmap.mli` (the four `public axiom` model ops).
- **milestone/gap:** dead-law hazard (Vmap's, re-checked under parameterization).
- **what I tried:** the task's open question — "parameterized defs may behave
  differently under unfolding; probe, don't assume." Built an EXPOSED-defs
  variant (`@[grind, expose] public def m_find`/`m_add` with `{v:Type}` bodies)
  and asked whether a client's `find k (add k w m) = m_find k (m_add k w m2)`
  goal still closes with the `m_find_add_eq` law DELETED.
- **error:** it closes (client exit 0) — `grind` unfolds the parameterized
  `m_add`/`m_find` exactly as in the monomorphic case (`m_add` = non-recursive
  `.MCons`, `m_find` matches only the head), so the add-laws are DEAD under
  exposure. Parameterization does NOT change unfoldability.
- **workaround used:** declared `m_find`/`m_add`/`m_isempty`/`m_empty` as OPAQUE
  `public axiom`s (the Vmap/oset pattern), with implicit `{v : Type}`. The four
  laws are then the only path; liveness confirmed by per-law deletion.
- **removed by:** same as Vmap — a per-def "spec-only, sealed against client
  unfold" export, or the M3 dead-law lint generalized to "definitionally-
  derivable law." Applies identically to parameterized defs.
- **severity:** MAJOR-ERGONOMIC (inherited from Vmap; confirmed unchanged by 'v).

## Unspecced `empty` — nullary via-injection over a PARAMETERIZED model (F-B2)

- **site:** `Vpmap.mli` `val empty : (u : unit) -> 'v t` (no `{ _ = m_empty }`);
  `Vpmap.ml` `let empty _ = (ANil : 'v t)`.
- **milestone/gap:** F-B2 / poly study ask-#2.
- **what I tried:** the Vmap spec `empty : (u : unit) -> 'v t{ _ = m_empty }`,
  injecting `ANil`. Also tried let-binding `let e = (ANil : 'v alist) in
  (e : 'v t{ _ = m_empty })` as the error message suggested.
- **error:** the `.mli` ACCEPTS the spec, but the `.ml` via-injection FAILS at
  both forms: `vox: this expression's refined type differs from the refinement
  expected here, and only a variable or an application can be implicitly
  re-refined; let-bind it first`. This is the F-B2 nullary-over-parameterized-
  model wall (the Lean datatype's `v` is unsolved at a parameterless
  constructor's injection), surfacing here as a **re-refinement** error rather
  than F-B2's original "don't know how to synthesize implicit argument a" — the
  message is misleading (the let-bound value IS a variable and still cannot
  re-refine). Worth noting for the ask-#2 implementer: the diagnostic points at
  re-refinement, not at the metavariable, so the root cause is easy to miss.
- **workaround used:** ship `empty` UNSPECCED (poly study B-opt-1 / F-B1
  `pbl_emptyplain`). Emptiness stays OBSERVABLE via `is_empty` (whose argument
  pins `v`).
- **removed by:** ask-#2 (pin a nullary via-constructor's Lean type parameter
  from the result type). Small, localized; would also fix the *message*.
- **severity:** MAJOR-ERGONOMIC.

## Downstream of unspecced `empty`: the two empty-anchored laws are client-UNREACHABLE

- **site:** `m_isempty_empty`, `m_find_empty`; `clients/smoke_vpmap.ml`.
- **milestone/gap:** new — a SHARPER consequence of F-B2 than the study recorded.
- **what I tried:** force `m_isempty_empty` the way Vmap's smoke does
  (`is_empty (empty ())` ⊢ `bool{ _ = true }`).
- **error:** unreachable two ways. (1) No op yields an `m_empty`-specced value
  (empty is unspecced), so `is_empty (empty ())`/`find k (empty ())` carry only
  `m_isempty (m_repr e)` / `m_find k (m_repr e)` with NO link to `m_empty` —
  probed: `find k1 e = m_find k2 m_empty` is NOT PROVED (`*unknown* = m_find k1
  e` in scope, no `e = m_empty` fact). (2) One cannot even STATE a closed
  `m_isempty m_empty` client goal: the value-independent Prop refinement
  `bool{ m_isempty m_empty }` elaborates but fails with `don't know how to
  synthesize implicit argument v`, and the refinement grammar REJECTS the
  disambiguating ascription `bool{ m_isempty (m_empty : MList int) }` (`Syntax
  error` at the `:`).
- **workaround used:** `m_find_empty` IS still reached — via the remove-vs-empty
  comparison `removed_key_gone : int mopt{ _ = m_find k m_empty }` (RHS names
  `m_empty` with `v` pinned by the return ADT type; LHS linked to `.MMiss` by
  `m_remove_spec`; the goal then needs BOTH `m_remove_spec` and `m_find_empty`,
  confirmed by deleting each). `m_isempty_empty` has no such path and is shipped
  DELIBERATELY for Vmap parity — it becomes live automatically once ask-#2 lands
  and `empty` can be specced `{ _ = m_empty }`.
- **removed by:** ask-#2 (spec'd `empty`) restores both laws' reachability;
  additionally, allowing a type ascription inside a refinement predicate would
  let closed model goals pin `v` directly.
- **severity:** MINOR (one shipped law is inert until ask-#2; documented, not a
  soundness or correctness issue).

## Value-level `'v` arg feeds a `MList v`-parameter op — no sort mismatch (Pset parity)

- **site:** `Vpmap.mli` `add : (k : int) -> (w : 'v) -> …`; model `m_add k w m`
  with `w : v` in `MList v`.
- **milestone/gap:** §0 (value-level `'v` sorts at VoxU / `S_other`).
- **what I tried:** pass the value-level `(w : 'v)` (VoxU-sorted per the study's
  §0) as the `v`-typed element of the container's parameterized model.
- **error:** none — exactly as `Pset.add`'s `(x : 'a)` into `ins {a} (x : a)`.
  The generic proof reasons at the shared sort and instantiates soundly at each
  `Tconstr` use (`int t` → Int, `string t` → String).
- **workaround used:** none (mirror Pset).
- **removed by:** n/a (positive result).
- **severity:** none (positive; the value side of a poly container is free).

## Carried over from Vmap (unchanged by 'v; recorded for completeness)

- **remove IS a #31 site** — recursive via-returning op; threads a refined REPR
  skeleton `'v alist{ m_repr _ = m_remove k (m_repr u) }` and injects into
  `'v t` once. `add` (single prepend) is #31-FREE; `find` (primitive `k = k'`
  branch) is #32-FREE. Removed by: the #31 fix. MAJOR-ERGONOMIC.
- **∀-postcondition needs a hand-proved characterization lemma** — `m_remove_find`
  (`by induction m <;> grind`) discharges `m_remove_spec`; grind cannot
  instantiate the ∀ at the recursion's indices alone. MINOR (proof content
  belongs in the `.ml`). Note: parameterizing the induction (`{v : Type}`) needed
  no change — `induction m` on `MList v` works as on `MList`.
- **model theory + client laws authored twice** (`.mli` axioms vs `.ml`
  theorems; inductive + defs in both blocks) — dual authoring, MAJOR-ERGONOMIC;
  removed by a shared model-theory include / prove-only law form (M1). The
  duplication is now *larger* per law: every axiom/theorem/`grind_pattern` gained
  a `{v : Type}` binder and, for the empty-anchored laws, a `(m_empty : MList v)`
  ascription to pin `v` in the pattern.
- **exposed-ADT constructor cannot be named in a refinement** — find smoke goals
  stated as model EQUATIONS between two opaque `m_find` apps, never `.MMiss`/
  `.MFound`. MINOR.
- **nested op-call argument must be let-bound** (C1) — `let a = add … in find k a`.
  MINOR. Bites poly clients identically (study F-X2).
