# Vmap — language/compiler needs (build notes)

Module: association map, int keys/values, genuine CONS assoc-list repr
(first-binding-wins; `add` = prepend/shadow). Via-abstract face over the
model `MList`; find results are the exposed ADT `mopt` (`MOpt`). Model ops
exported OPAQUE (oset obligation pattern). Verified green: `.mli` + `.ml`
seal + `smoke_vmap.ml` (all 4 laws forced, each proven load-bearing by
deletion). Compiler: `_install/bin/ocamlc.opt`, pinned Lean 4.31.0.

Ops as shipped: `empty`, `is_empty`, `find`, `add`.
Laws as shipped (all LIVE): `m_isempty_empty`, `m_find_empty`,
`m_find_add_eq`, `m_find_add_ne` (k≠k').

---

### Vmap · opaque model ops required to keep the algebra alive
- **site:** vox_stdlib/vmap.mli:34-63 (all four `public axiom` model ops + laws)
- **milestone/gap:** new
- **what I tried:** the blueprint §3 sketch exposes the model vocabulary
  (`@[grind, expose] public def m_add := .MCons k v`, `public def m_find`).
- **error:** no compile error — a SILENT dead-law hazard. `m_add` is a
  non-recursive prepend and `m_find` matches only the head, so with the
  defs exposed, a client's `grind` discharges `m_find k (m_add k v m)` by
  unfolding, with no reference to `m_find_add_eq`/`_ne`. Under the strict
  §6.7 reading (remove law ⇒ goal fails) all three add/empty laws would be
  DEAD. (Contrast Vlist: `lapp`/`llen` recurse on the abstract argument, so
  `llen_lapp` is un-unfoldable and stays live even when exposed.)
- **workaround used:** declared `m_find`/`m_add`/`m_isempty`/`m_empty` as
  OPAQUE `public axiom`s (the oset pattern), so clients cannot unfold and
  the four laws are the only path — verified live by deleting each law and
  watching exactly its smoke goal fail (lines 19/24/31/38).
- **removed by:** a per-def "spec-only, sealed against client unfold"
  export (expose for stating specs, opaque for client automation), OR a
  lint that flags a shipped law whose goal `grind` already closes without
  it (the M3 dead-law lint, generalized to "definitionally-derivable law").
  Either would let a module keep readable exposed defs without shipping
  dead algebra.
- **severity:** MAJOR-ERGONOMIC

### Vmap · model theory authored in both blocks
- **site:** vox_stdlib/vmap.mli:26-63 and vox_stdlib/vmap.ml:19-52
- **milestone/gap:** model-dup
- **what I tried:** state the `MList` inductive + `m_empty`/`m_isempty`/
  `m_find`/`m_add` once.
- **error:** none — required by the pattern. The `.mli` declares the
  inductive + four OPAQUE model ops; the `.ml` restates the inductive and
  gives all four CONCRETE defs (plus `m_repr`). 5 model decls + 1 inductive
  duplicated across the two blocks.
- **workaround used:** dual authoring; `.ml` defs registered under the same
  solver names discharge the `.mli` axioms at the seal.
- **removed by:** a shared model-theory include, or letting the `.ml`
  import the `.mli`'s inductive/decls rather than redeclaring them.
- **severity:** MAJOR-ERGONOMIC

### Vmap · four client laws typed twice (obligation form)
- **site:** vox_stdlib/vmap.mli:53-63 (axioms) vs vox_stdlib/vmap.ml:39-52 (theorems)
- **milestone/gap:** M1
- **what I tried:** state each client law once.
- **error:** none — the seal demands a same-named, same-typed theorem for
  each `.mli` axiom. 4 laws × (statement + grind_pattern) duplicated
  verbatim.
- **workaround used:** copy the statement into the `.ml` and prove it.
- **removed by:** a prove-only law form (M1): `.mli` states the law once,
  `.ml` supplies only the proof term / tactic, no restated statement.
- **severity:** MAJOR-ERGONOMIC

### Vmap · exposed-ADT constructor cannot be named in a refinement
- **site:** vox_stdlib/clients/smoke_vmap.ml (find goals) — first tried
  `mopt{ _ = .MMiss }` / `mopt{ _ = .MFound v }`
- **milestone/gap:** new  (the pair/option-RESULT friction flagged for this module)
- **what I tried:** assert `find`'s result IS a specific constructor, e.g.
  `find_empty_misses (k:int) : mopt{ _ = .MMiss } = ...`.
- **error:** `line 16, characters 44-45: Syntax error` — the refinement
  predicate grammar has no leading-dot term; bare `MMiss` is likewise not a
  refinement atom. So a function returning an exposed ADT cannot state
  "returns constructor C" as its postcondition.
- **workaround used:** stated the find-law smoke goals as model EQUATIONS
  between two OPAQUE `m_find` applications that the law normalizes to the
  same result (e.g. `find k (add k v m1) : mopt{ _ = m_find k (m_add k v m2) }`
  — both sides `= MFound v` by `m_find_add_eq`), never naming a constructor.
  This is arguably a STRONGER dead-law witness, but it is a workaround for a
  postcondition that should have been expressible directly.
- **removed by:** allow an exposed-ADT constructor in a refinement (dotted
  `.MFound v`, matching the in-block law rule, or bare with elaboration).
- **severity:** MINOR

### Vmap · nested op-call argument must be let-bound
- **site:** vox_stdlib/clients/smoke_vmap.ml:17-18, 23-24, 30-31, 37-38
- **milestone/gap:** C1 (named-call-result injection)
- **what I tried:** `Vmap.is_empty (Vmap.empty ())`, `Vmap.find k (Vmap.add k v m)`.
- **error:** `the argument for a dependent parameter must be a variable or a
  pure expression the logic can name (let-bind it first)`.
- **workaround used:** `let e = Vmap.empty () in Vmap.is_empty e`, etc.
- **removed by:** auto-naming a call-result passed to a dependent parameter.
- **severity:** MINOR

### Vmap · (key,value) entry as a MULTI-ARG constructor — pair concern did NOT bite
- **site:** vox_stdlib/vmap.ml:14 `type alist = ANil | ACons of int * int * alist`
- **milestone/gap:** new (recorded because the pair/tuple hazard was pre-flagged)
- **what I tried:** carry each binding as `ACons of int * int * alist`
  (key, value, tail) and map it with `m_repr | .ACons k v t => .MCons k v (m_repr t)`.
- **error:** none. A tupled constructor is seen by the model exactly like
  Vlist's `Cons of int * tree` (`.ACons k v t`, three scalar args) — NOT a
  pair VALUE. No bool-field / Prop-emission trouble arose (no bool fields).
- **workaround used:** none needed — chose a 3-arg constructor rather than a
  single `(int * int)` tuple field, sidestepping pair-as-value modeling.
- **removed by:** n/a for this shape. NOTE for the deferred htbl/ordered-tree
  Vmap (v1.1): storing `(int * int)` as ONE tuple field, or a `key * value`
  record, is where the flagged pair-as-value modeling would need to be
  probed; it is untested here because the cons-list repr avoids it.
- **severity:** COSMETIC (evidence that the workaround is load-bearing)

### Vmap · m_find_add_ne disequality — grind needed no hand lemma
- **site:** vox_stdlib/vmap.ml:49-51
- **milestone/gap:** #32-adjacent (recorded because the k≠k' case was pre-flagged)
- **what I tried:** `theorem m_find_add_ne (h : k ≠ k') : m_find k (m_add k' v m) = m_find k m := by grind`.
- **error:** none. `grind` discharged the disequality case directly:
  `m_add k' v m` unfolds to `.MCons k' v m`, `m_find` hits `if k = k'`, and
  the hypothesis `k ≠ k'` falsifies the guard with no extra lemma. Ground
  instantiation at distinct literals (1,2) in the smoke likewise needed no
  help — grind decides `1 ≠ 2`.
- **workaround used:** none.
- **removed by:** n/a — better automation is NOT needed for this law.
- **severity:** none (positive result)

### Vmap · NO #31 at `add`, NO #32 at `find` (pre-seeded expectations refuted)
- **site:** vox_stdlib/vmap.ml:60-64 (`add`), vox_stdlib/vmap.ml:51-59 (`find`)
- **milestone/gap:** #31 / #32 (both NOT hit — recorded to calibrate)
- **what I tried:** `add` injects a via value once via a single prepend
  `(ACons (k, v, t0) : t{ _ = m_add k v m })`, exactly like Vlist.cons;
  `find` is a one-path recursion branching on the primitive `if k = k'`.
- **error:** none.
- **workaround used:** none. `add` does NOT thread a recursive via result
  (contrast Vlist.append, which does and needs the #31 skeleton workaround),
  so there is no #31 site. `find` branches on a primitive int test, not a
  bind-then-branch on a spec'd bool (contrast an OR-membership over two
  subtrees), so there is no #32 site. The blueprint's pre-seed ("Vmap.add
  if it threads a via result") is refuted: a prepend-only add never does.
- **removed by:** n/a.
- **severity:** none (calibration note)

---

## Eliminator + relational-def addendum (Phase C, F-2/F-3)

Added `remove` (Mech C), `m_agree` (F-3 relational). `keys` (Mech A) tracked
separately (two-model-import probe). Verified green: `.mli`+`.ml` seal +
extended smoke (removed_key_gone/remove_sees_through/agree_point). Liveness
proven: trivializing `remove`'s postcondition breaks its smoke goals (line 45);
trivializing `m_agree` to `True` breaks exactly `agree_point` (line 56).

### Vmap · remove IS a #31 site (recursive via-returning op)
- **site:** vox_stdlib/vmap.ml `remove` (`let rec go ... alist{ m_repr _ = m_remove k (m_repr u) }`)
- **milestone/gap:** #31
- **what I tried:** the natural `let rest = go r in if k=k' then rest else ACons (k',v,rest)`
  returning `t` directly.
- **error:** (pre-#31 clone) a recursive op that returns the via type `t`
  loses its image map at the `let rest = ...` binding — the same failure
  Vlist.append hits. (Unlike `add`, which is a single non-recursive prepend
  and is #31-free — so within one module `add` is clean and `remove` is not,
  a clean calibration of exactly when #31 bites.)
- **workaround used:** the skeleton-thread: `go` returns a refined REPR
  skeleton `alist{ m_repr _ = m_remove k (m_repr u) }`, threaded through the
  recursion, injected into `t` once via `let res = go t0 in (res : t{ m_remove_spec _ k m })`.
- **removed by:** #31 fix (via value keeps its map across a `let`).
- **severity:** MAJOR-ERGONOMIC

### Vmap · ∀-postcondition needs a hand-proved characterization lemma
- **site:** vox_stdlib/vmap.ml `m_remove_find` (induction) discharging `m_remove_spec`
- **milestone/gap:** new (F-3 impl side)
- **what I tried:** let the op's `∀ k'` postcondition (`m_remove_spec`) fall
  straight out of the recursive `m_remove` def.
- **error:** grind cannot instantiate a `∀`-fact at the recursion's indices
  on its own (the same wall as mutable-array per-call frame conditions). The
  op VC `∀ k', m_find k' (m_remove k M) = if k'=k then MMiss else m_find k' M`
  needs the statement proved once by induction.
- **workaround used:** ship `m_remove_find` (`by induction m <;> grind`,
  grind_pattern on `m_find k' (m_remove k m)`) as the step lemma; the op VC
  and every client instantiation then fire off it. This is the
  loop-invariant-as-prelude-lemma discipline applied to an eliminator.
- **removed by:** n/a (induction is genuine proof content, correctly located
  in the `.ml`); a `∀`-aware VC instantiator would remove the manual lemma.
- **severity:** MINOR (expected; the proof belongs somewhere)

### Vmap · F-3 quantifier lives in the shipped def — client writes none
- **site:** vox_stdlib/vmap.mli `m_remove_spec`/`m_agree` (`@[grind, expose]`);
  vox_stdlib/clients/smoke_vmap.ml `agree_point`, `removed_key_gone`
- **milestone/gap:** N-F3 (positive result)
- **what I tried:** consume `m_agree a b` / `m_remove_spec r k m` in a client
  by instantiating at a point (`agree_point` refines `b : t{ m_agree a _ }`,
  goal `_ = m_find k a`).
- **error:** none. The `∀` in the exposed def unfolds and grind instantiates
  it at the client's point with no client-side quantifier — exactly the F-3
  mitigation the addendum predicted. Confirms a client CAN consume a
  quantified relational def as a hypothesis. Dependent arg refinement
  (`b : Vmap.t{ m_agree a _ }` referencing prior arg `a`) works.
- **workaround used:** none — this is the intended pattern working.
- **removed by:** n/a.
- **severity:** none (positive result; F-3 vocabulary is usable today)

### Vmap · keys (Mech A) — DONE (needed Vlist's ll_nil_not_mem + equational empty-spec)
- **site:** vox_stdlib/vmap.{mli,ml} `keys : (m : t) -> Vlist.t{ m_keys_spec _ m }`;
  smoke `key_enumerated` in smoke_vmap.ml.
- **milestone/gap:** F-2 (cross-module eliminator composition)
- **what I tried:** membership-DIRECT keys build — `keys_go : alist -> Vlist.t{ m_keys_spec _ (m_repr u) }`
  with `m_keys_spec (l : LList) (m : MList) := ∀ k, ll_mem k l = m_haskey k m`
  and `m_haskey k m := m_find k m ≠ .MMiss`; base arm `Vlist.empty ()`, step
  `let l = go r in Vlist.cons k l`. Enumerates KEYS only (int list — dodges the
  pair-as-value hazard; values-enumeration deferred to v1.1).
- **error:** the base goal `ll_mem k ll_nil = False` was underivable against the
  INTERIM Vlist (opaque `ll_nil`, no non-membership law) — grind-fragile inside
  the induction/VC, and a hypothesis-form inversion wouldn't fire. (The
  STRUCTURAL `.LCons`/`ll_cons` mirror is also a dead end: `ll_cons` is opaque
  and the empty base needs a not-mem fact regardless.)
- **workaround used:** none needed once building against the FINAL Vlist: its
  equational `empty : t{ _ = ll_nil }` lets grind substitute IMG := ll_nil, so
  the shipped `ll_nil_not_mem` (grind_pattern `=> ll_mem x ll_nil`) fires at the
  base; cons step by the existing `ll_mem_cons`; final by the carried spec. No
  structural mirror / bridge (contrast build-vset's Vset.elements — both valid,
  membership-direct is the lower-scaffolding cut on the final Vlist). #31 does
  NOT block: building an EXTERNAL module's via type, the recursive `Vlist.t`
  result keeps its refinement across `let l = go r` (seen in the solver
  hypotheses), so no skeleton-thread — contrast same-unit `remove` which IS a
  #31 site. STEP-0 two-model-import `.mli` (open Vlist, ∀-spec over imported
  LList + own MList) elaborates clean. Verified against refreshed `_artifacts`
  Vlist (mli+ml exit 0), smoke green, liveness-swept (trivializing `m_keys_spec`
  breaks `key_enumerated`, line 64).
- **removed by:** the `ll_nil_not_mem` law + equational empty-spec (both now
  shipped by the final Vlist — I identified/verified/routed the law, landed at
  Vlist.mli:95). General lesson: pin ONE fixed dependency snapshot per wave —
  the fragility was pure snapshot skew (an interim Vlist lacked the law; a
  client-side `ll_isnil`-inversion shortcut only worked while `ll_isnil` was
  still exposed in a stale `_artifacts` olean).
- **severity:** RESOLVED (was BLOCKING; fixed by the Vlist law + snapshot pin).

  NOTE (advisory §6.7): `m_haskey` is exposed and non-recursive with no law
  about it, so it draws the Amendment-A WARN — kept DELIBERATELY exposed: it is
  the client's bridge from key-enumeration to find-presence (unfolds to
  `m_find k m ≠ .MMiss`), which is load-bearing utility, and exposing it kills
  no law (nothing is dead). Same posture as Vlist's `ll_isnil` pre-de-expose.

---

## WP-3 surface completion (mem / singleton / union) + probes (bindings, cardinal, fold)

Shipped `mem` (bool key-presence query), `singleton`, `union` (a-biased),
all VERIFIED against Vlist+Voption artifacts, smoke-exercised, negative controls
fail closed. Three requested ops DEFERRED with findings (below): `bindings`
(pair-model probe), `cardinal` (shadowing), `fold` (arity + shadowing).

### Vmap · mem / singleton / union — positive
- **site:** vox_stdlib/vmap.ml `mem`/`singleton`/`union`, `m_app`/`m_find_app`,
  mli `m_unionspec`
- **milestone/gap:** new (map surface)
- **what I tried:** `mem` = one-path recursion branching on the primitive `k=k'`
  (no #32), spec `_ = m_haskey k m` (m_haskey already shipped, exposed).
  `singleton k v` = direct `ACons (k,v,ANil)` injection (avoids an intra-unit
  via call), spec `_ = m_add k v m_empty`. `union` = a-biased list append
  (m_app), spec `m_unionspec r a b := ∀k, m_find k r = match m_find k a with
  MMiss => m_find k b | x => x` — the first-match-wins semantics that `add`'s
  prepend-shadow already establishes.
- **error:** none. `union` threads the refined skeleton like `keys`/`remove`
  (#31 pattern); `m_find_app` (proved `induction a <;> grind`) discharges
  `m_unionspec`.
- **workaround used:** the #31 skeleton-thread for `union` (same as `remove`);
  direct-constructor injection for `singleton` (same as `add`).
- **removed by:** n/a (positive).
- **severity:** none (positive result)

### Vmap · union bias choice (a-biased / left-wins) — design
- **site:** vox_stdlib/vmap.mli `m_unionspec`
- **milestone/gap:** new (design decision, per WP-3 dispatch)
- **what I tried:** pick a union bias. Chose A-BIASED (a's binding wins where a
  has the key) because it is exactly LIST APPEND (a ++ b) and `m_find` already
  returns the first match — so the spec `m_find (union a b) = find-a-else-b`
  falls straight out of `m_find_app` with no new machinery, AND it is consistent
  with `add`'s "first binding wins / prepend shadows" story. Right-biased would
  reverse the append and contradict that story.
- **error:** none.
- **workaround used:** n/a (design).
- **removed by:** n/a.
- **severity:** none (design justification)

### Vmap · bindings PAIR-MODEL PROBE — verdict: expressible, but needs a new container
- **site:** vox_stdlib/scratch_probe/wp3/probe_pair_tuple.ml (+ the MList precedent)
- **milestone/gap:** L (pair/tuple value model)
- **what I tried:** model `bindings : t -> <list of (int*int)>`. Probe A: give an
  OCaml tuple alias `type pair = int * int` a `[@@vox.sort lean "IPair"]`.
- **error:** `vox: vox.sort on a type alias has no effect (an alias expands to
  its definition before sorting)` — a tuple is a structural alias, so a VALUE
  pair `(int*int)` cannot carry a model sort. So `bindings` as a tuple-element
  list is not directly modelable.
- **workaround used:** a pair is expressible as a NOMINAL pair-carrying inductive
  — indeed `MList` itself IS `MCons : Int -> Int -> MList`, a verified list of
  (key,value). So the MODEL layer expresses key×value entries fine; the block
  can define `pl_headkey`/`pl_headval : PList -> Int` accessors (both int, no
  tuple destructor). Shipping `bindings` therefore means a WHOLE NEW via-abstract
  "pair list" container (own inductive + head_key/head_val/tail accessors +
  membership/lookup laws) — that duplicates most of a list module and is beyond
  Vmap surface completion. AND a client can already reconstruct bindings from the
  shipped `keys` eliminator + `find` (`for each k in keys m: (k, find k m)`), so
  a dedicated op is not load-bearing.
- **removed by:** a first-class tuple/pair value sort (then `bindings :
  t -> Vlist-of-pairs` is trivial), OR a dedicated `Vpairlist`/`Vassoc` module.
- **severity:** MEDIUM (deferred to v1.1; keys+find covers the use case today)

### Vmap · cardinal DEFERRED — shadowing model + Decidable m_haskey
- **site:** (not shipped) — vox_stdlib/vmap.ml model `MList`
- **milestone/gap:** new (map cardinality vs shadowing invariant)
- **what I tried:** `cardinal : t -> int{ _ = distinct-key count }`. The map's
  `add` is an unconditional prepend (shadows), so the assoc-list can hold
  DUPLICATE keys and `remove` drops all of them — the invariant is NOT "distinct
  keys". A list-length cardinal over-counts shadowed bindings (wrong vs OCaml
  Map.cardinal). A correct distinct count needs `m_card (MCons k v t) = m_card t
  + (if m_haskey k t then 0 else 1)`, which needs `Decidable (m_haskey k t)`
  (= `m_find k t ≠ .MMiss`) in the model AND an O(n^2) impl that re-scans the
  tail per entry.
- **error:** (anticipated) Decidable-m_haskey in the model + a costly, hard-to-
  verify dedup impl.
- **workaround used:** DEFER. Not shipped.
- **removed by:** either a distinct-keys invariant on the repr (would change
  `add` to overwrite, out of scope) or a Decidable m_haskey instance + dedup
  proof.
- **severity:** MEDIUM (deferred; the shadowing model is the real obstacle)

### Vmap · fold DEFERRED — HOF kit is ternary; map fold is quaternary + shadowing
- **site:** (not shipped) — HOF kit (Vlist ternary `relFold`)
- **milestone/gap:** L14-adjacent (HOF kit arity)
- **what I tried:** `fold : (key -> value -> acc -> acc) -> ...`. The WP-0 HOF
  kit's `relFold` is TERNARY (acc:int, elem:int, acc':int); a map fold needs a
  QUATERNARY step (acc, key, value, acc'). Folding the raw assoc-list with the
  ternary kit (over keys or values only) would ALSO visit shadowed bindings
  (add prepends), so a fold spec over `m_repr` leaks shadowed entries — wrong
  for a map fold that should visit each key once.
- **error:** ternary kit cannot express the (key,value) step; shadowing leaks.
- **workaround used:** DEFER. A client folds over `keys m` (Vlist.fold_left) and
  looks up values with `find` (which returns the VISIBLE binding), making the
  fold explicit and shadow-correct.
- **removed by:** a quaternary HOF kit variant + a distinct-keys map invariant.
- **severity:** MEDIUM (deferred; client keys+find+Vlist.fold_left covers it)
