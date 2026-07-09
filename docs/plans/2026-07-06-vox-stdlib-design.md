# A verified standard library for vox

*Design document, 2026-07-06.*

vox has, as of this week, all the pieces a verified standard library
needs: refinement types + contracts, `total_` spec functions, `[@vox.via]`
(a type modelled through a Lean function), `[@@vox.lemma]` proof-carrying
lemmas, `[@@vox.reflect]` trusted primitive bindings, `[@@vox.sort
opaque|lean]`, McCarthy-store mutable arrays with RustHorn borrows, and
sealed `.mli` blocks that export a model to clients via `VoxSig_*.olean`.
A dozen verified artifacts already live in `testsuite/tests/vox/lib` and
`demo`. This document proposes how to turn that raw material into a
coherent, small-TCB **vox stdlib**: what modules it contains, the house
rules for writing them, the trust ledger, the gaps that bite (each
probe-verified), packaging, and a roadmap.

The central design claim, validated empirically below (§4, Probe A/A2):
**a stdlib's client-facing lemmas must be `public theorem`s in the `.mli`
block**, because those ride `VoxSig_*.olean` to clients, whereas
`[@@vox.lemma]` facts are same-unit only and do **not** cross the `.cmi`.
Everything else follows from that and from the `via` abstraction story.

---

## 0. What "stdlib-grade" means here

Three tiers, because not every artifact should be client-facing:

- **Core** — client-facing modules with a stable, abstraction-preserving
  interface and a shipped lemma algebra. The thing a development `open`s.
- **Backend** — a concrete verified implementation that sits *behind* a
  Core module's `via` interface, interchangeable with other backends
  (this is the payoff of representation independence: `Vset`'s client
  never learns whether the backend is a BST, an RB-tree, or a Patricia
  trie).
- **Showcase** — a self-contained verified artifact whose value is a
  demonstration or a proof-of-technique, not a general-purpose API.

A Core module must: (a) hide its representation behind `via` or an opaque
sort unless the representation *is* the client's vocabulary; (b) ship its
lemma algebra as `public theorem`s in the `.mli`; (c) carry **zero**
`assume_unchecked_` and no `.ml` `axiom` unless that trust is named and
localized (a reflect binding or a borrow library).

---

## 1. Scope + module map

Legend for **Model**: *native* = `via` to a built-in Lean type (`Int`,
`Nat`, `List`), no block theory needed; *inductive* = `via` to a
block-defined Lean `inductive`; *extensional* = `via` to `Int -> Prop`;
*exposed ADT* = concrete variant visible to clients, model in an `.mli`
block; *opaque* = `[@@vox.sort opaque]` with algebraic laws; *reflect* =
`[@@vox.reflect]` primitive binding.

### Core (client-facing)

| Module | Repr | Model (via what) | Core ops (contract sketch) | Graduates |
|---|---|---|---|---|
| **Vlist** | cons-list | inductive `LList` (or native `List`) | `empty:{lisnil _}`, `cons x l:{_=lcons x l}`, `length l:{_=llen l}`, `mem x l:{_=lmem x l}`, `append a b:{_=lapp a b}` | **new (PoC built, §7)** |
| **Voption** | `None\|Some` | exposed ADT / native `Option` | `is_some o:{_=(o≠none)}`, `get_or d o:{...}`, `map f o`, `bind` | new (trivial) |
| **Vresult** | `Ok\|Error` | exposed ADT | `is_ok`, `map_ok`, `map_err`, `get_or` | new (trivial) |
| **Vset** (int) | sorted tree | inductive `ISet` or extensional `Int→Prop` | `empty:{no_mem _}`, `mem x s:{_=mem x s}`, `add x s:{_=ins x s}`, `union`, `card` | **upgrade `via_set`/`xset`** |
| **Vmap** (int keys) | assoc / tree | `via` a Lean `Int→Option v` | `empty`, `find k m`, `add k v m`, `remove` | **graduate `htbl`→hashmap; ordered defer** |
| **Vbits** | `int` | reflect `[@@vox.reflect]` bit ops | `land`, `lor`, `lsr` with block-axiom masking algebra | **graduate `reflectbits`** |
| **Viarray** | `int iarray` | reflect `ia_len`/`ia_get` prims | `length a:{_=ia_len a}`, `get a i:{...}` (bounds in contract) | **graduate `ia_lib`** |

### Backend (interchangeable, behind a Core `via` interface)

| Module | Repr | Verified property | Grade | Graduates |
|---|---|---|---|---|
| **Vset_bst** | plain BST | ordering invariant, `mem`/`insert` model | fully proved | `bst` |
| **Vset_rbt** | red-black tree | bst + no-red-red + equal black-height | fully proved | `rbt` |
| **Vset_trie** | Patricia trie | bit-level membership model | proved *above* 3 bit prims | `ptrie` (+`triset` face) |
| **Vhashmap** | fixed-width buckets | one-bucket-lookup correctness | fully proved | `htbl` |

The three set backends all satisfy the same `type t : value refines
(iset)` interface: swapping one for another re-verifies **no client
code**. That is the flagship demonstration of `via`, and the reason
`Vset`'s interface should be via-abstract rather than an exposed ADT.

### Showcase (demonstrations, not general API)

`peano` (unary nats as `Nat`), `bignum` (binary bignums, same interface —
two machines, one spec), `cfold` (constant folder, denotation-preserving
via `Int`), `utf8` (codec with round-trip), `mset` (mutable set behind a
borrow), `pset` (parameterized add-only set). Keep these as-is; they teach
the modeling spectrum and the borrow/`via` interaction. `gset` and the
`step_*`/`rbt_bad_balance` fail-closed fixtures are test scaffolding, not
stdlib.

### What's stdlib-grade vs demo-grade (verdict)

Fully honest (zero trusted assumptions), ready to graduate: `bst`, `rbt`,
`htbl`, `via_set`, `xset`, `pset`, `cfold`, `peano`, `bignum`,
`reflectbits`, and the new `Vlist`. Trust to flag on graduation: `ptrie`
(3 bit-hack primitives), `mset` (borrow library), `gset` (fully
`assume_unchecked_` — **not** stdlib-grade, it is a trusted-ghost fixture).

---

## 2. Spec-style conventions (house rules)

These are the rules that make modules *compose* and keep the TCB legible.

### R1 — Choose the model by asking "does Lean already have this type?"

- **Yes → `via` to the native type**, ship no model theory (arithmetic /
  list / set automation is Lean's). `peano`/`bignum` (`Nat`), `cfold`
  (`Int`) do this. A `Vlist` modelled on native `List` inherits
  `List.length_append` etc. for free.
- **No, but a decidable inductive suffices → `via` to a block
  `inductive`** (`via_set`'s `ISet`, `Vlist`'s `LList`). You write the
  defs and prove the algebra, but clients get concrete counterexamples.
- **You need extensionality / set equalities → `via` to `Int -> Prop`**
  (`xset`). Buys `ins_idem`-style equalities; costs the decidable `card`.
- **You want laws over an abstract type with no concrete model → opaque
  sort** (`oset`). `via` subsumes this whenever a real model exists;
  reach for opaque only when it genuinely doesn't.

### R2 — Prefer `via` over an exposed ADT for client-facing containers

`via` gives representation independence (swap backends) *and* an abstract
client vocabulary (clients reason with `mem`/`ins`/`llen`, never tree
recursions). Use an **exposed ADT** only when the client legitimately
constructs and matches the constructors (a showcase teaching module, or a
type whose constructors *are* the API like `Voption`).

### R3 — Client-facing lemmas are `public theorem`s in the `.mli` block

**This is the load-bearing rule.** `[@@vox.lemma]` in a `.ml` is same-unit
only (§4 Probe A, confirmed). A stdlib's whole point is that its algebra is
ambient in clients, so every law a client needs must be a `public theorem`
inside the `.mli` `[%%vox.lean]` block, proved by `induction _ <;> grind`
and given a `grind_pattern`. That artifact compiles to `VoxSig_<Unit>.olean`
and travels to every client (§4 Probe A2, confirmed). `bst.mli`, `rbt.mli`,
`htbl.mli`, and the new `Vlist` all do exactly this. Use `[@@vox.lemma]`
only for *internal* proof structure the implementation itself consumes.

### R4 — `total_` name-only vs equations-exposed

- **`val total_ f : ...` (name-only)** when the measure is over a *hidden*
  ADT and clients should reason only from exported contracts, never by
  unfolding — the abstraction-preserving default (`mli_totalspec`).
- **`@[grind, expose] public def f` in the `.mli` block** when clients must
  *compute* with the model because the model **is** their vocabulary — a
  container like `Vlist` exposes `llen`/`lmem`/`lapp`; `mli_exposed` is the
  contrast. Containers expose; measures-over-hidden-invariants stay
  name-only.

### R5 — Which algebra each module must ship

- **Sizes/measures:** non-negativity (`llen l >= 0`), measure-of-combine
  (`llen (lapp a b) = llen a + llen b`, `card`-of-union).
- **Membership:** membership-of-constructor (`mem_insert`, `lmem_lapp`),
  and the ordering-derived not-membership bounds (`not_mem_lt/gt`) that
  make one-path search complete.
- **Invariant preservation:** `bst (insert x t)`, `rb (add x t)` — the
  invariant is maintained at every production site.

### R6 — Naming discipline (three hard-won gotchas, §7)

- Model type: `TitleCase` Lean name (`LList`, `ISet`). Spec functions:
  lowercase, domain-prefixed (`llen`, `lmem`, `lapp`; `mem`, `ins`).
- **Never name a spec/map function after a Lean core identifier.** A map
  named `repr` collides with core `Repr.repr` ("already declared"); avoid
  `repr`, `id`, `min`, `max`, bare `length`. The PoC's map is `lrepr`.
- **Specs mention `def`s, never model constructors.** `{ _ = lcons x l }`
  needs a `def lcons := .LCons x l`; a bare constructor `lcons` in a
  refinement elaborates to an application of a non-function ("Function
  expected at").
- **Public model/ghost names are globally unique across co-imported
  units** (sweep F2). Two units that each `public`-declare `ISet` cannot be
  co-imported by one client, even byte-identically. **Share a model by
  importing the one unit that defines it, never by redefining it.**

### R7 — Composition across modules

`via` composes across modules only through *importing the defining unit's
model*. A `Vmap` whose values are `Vset`s, or that reuses `Vset`'s model,
must `import` `Vset`'s `VoxSig` and name `ISet` from there (R6). This
works (the reflect cross-unit test and `triset`-over-`ptrie` do it); it is
the only sound way to share a Lean model between two units.

---

## 3. Trust ledger (the TCB)

A verified stdlib's selling point is a **small, auditable** assumption
list. Itemized:

### Foundational (shared by every vox module, unavoidable)

- The Lean 4 kernel + the `grind` tactic (soundness of proof checking).
- The vox compiler's VC generation and the OCaml→Lean reflection
  (`vox_reflect.ml`): that a refinement predicate and the emitted Lean
  goal denote the same thing. This includes vox's "ideal arithmetic"
  stance (unbounded `Int`; machine overflow is out of model).

### Per-module, itemized

| Trust item | Modules | Class | Where declared |
|---|---|---|---|
| *(none — fully proved)* | Vlist, Voption, Vresult, Vset(-bst/-rbt), Vhashmap, `via_set`, `xset`, `pset`, `cfold`, `peano`, `bignum` | proved | — |
| 3 bitwise primitives (`zero_bit`, `mask`, `branching_bit`) | Vset_trie (`ptrie`) | `assume_unchecked_` | the `.ml`, reviewed |
| `[@@vox.reflect]` correspondence (e.g. `imin`↔`bmin`, `land`↔`Vox.land`) + any block masking axioms | Vbits (`reflectbits`), Viarray if it reflects | assumed | the `.mli`, reviewed |
| Borrow library (≈6 trusted `assume_unchecked_` functions) | mutable Varray/Vset, `mset` | assumed | a *named* companion lib (`mset_lib`/`bslice`) |
| `[@@vox.sort lean "Name"]` type correspondence | any ghost sort **without** a via manifest | assumed | the declaring `.mli` |

Two points that keep the ledger small:

- A `[@@vox.sort lean]` **backed by a `via` manifest is honest, not
  assumed**: the abstraction function is a real `def` and the `refines`
  inclusion is machine-checked (`Vlist`'s `LList`, `via_set`'s `ISet`).
  Only a *bare* ghost sort (a trusted handle type) is TCB.
- An interface `axiom` (`oset`) is **not** TCB — it is an *obligation* the
  implementation's seal must discharge with a same-named proved theorem.

### Review story

The stdlib's full assumption list is the union of the reflect
correspondences (§Vbits/Viarray) and the borrow-library primitives (mutable
containers), plus any bare ghost sorts. **Both are confined to named
companion modules** (`Vbits`, the borrow lib), so an auditor reviews a
short, fixed set of files and nothing else in the stdlib can add to the
TCB — every other module is machine-checked end to end. This is the
concrete "small, auditable" claim.

---

## 4. Gaps that block it (ranked, probe-verified)

Every "blocking/annoying" claim below is backed by a compile I ran against
the installed compiler + pinned Lean 4.31 (sources in `scratch_probe/`).

### Probe A — cross-unit `[@@vox.lemma]` does NOT travel (**confirmed**)

`lemlib.ml` defines `total_ len` and a `[@@vox.lemma] lemma_len_nonneg`;
`lemlib.mli` exports `len` name-only. Client:

```ocaml
let use_nonneg (l : Lemlib.ilist) : int{ _ >= 0 } = refine_ (Lemlib.len l)
```

**Fails**: `NOT PROVED — Goal: len l >= 0, Hypotheses: <none>`. The ambient
nonneg fact is same-unit only; it does not ride the `.cmi`.

### Probe A2 — a `public theorem` in the `.mli` block DOES travel (**confirmed**)

`lemlib2.mli` puts `len` and `public theorem len_nonneg` (with a
`grind_pattern`) in a block. Client `let check (l) : unit{ len l >= 0 } =
()` **passes** — the theorem arrived via `VoxSig_Lemlib2.olean`. This is
why **R3** is the house rule, and why the lemma gap is *annoying, not
blocking*.

### Probe B — cmi-riding packaging (**confirmed**, §5)

A client compiled against only `lemlib2.cmi` + `.cmo` +
`VoxSig_Lemlib2.olean` on the `-I` path (no source present) verifies;
removing the olean flips it to `unknown module prefix 'VoxSig_Lemlib2'` and
the VC fails. So the olean is load-bearing and *sufficient*: an installed
stdlib is cmis + oleans, nothing more.

### Ranked list

| Gap | For the stdlib | Evidence | Ask |
|---|---|---|---|
| **Generic ordered functor** `Make(Ord)` for `Vset`/`Vmap` over an arbitrary ordered element | **BLOCKING** *for that feature*; scoped around by shipping int-keyed + order-free-generic containers | functor findings: blocks nested in a functor body are dropped; a truly abstract order can't discharge at instantiation | needs compiler work (functor block collection + per-instantiation order) — defer to v2 |
| **Cross-unit `[@@vox.lemma]`** | **ANNOYING** (clean workaround = R3) | Probe A / A2 | ergonomic win: have `[@@vox.lemma]` also emit a `public theorem` into the unit's `VoxSig` |
| **`via` value loses its map at a `let` binding (#31)** | **ANNOYING** (bites every recursive via-returning op) | PoC `append` (§7): `let rest = go r` binds `rest` at the image sort, not the tree | fix so a `let`-bound via value keeps its type; until then, R-pattern: helpers return a refined *skeleton* `tree{ lrepr _ = ... }` and inject once |
| **Branch on a refined bool doesn't thread its fact (#32)** | **ANNOYING** | `rbt` worked around with explicit colour matching | thread the `bool{ _ = p }` fact into both branches |
| **Named-value (call-result) injection into a `via` type mis-sorts** | **COSMETIC** | PoC `(go ta : t{...})` failed; `let res = go ta in (res : t{...})` passed | bind to a variable first |
| **Spec name collides with a Lean core identifier** | **COSMETIC** (naming discipline, R6) | PoC map `repr` → "already declared" | optional: a validator warning |
| **0-ary spec constant not referenceable in a refinement** | **COSMETIC** | PoC uses `lisnil _` instead of a bare `lnil` literal | minor surface fix |
| **At-a-distance reflect (`[%%vox.reflect Path => "Sym"]`, phase 3)** | **COSMETIC** for v1 | reflect design doc | v1 declares reflect bindings in a companion `.mli`; not needed |

Net: exactly **one** true blocker (the generic ordered functor), and it is
scoped around, not on the v1 critical path. The rest are ergonomics with
established workarounds.

---

## 5. Packaging + build

- **Dependency = cmis + oleans (Probe B).** A development that uses the
  stdlib compiles against the installed `.cmi`/`.cmo` and the
  `VoxSig_*.olean` files on the include path. No source, no re-verification;
  the client's own VCs consult the imported oleans.
- **In-tree layout.** Promote a real library out of the test incubator:
  `oxcaml/vox/stdlib/` (or a dune `(library (name vox_std))`), each module a
  `.ml`/`.mli`, with the build installing the cmis and the emitted
  `VoxSig_*.olean` beside them. The `testsuite/tests/vox/lib` artifacts stay
  as the regression corpus and the graduation source.
- **Editor examples.** The proof-pane editor can ship stdlib-backed
  examples: a client snippet that `open`s `Vlist`/`Vset` and shows the
  ambient algebra discharging a goal, with the `VoxSig` oleans preloaded on
  its solver path (the editor already resolves oleans by include path).
- **Migration of existing `lib/` artifacts.** Graduate the honest ones
  (§1); rename to the `V*` scheme; move `bst`/`rbt`/`ptrie` under the
  `Vset` via-interface as backends; keep the showcases where they are and
  cross-link them from the stdlib docs.

---

## 6. Roadmap

**v0 — prove the concept (2–3 modules, no compiler work).** `Vlist` (PoC
done, §7), `Voption`/`Vresult` (trivial exposed ADTs), `Vbits` (graduate
`reflectbits`). Everything v0 needs works on today's compiler. Deliverable:
a client that `open`s these and verifies against their shipped algebra.

**v1 — the useful core.** `Vset` (via-abstract, real sorted-BST backend,
full membership/ordering algebra — upgrade `via_set`'s degenerate `elems`
to full-tree membership), `Vhashmap` (graduate `htbl`), `Viarray`
(graduate `ia_lib`). Compiler work that pays for itself here: fix #31
(via-let-binding) and #32 (refined-bool branching) — both are pure
ergonomics that make container implementations readable; optional but
high-value: `[@@vox.lemma]` → `VoxSig` export (removes the hand-written
`.mli` theorem duplication).

**v1.1 — backends + mutation.** `Vset_rbt`/`Vset_trie` as drop-in backends
behind `Vset`'s interface (the representation-independence showcase);
mutable `Varray`/`Vset` behind the borrow library (adds the borrow-lib TCB,
so gate on review of that one companion module).

**v2 — generic ordered containers.** `Vset.Make(Ord)`/`Vmap.Make(Ord)`
over an arbitrary ordered element. Requires the compiler work the functor
findings flag: collecting `[%%vox.lean]` blocks from functor bodies, and a
per-instantiation definable order so an abstract `lt` discharges at `int`.
This is the one item that is genuinely blocked today.

---

## 7. What the PoC taught (`scratch_probe/vlist.{ml,mli}` + `vlist_client.ml`)

I built `Vlist`: a cons-list whose logical model is a Lean `LList`
inductive, reached through `type t = tree{ 0 = 0 } [@vox.via (lrepr :
llist)]`. The `.mli` is via-abstract (`type t : value refines (llist)`),
exposes the model (`lcons`/`llen`/`lmem`/`lapp`/`lisnil`), and ships a
three-theorem algebra (`llen_nonneg`, `llen_lapp`, `lmem_lapp`) as `public
theorem`s. Five operations — `empty`, `cons`, `length`, `mem`, `append` —
verify **honestly, zero `assume_unchecked_`**, in ~1.8 s. A cross-unit
client composes real calls and the algebra fires ambiently:

```ocaml
let total_len (a : Vlist.t) (b : Vlist.t) : int{ _ = llen a + llen b } =
  let ab = Vlist.append a b in
  Vlist.length ab            (* llen (lapp a b) --llen_lapp--> llen a + llen b *)
```

Lessons that shaped the house rules above:

1. **`[@@vox.lemma]` doesn't cross units** (Probe A) — so the algebra had
   to be `public theorem`s in the `.mli` (R3). This is the single most
   important design constraint.
2. **Gap #31 dominates recursive `via` code.** A recursive helper that
   returns the abstract `t` loses its representation at the `let` that
   binds the recursive result. The robust pattern is a helper that returns
   a **refined skeleton** `tree{ lrepr _ = <image eq> }`, threading the
   image equation as a predicate on the tree, and injecting into `t`
   exactly once, through a variable, at the very end.
3. **Named-value injection into `via` mis-sorts** — bind the call result to
   a variable, then ascribe (`let res = go ta in (res : t{...})`).
4. **Never name a map/spec after a Lean core symbol** (`repr` → collision).
5. **Specs reference `def`s, not model constructors** (`lcons` must be a
   `def`, not `.LCons`).
6. **A recursive int measure needs its recursive result let-bound**
   (`let n = go r in 1 + n`) for the postcondition to surface as a fact.
7. **Dependent call arguments must be variables** (let-bind
   `Vlist.append a b` before passing it to `Vlist.length`).

The PoC also confirmed the cost model from the sweep: an honest via module
is ~0.5–1 s, dominated by Lean startup plus a term linear in the
grind-visible model; the abstraction machinery is free.

---

## Appendix — probe/PoC sources

`scratch_probe/` (not committed to the test suite; kept as the design's
evidence): `lemlib*.{ml,mli}` + `lemclient*.ml` (cross-unit lemma A/A2),
`vlist.{ml,mli}` + `vlist_client.ml` (the PoC). All compile against
`_build/_bootinstall/bin/ocamlc.opt -vox-solver-path <lean>`.
