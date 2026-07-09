# vox: verifying SHARED mutable state — design options

Status: design study (2026-07-06). Branch `vox-sharedmut`. No compiler
change landed; this is a survey mapped onto vox's machinery, five
concrete options, and a staged recommendation.

WebSearch/WebFetch are disabled on this box; the literature is cited
from memory. Anything I am unsure of is flagged inline with "(check)".

---

## 0. TL;DR

vox today verifies only *unique* mutation, and it does so by a single
trick applied four ways: **the mode system guarantees exclusivity, so
the frame condition is free.** A mutable array is threaded `@ unique`,
so nothing else can name it and a write cannot invalidate anyone
else's fact; a borrow is `@ local unique`, so the loan cannot escape
and its prophecy resolves honestly; a separation token is `@ unique`,
so a stale token is un-presentable. In every case vox never asks "what
else points here?" because `typing/uniqueness_analysis.ml` has already
answered "nothing."

**Shared mutation is exactly the regime where that answer is no longer
"nothing."** Two live aliases to one cell; a write through one must be
visible through the other; and a caller must now *prove* that a write
to `x` did not disturb the invariant of `y`. That is the classic heap
frame problem, and the literature's answers (separation logic, dynamic
frames, regions, fractional permissions, object invariants) are all
ways to pay for the reasoning the mode system was giving vox for free.

The design space stratifies naturally along the mode lattice:

| regime | mode witness | frame cost | who reasons |
|---|---|---|---|
| exclusive owner | `@ unique` | free (threading) | mode checker — **done today** |
| shared read / frozen | `@ shared` / read-visibility | free (stable ghost) | mode checker — **cheap extension** |
| shared write | `@ aliased` writable / `@ contended` | real | needs heap/SL/invariants — **the hard case** |

My recommendation is a staged blend that never leaves the
grind-automation-first, small-TCB culture until it must: **invariant
cells** first (weeks, grind-trivial, sound under arbitrary aliasing),
then **mode-backed shared-read** (the contention/visibility ace), then
**native prophecy borrows** (Creusot parity, a quarter). Explicit-heap
and SL-in-Lean are deferred — the first hits vox's measured
"quantified frames kill E-matching" wall, the second abandons grind —
and kept only as the long-term ceiling for genuinely cyclic aliasing.

---

## 1. What EXISTS today (the machinery every option must respect)

### 1.1 Four mutation mechanisms, one exclusivity trick

**(a) `let mutable` locals — SSA versioning (Boogie/Why3 VCGen).**
`typing/vox_verify.ml:280-314` (`mut_versions`): each live mutable
binder maps to a *current logical version* — a synthetic ident, always
in scope. A read names the version; a write mints a fresh version with
a definitional equation `version = rhs` (Skolem-style, defined once).
Branch joins and loops **havoc** (`mut_havoc`, `:1074-1120`): a fresh
unconstrained version. Loop invariants are declared with
`[@vox.invariant]` and checked assert/havoc/assume/assert at the
back-edge (`:1163-1194`, and `demo/lean_mutable.ml:63-79`). A local
*cannot alias* — it has no address the source language can copy — so
this fragment never touches the frame problem at all.

**(b) Mutable arrays — McCarthy stores.** `demo/lean_reverse.ml:21-95`
is the reference. Three trusted grind axioms over an opaque `VoxU`
array:

```lean
@[grind] axiom len_upd   : len (upd a j w) = len a
@[grind] axiom elem_upd_eq : elem (upd a j w) j = w
@[grind] axiom elem_upd_ne : ¬(k = j) → elem (upd a j w) k = elem a k
```

`aset` returns the *store* (`varr{ _ = upd a j w }`), `aget`/`alen`
return the **same atom** (`varr{ _ = a }`). The array is threaded
`@ unique`, so there is exactly one name for it and the frame is free.
**The measured lesson** (`AGENTS.md`, `demo/lean_reverse.ml:16-19`):
quantified per-call frame conditions do NOT scale — grind cannot
instantiate `forall`-facts at goal indices. Loop invariants must be
stated as *prelude Props* with one hand-proved step lemma whose
variables are all bound by its conclusion, so E-matching's automatic
pattern fires and discharge is ground congruence. **This constraint is
the single most important input to the whole design.**

**(c) Borrows / slices — RustHorn/Creusot prophecies.**
`lib/borrow_lib.mli` is the canonical statement. A live loan `mut`
carries `now m` (current contents) and `fin m` (prophesied final
contents); a prophecy `proph` denotes the value it resolves to, `pv p`
(all three are `[@@vox.sort int]`, so a loan denotes its int value with
no wrapper). `borrow_mut p x k` hands the continuation a loan tied at
entry (`now _ = x && fin _ = p`), `@ local unique` so it cannot escape;
when the bracket returns, the residual `vref{ _ = p }` is honest.
`mdrop : (m) @ local unique -> unit{ fin m = now m }` resolves the
prophecy. `demo/lean_flip_proph.ml` shows reborrowing children in place
(`borrow_left`, `plugl (tnow m) (tpv p)`) — a full imperative tree flip
proved through prophecies, mutation-only dataflow. **The
implementation is TRUSTED** (`assume_unchecked_` + `Obj.magic_unique`);
the trust is confined to the small library and every client fact is
derived.

**(d) Interior mutability — Verus PCell/PointsTo separation tokens.**
`lib/pcell_lib.mli`: `icell` is a mutable cell vox never models;
`itoken` is an unforgeable, unduplicable ghost witness of its
contents. `alloc : (v) -> (icell * itoken){ tid (snd _) = cid (fst _)
&& cts (snd _) = v } @ unique`; `read`/`write` consume the token
`@ unique` and return a fresh one. **The mode checker is the borrow
checker**: a stale token cannot be presented, a token cannot be
duplicated, and no fact is ever retracted because facts speak of
immutable token *snapshots*, never of the cell. `demo/lean_pcell.ml`
swaps two cells and proves the sum.

### 1.2 The via bridge (models over mutation)

`docs/plans/2026-07-04-refines-via-design.md` §"VIA AND BORROWS"
(`:442-505`) and `lib/mset_lib.{mli,ml}` show that mutation and pure
models were *already joined by an abstraction function*: `lib/mhtbl`
mutates a `Bslice.varr` in place while `bcts : varr -> table` names the
immutable model. `via` makes that function the implicit image-binder
map and seals it — a mutable finite set (`lib/mset`) is mutated in
place through a borrow and exposed as `type t : value refines (iset)`
with **zero `assume_unchecked_` in the payoff module**; the trust is
confined to the six-function `mset_lib`. Two design rules matter here:
loans do NOT get via types (they stay opaque `VoxU` with `snow`/`sfin`/
`spv` landing directly at the image sort), and there is **one trusted
borrow library per image model**. Any shared-mutation design must
compose with this: the shared cell's *model* is what a client should
see, never its carrier or its aliasing discipline.

### 1.3 The mode system — vox's ace (inventory)

Findings from `typing/mode.ml`, `typing/uniqueness_analysis.ml`, and
`jane/doc/extensions/{_05-modes,_06-kinds,_07-uniqueness}`:

- **Uniqueness** (`Unique < Aliased`, `mode.ml:510-532`): `@ unique`
  means "no other reference exists *on this path when consumed*" — not
  a global single-owner invariant (a value may be unique on one branch,
  aliased on another; the GC frees). Enforced by a **dedicated,
  conservative, sound** post-typing pass
  (`uniqueness_analysis.ml:16-48, 456-518`) that walks the tree
  composing usages (`seq`/`par`/`choose`) and rejects a second use of a
  unique value. **Deep**: a unique value's children are unique, unless a
  field carries an `@@ aliased` modality. This is what vox trusts today.

- **Linearity** (`Many < Once`, `mode.ml:534-556`): **affine** (at most
  once), enforced by the same use-counting. Purpose: a closure
  capturing a unique value is `once`, so you cannot re-alias the unique
  value by re-invoking the closure. This is what makes the `@ once
  local` borrow continuation sound.

- **Locality** (`Global < Regional < Local`, `mode.ml:456-508`):
  `local` values cannot escape their region (function/loop body). This
  is the mechanism behind "a loan is `@ local unique` and cannot
  escape the bracket." (Caveat: locality currently conflates
  stack-escape and borrow-escape into one axis; a split is planned —
  `_07-uniqueness/borrow.md:222-244`.)

- **THE ACE — contention + visibility.** Beyond uniqueness there are two
  more past-axes that govern *read vs write access to mutable state*:
  - **Contention** (`Uncontended < {Shared, Corrupted} < Contended`,
    `mode.ml:589-618`): `Shared` = multiple readers, no writer;
    `Contended` = another party may read+write.
  - **Visibility** (`Read_write < {Read, Write} < Immutable`,
    `mode.ml:699-728`): the typechecker forbids **reading** mutable
    fields of a `write`-visibility value and **writing** mutable fields
    of a `read`-visibility value — *even single-threaded*.

  Together these give, for free, a **shared-read discipline**: an
  aliased value at `@ shared`/read-visibility can have many aliases but
  its mutable state cannot be written while shared. That is precisely
  the condition under which its ghost is *stable* and can be handed to
  every alias as an immutable fact — RustBelt's "shared references are
  persistent." vox does not exploit this today.

- **Mode crossing** (`_06-kinds/02-syntax.md:106-136`): immutable/
  immediate types cross uniqueness (safe to treat as unique even when
  aliased); ordinary boxed/abstract types do NOT — which is why
  `borrow_lib`/`pcell_lib` mark their carriers "boxed, must not
  mode-cross uniqueness."

- **Absent today**: fractional permissions; a native `&mut`-style
  exclusive *mutable-through-reference* borrow ("exclusive mutable" is
  explicitly future work, `_07-uniqueness/pitfalls.md:120-121`); a way
  to imperatively `set` a unique value in place and keep using it (the
  API must *return* the new reference).

- **Known caveat for a verifier**: there is a documented soundness hole
  in uniqueness × pattern-matching on `@@ aliased`-modality fields,
  worked around by a coding idiom (`_07-uniqueness/intro.md:155-183`).
  A vox design that leans on modalities must respect this.

---

## 2. The benchmark set

I rank every option against these six, chosen to span the difficulty
axis (exact spec vs invariant; tree vs cyclic; one writer vs many):

1. **Shared counter** — one `int ref` incremented through two aliases;
   prove the final value (exact) or `≥ 0` (invariant). The atom of
   shared *write*.
2. **Memo table** — a mutable hashtable cache behind a pure function
   `f`; prove the memoized version is observably `f`. Invariant =
   "table agrees with `f` on its domain." The 80% industrial case.
3. **Union-find** — mutable parent array with path compression; prove
   `find` returns the canonical representative and that compression
   preserves the partition. Shared mutable graph + amortization.
4. **Doubly-linked list** — each node aliased by `prev.next` and
   `next.prev`; cyclic. Prove structural invariants. The hardest:
   genuine cyclic sharing.
5. **Observer** — a subject holds a list of observer cells; `notify`
   mutates each. Callback + shared mutable + read-sharing of the
   subject.
6. **Iterator invalidation** — prove that mutating a collection
   invalidates outstanding iterators (a *safety* property you want to
   REJECT, not a functional one). Really a linearity/mode obligation.

A note that shapes the ranking: **union-find is a single array**, so it
already fits vox's McCarthy-store story *without any aliasing* (the
"parent pointers" are array indices, not ML aliases). It appears
"shared" but is not — the genuine-aliasing benchmarks are the counter,
memo table, DLL, and observer.

---

## 3. Design options

### Option 1 — MODE-BACKED OWNERSHIP (extend today's discipline)

**(a) Core idea.** Lean harder on the mode system. Keep `@ unique` for
exclusive owners (frame-free, today). Add two capabilities:
*shared-read* via the contention/visibility axes (a `@ shared` alias
exposes a **stable ghost snapshot**, assumable by every alias, no frame
because no write is possible while shared — RustBelt's persistent
shared reference), and *native prophecy borrows* promoting the
hand-written `borrow_lib` now/fin into a checker-understood `&mut`
keyed on `@ local unique`. Instantiates: RustBelt (Jung, Jourdan,
Krebbers, Dreyer — shared refs as persistent propositions), RustHorn
(Matsushita, Tsukada, Kobayashi, ESOP 2020 — prophecy soundness), and
Creusot (Denis, Jourdan, Marché — the prophecy encoding as a tool).

**(b) Surface.** Shared-read: an argument at `@ shared` (or a
read-visibility field) whose refinement is read as an immutable fact
inside the region — e.g. `observe : (s : subject) @ shared -> int{ _ =
model s }` where `model s` is frozen for the call. Borrows: exactly
today's `borrow_lib` vocabulary (`borrow_mut`, `mget`/`mset`, `mdrop`,
reborrow via plug functions), but with `borrow_lib`'s
`assume_unchecked_` casts replaced by compiler-emitted now/fin loans so
the user writes no `Obj.magic`.

**(c) Lean encoding.** now/fin/pv are opaque `VoxU → Sort` functions;
drop resolves `fin = now`; reborrow updates the parent via a plug
term. **No quantified frame axioms** — the prophecy pairs are per-loan
ground terms, and reborrow composition is ground congruence. This is
the automation-critical property, and it is *already demonstrated to
survive grind* (`flip_proph`, `mset` verify today). Shared-read adds
nothing to Lean at all: it is a *mode-gated assume* of the existing
refinement, emitted by the compiler when the mode is `shared`.

**(d) Soundness / TCB.** The mode checker
(`uniqueness_analysis.ml`) is trusted to deliver exclusivity for
`unique` and no-writer for `shared`. Making borrows native *moves* the
now/fin trust from a hand-written library into the compiler — same
trust, better ergonomics — and is the chance to discharge it *once*
against RustHorn's metatheory instead of per-library `assume_unchecked_`
(a TCB *reduction* relative to today). Shared-read adds the contention/
visibility axes to the TCB (trust that `shared` truly precludes a
concurrent or aliased writer — a real assumption given the documented
modality×match caveat).

**(e) Unlocks (benchmarks).** Shared counter *with one writer at a
time* (borrow it exclusively to increment). Observer *reads* (subject
at `@ shared`). `&mut`-through-structure: tree flip, in-place list
reverse, mutable-set insert (all proved today via the library).
**Does NOT unlock**: genuinely cyclic aliasing (DLL), simultaneous
shared *writers* (a truly aliased writable counter), memo tables where
the cache is aliased and written (the writer is not exclusive).

**(f) Incremental path.** Shared-read fact: ~1–2 weeks (a mode-gated
assume in `vox_verify.ml`, plus a demo and a fail-closed test that a
`@ shared` value's mutable state cannot be written and relied upon).
Native `&mut`: ~a quarter (borrow elaboration in `typecore`/
`vox_verify`, reborrow linking, the once-and-for-all soundness note).

**(g) Composition.** Composes cleanly with `via` (mset already proves
mutation-behind-a-model with borrows) and with `[@@vox.lemma]`. It *is*
the mode system, so it is maximally aligned with vox's architecture.

---

### Option 2 — EXPLICIT HEAP (Dafny/Boogie; globalize the array story)

**(a) Core idea.** One global McCarthy heap per mutable type (or a
polymorphic `Heap : Loc → Val`). A mutable cell is an opaque `Loc`;
reads are `sel h loc`, writes return `upd h loc v`. Aliasing is now
*expressible* — two locs may be equal or provably distinct — and frame
conditions become disjointness facts and modifies-sets. Instantiates:
Boogie's heap (Barnett, Leino et al.), Dafny's dynamic frames (Leino —
`reads`/`modifies` clauses, `Repr` ghost sets), and Why3's
region-typed alias control (Filliâtre, Paskevich).

**(b) Surface.** `let r = ref 0` yields a `loc`; `!r`/`r := v` thread
an ambient heap ghost, like `varr` threads its store. Mutating
functions take and return the heap implicitly (a heap-monadic
contract) or explicitly; a `modifies {r; s}` clause bounds the
footprint.

**(c) Lean encoding.** `Heap`, `sel`, `upd`, and the *same three
McCarthy axioms* — but globalized. **The killer is the frame.** When
`f` modifies footprint `S`, a caller must know `∀ ℓ ∉ S, sel h' ℓ =
sel h ℓ`. That is a **quantified frame axiom instantiated at goal
locations** — precisely the pattern `AGENTS.md` measured as fatal to
grind's E-matching. The escapes are (i) modifies-clauses encoded as
*ground* equalities per statically-known touched loc (works only for
closed, finite footprints), or (ii) Dafny-style `Repr` ghost sets,
which push the quantifier into set membership — still quantified.
Freshness of `alloc` is itself a quantified axiom (`∀ live ℓ, ℓ ≠
fresh`).

**(d) Soundness / TCB.** Larger than Option 1: the global heap must
faithfully model OCaml's heap, and the alloc/freshness + frame axioms
are load-bearing and quantified (harder to trust and to automate).

**(e) Unlocks.** In principle *everything* — counter (aliases share a
loc), memo table, DLL (cyclic loc→val is fine), observer. In practice,
only where the footprint is statically closed so frames stay ground;
open-footprint frames stall.

**(f) Incremental path.** A quarter-plus for the encoding; the real
risk is that making automation survive forces the same "prelude-Prop
invariant + ground step lemma" discipline as arrays, *per data
structure* — i.e. it does not actually buy general automation, it buys
expressiveness at a persistent grind tax.

**(g) Composition.** The heap threads like `varr`; `via` can seal a
heap-backed model. But the quantified frame is a standing conflict with
vox's automation culture.

---

### Option 3 — SL-IN-LEAN (embedded separation logic; CFML / Iris-lite)

**(a) Core idea.** Embed a separation logic in the Lean prelude: heaps
as finite partial maps, `⋆` as disjoint-union `Prop`, points-to `ℓ ↦
v`, the frame rule. VCs become SL entailments `P ⊢ Q ⋆ ?frame`.
Instantiates: CFML / characteristic formulae (Charguéraud — an
OCaml-flavored embedding in Coq), Iris (Jung, Krebbers, Dreyer et al. —
resource algebras, invariants, later modality, prophecies), RefinedC
(Sammler, Lepigre, Krebbers — refinement + ownership, automated by the
**Lithium** goal-directed SL solver), and Steel/Low*/F* (Swamy,
Fromherz, Martínez — SteelCore concurrent SL, SMT-backed).

**(b) Surface.** Contracts with `⋆` and `↦`; Hoare triples `{ pre } e
{ r. post }`. Maximally expressive.

**(c) Lean encoding.** The SL is *defined*, not axiomatized. The crux:
grind/E-matching **cannot** do frame inference or entailment with an
existential frame `?frame` — SL entailment is undecidable in general
and needs a dedicated proof search (Lithium in RefinedC, `xsimpl`/
`xpull` in CFML, the Iris Proof Mode in Iris). **grind is
insufficient**; vox would need to build or port an SL tactic layer in
Lean. (There is an early `iris-lean` effort — check maturity; I would
not assume it is production-ready.)

**(d) Soundness / TCB.** *Smallest* TCB of all options — the SL is
defined and its soundness proved once against a heap model, then
everything is checked. The most honest option. But it trades TCB for
automation: it abandons grind for a bespoke tactic.

**(e) Unlocks.** Everything, including cyclic aliasing (DLL), union-find
exact specs, observer, and — via Iris — eventual concurrency. The only
option that reaches the whole benchmark set.

**(f) Incremental path.** Multiple quarters; building an SL tactic in
Lean is a research project. This is the long-term *ceiling*, not a
near-term milestone.

**(g) Composition.** Subsumes borrows and `via` as special cases, but
requires rewriting them and retraining the automation story.

---

### Option 4 — PERMISSION REFINEMENTS (fractional permissions as ghost args)

**(a) Core idea.** Fractional permissions `p ∈ (0,1]` as
refinement-level ghost values threaded like the PCell token: `p = 1` →
write, `0 < p < 1` → read; `split`/`join` divide and recombine.
Generalizes vox's *linear* PCell token from a whole permission to a
fractional one, so a cell can be shared for reading (each alias holds a
fraction) and re-unified for writing. Instantiates: Boyland (fractional
permissions), Viper's implicit dynamic frames (`acc(x.f, p)` — Smans,
Jacobs, Piessens), Chalice, VeriFast fractionals.

**(b) Surface.** Extend `pcell_lib`: `read` needs any fraction, `write`
needs the full fraction; `split : token{ frac _ = 1 } -> (token{ frac _
= half } * token{ frac _ = half })` and its inverse `join`. The
fraction is a ghost refinement (rational, or a `Frac` sort).

**(c) Lean encoding.** Permission arithmetic (linear rationals — grind
handles this) plus the existing PointsTo `cts`/`tid`. **The frame stays
free**, because the token is still *threaded* (unique on the token);
fractions only relax *duplication*. The split/join laws are ground
equations. **Automation survives** — this is the key attraction over
Options 2/3: fractions keep the "threaded token, no quantified frame"
property while adding read-sharing.

**(d) Soundness / TCB.** Same shape as PCell — one trusted library —
plus the invariant "sum of live fractions ≤ 1," which the mode
system's linearity on tokens must enforce. Small TCB delta.

**(e) Unlocks.** Readers-then-writer sharing; reclaimable immutable
snapshots. Does **not** solve aliasing *topology* (DLL, cycles) or
simultaneous shared *writers*. Overlaps heavily with Option 1's
shared-read — fractions are one concrete *implementation* of it.

**(f) Incremental path.** ~2–4 weeks as a library (like `pcell_lib`),
no compiler change.

**(g) Composition.** Composes with tokens and `via`; the fraction is
just another ghost refinement.

---

### Option 5 — INVARIANT CELLS (monitor/object invariants)

**(a) Core idea.** A shared mutable cell carries a **declared
invariant** `I(v)` over its contents. Reads ASSUME `I`; writes must
RE-ESTABLISH `I`. **No aliasing reasoning at all** — `I` is
self-framing over the cell's own contents and is the *only* thing any
alias may assume, so any number of aliases may coexist because they all
agree on the one stable fact `I`. Instantiates: Hoare monitor
invariants; the Spec#/Boogie object-invariant methodology (Barnett,
DeLine, Fähndrich, Leino, Schulte — minus pack/unpack, since we drop
cross-object invariants); Viper abstract predicates in their simplest
form.

**(b) Surface.**

```ocaml
type counter = { mutable n : int } [@@vox.cell_invariant (fun n -> n >= 0)]
val get  : counter -> int{ _ >= 0 }      (* assumes I *)
val incr : counter -> unit               (* must prove I(n+1) from I(n) *)
```

Aliases freely; every read gets `I`.

**(c) Lean encoding.** `I` is a Lean predicate. A read emits `assume
I(v)`; a write emits `assert I(v')`. **No heap, no frame, no
quantifiers** — the invariant makes the cell's abstract state
irrelevant beyond `I`, so each VC is a *ground* implication `I(v) →
I(f v)`. This is the **most grind-friendly** option of all five.

**(d) Soundness / TCB.** The argument: because reads only ever learn
`I` (never the exact contents) and writes re-establish `I`, no alias
can observe a state violating `I`, *regardless of interleaving or
aliasing*. It is therefore **sound under arbitrary aliasing and even
concurrency** for the invariant fragment, because `I` is stable. Trust
that every write goes through the checked `set` (no raw field mutation
escaping vox — enforceable by requiring the field private/abstract).
Tiny TCB delta.

**(e) Unlocks.** Memo table (`I` = "table agrees with `f` on its
domain"), counter (`I` = "`n ≥ 0`" or a monotonicity invariant), caches,
pools, connection managers — **any shared cell whose useful spec is an
invariant rather than an exact value.** Does **not** unlock exact
functional specs of shared state (cannot prove "final counter = 42"
through aliases, only "counter ≥ 0"), nor DLL structural exactness, nor
union-find canonical-rep exactness. Weakest, but hits the task's own
"80% case."

**(f) Incremental path.** ~1–2 weeks: a `[@@vox.cell_invariant]`
attribute, read-assume/write-assert emission in `vox_verify.ml`, a
demo (bounded/monotone counter), a memo-table demo, and a fail-closed
test (a write that breaks `I` is rejected at the VC).

**(g) Composition.** Orthogonal to everything; the invariant may be
stated at a `via` model. **Combines with Option 1**: `unique` gives
exact specs where you own the cell, invariant-cells give the shared
fallback where you don't.

---

## 4. Options at a glance

| # | option | core lit | unlocks | automation (grind?) | TCB delta | effort |
|---|---|---|---|---|---|---|
| 1 | mode-backed ownership | RustBelt / RustHorn / Creusot | shared-read, `&mut`-through-structure, observer reads | **survives** (ground now/fin; no frame) | *reduces* (borrows native vs library); +contention axis | shared-read ~2wk; `&mut` ~1qtr |
| 2 | explicit heap | Dafny / Boogie / Why3 regions | counter, memo, DLL, observer (closed footprints) | **hostile** (quantified frames) | large (global heap, freshness, frame axioms) | 1qtr+ |
| 3 | SL-in-Lean | CFML / Iris / RefinedC / Steel | **everything** incl. cycles + concurrency | **no** (needs bespoke SL tactic) | smallest (SL defined) | multi-qtr |
| 4 | fractional permissions | Boyland / Viper IDF / Chalice | readers-then-writer, reclaimable snapshots | **survives** (threaded token, linear frac arith) | small (one lib + Σfrac≤1) | ~2–4wk (library) |
| 5 | invariant cells | Spec#/Boogie / monitors / Viper predicates | memo, counter, caches (invariant specs) | **best** (ground `I(v)→I(f v)`) | tiny | ~1–2wk |

---

## 5. Creusot's `&mut` vs vox's now/fin — the correspondence

Creusot models `&mut T` as a pair `(*x, ^x)` = (current value,
**prophesied final** value). `^x` is a logical variable resolved to the
borrow's contents at the point the borrow dies; a function
`fn f(x: &mut T)` gets a contract over `*x` and `^x`, mutation updates
`*x`, and the postcondition constrains `^x`. This is the RustHorn
translation (Matsushita, Tsukada, Kobayashi, ESOP 2020), proved sound
and complete for the borrow-typed fragment of Rust.

vox's `borrow_lib` is a line-for-line match:

| Creusot | vox (`borrow_lib`) |
|---|---|
| `*x` (current) | `now m` |
| `^x` (prophesied final) | `fin m` |
| implicit prophecy `^` | reified `proph`, `pv p`, consumed `@ unique` |
| resolution at borrow end (`^x = *x`) | `mdrop : … unit{ fin m = now m }` |
| reborrow `let y = &mut *x` links `^y` into `*x` | `borrow_left`, `plugl (now m) (pv p)` |
| Rust borrow checker (NLL) gives exclusivity | `uniqueness_analysis.ml` gives exclusivity |

**Differences that matter:**

1. **Explicit vs inferred resolution.** Creusot infers *where* `^x`
   resolves from lexical borrow scopes; vox makes it explicit — the
   user calls `mdrop`, and `@ local unique` + `@ once` on the
   continuation encode the lexical scope. vox's `proph` token *reifies*
   Creusot's implicit `^`, and consuming it `@ unique` is how vox gets
   Creusot's single-resolution guarantee (two resolutions of one `p`
   could prove `False`).

2. **Derived vs axiomatic soundness — the TCB gap.** Creusot's
   prophecy encoding is *derived* from RustHorn's metatheory (proved
   once). vox's now/fin is *assumed* via `assume_unchecked_` inside
   each trusted borrow library. So vox's borrows currently have a
   **larger TCB than Creusot's** — and Option 1's "native `&mut`"
   milestone is exactly the opportunity to discharge the now/fin cast
   once against a RustHorn-style argument, shrinking vox toward
   Creusot's footing.

3. **Same boundary, same open problem.** Creusot is complete only for
   the prophecy-structured (tree-shaped) fragment and *punts on genuine
   shared mutation* — it forces `Cell`/`ghost`/`raw pointer` escape
   hatches for aliased writable state. vox inherits this boundary
   exactly. **Shared mutation is the open problem for both**, which is
   why Option 1 cannot be the whole answer and Options 3/5 exist.

---

## 6. Recommendation

vox's culture is grind-automation-first, honest small TCB, and
mode-system leverage. Against that:

- **Option 3 (SL-in-Lean)** is the honest long-term ceiling and the
  only path to unrestricted cyclic aliasing + concurrency, but it
  abandons grind for a bespoke SL tactic — a multi-quarter research
  bet. Keep as **north star**, not a near-term milestone.
- **Option 2 (explicit heap)** hits the exact wall vox already measured
  (quantified frames kill E-matching). Reserve it for the genuinely
  cyclic residue (DLL) where nothing cheaper works, and expect a
  per-structure invariant+step-lemma tax.
- **Options 1, 4, 5** all keep automation and a small TCB.

**Staged blend (recommended):**

- **Milestone 1 (weeks) — Option 5, invariant cells.** Best bang per
  unit effort: grind-trivial (ground `I(v) → I(f v)`), sound under
  arbitrary aliasing, tiny TCB, and it unlocks the industrial 80% —
  memo tables, bounded/monotone counters, caches. Ships first.

- **Milestone 2 (weeks) — Option 1 shared-read.** Exploit the
  contention/visibility mode ace: a `@ shared` alias exposes a stable
  ghost snapshot, giving *exact* specs for read-sharing (observer
  reads, immutable-after-init config). Pairs directly with invariant
  cells (invariant for writable-shared, exact snapshot for
  read-shared). Small compiler change (a mode-gated assume).

- **Milestone 3 (a quarter) — Option 1 native `&mut`.** Promote the
  hand-written now/fin `borrow_lib` into checker-native prophecy
  borrows (Creusot parity), discharging the `assume_unchecked_` casts
  once against a RustHorn-style argument. This is the *exact functional
  spec* story for tree-shaped exclusive mutation, and it shrinks the
  TCB rather than growing it.

- **Option 4 (fractional)** slots in as the *implementation* of
  shared-read if a fraction-based account proves cleaner than the
  mode-based one — keep it as a parallel library experiment, no
  compiler change, decide empirically.

- **Options 2 / 3** deferred, with the trigger recorded: reach for
  explicit-heap only when a genuinely cyclic benchmark (DLL) has no
  cheaper account, and for SL-in-Lean only if vox commits to being a
  general concurrent program logic.

### First concrete milestone

**Invariant cells (`[@@vox.cell_invariant`).** Deliverables:

1. Attribute parse + a compiler pass emitting, per cell type, `assume
   I(v)` at a read and `assert I(v')` at a write (in `vox_verify.ml`,
   alongside the existing record/field handling near `:1396-1435`).
2. Require the mutable field private/abstract so no raw mutation
   escapes vox (the whole soundness hinge).
3. Demos: a monotone/bounded counter (`I = n ≥ 0`) shared through two
   aliases; a memo table whose `I` says "cache agrees with the pure
   spec `f`," yielding an observably-pure memoized `f`.
4. A fail-closed test (`mechanics/`): a `set` that breaks `I` is
   rejected at the VC, at the intended layer (contract VC, not
   elaboration) — per AGENTS.md's "eyeball the rejection layer" rule.

No new sort, no heap, no quantifiers — pure grind, in the culture,
shipping the 80% in the first increment.

---

## 7. A probe worth running before Milestone 1 (not yet run)

The one place the options *decisively* differ is whether a shared cell
needs a heap at all. A ~1-day probe would encode the **memo table**
both ways on the built compiler
(`_build/_bootinstall/bin/ocamlc.opt -vox-solver-path <lean> -c`):
(a) as an *invariant cell* (Option 5) — expect a ground VC, sub-second;
(b) as an *explicit-heap* cell with a `modifies` frame (Option 2) —
expect the quantified-frame stall AGENTS.md predicts. If (a) verifies
and (b) stalls, that is the empirical case for leading with Option 5
and deferring Option 2, and it costs one file. I recommend running it
as the first task of the implementation phase rather than as part of
this survey (the survey's conclusion does not hinge on it — the
array-store measurement already stands in for (b)).
