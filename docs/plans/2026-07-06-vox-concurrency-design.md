# Concurrency with atomics in vox: a design-options study

**Status:** design only, no implementation. Feasibility ladder + recommendation.
**Author:** vox-conc quest, 2026-07-06.
**Sibling dependency (resolved):** the shared-mutation-semantics quest landed at
`/usr/local/home/jujacobs/oxcamls/vox-sharedmut/docs/plans/2026-07-06-vox-shared-mutation-design.md`
(commit `b152bb802`). Its recommended staged blend — **M1** invariant cells
(`[@@vox.cell_invariant]`), **M2** mode-backed shared-read (`@ shared` stable
ghosts), **M3** native `&mut` prophecy borrows; explicit-heap and SL-in-Lean
deferred — is the concrete substrate this doc now conditions on (see
[§0](#0-conditioning-on-the-substrate-m1m2m3)). The convergence is strong:
sharedmut's M1 *is* this doc's Option A with a plain aliased carrier instead of
an `Atomic.t`, and sharedmut already argues M1 is sound *under concurrency*.
Effects/schedulers are the exceptions quest's; borrows are the borrowing
quest's; this doc uses their vocabulary but does not redesign their layers.

---

## TL;DR

- **vox is not as sequential as the future-work list implies.** A verified
  *parallel* quicksort already exists in the tree
  (`testsuite/tests/vox/demo/lean_qsort.ml`, `psort`): `Par_lib.fork_join2`
  runs the two recursive sorts on disjoint slice-borrows, on separate domains,
  and the whole thing verifies with the *same* spec as the sequential version.
  This is **Level 0** of the ladder and it works today. The genuine gap is
  **shared (contended) mutable state** — atomics, locks, lock-free structures.
- **The lever is LDRF-SC + OxCaml's mode system.** OCaml 5's memory model gives
  sequential consistency to data-race-free programs (Dolan et al., *Bounding
  Data Races in Space and Time*, PLDI 2018). OxCaml's mode system
  **statically** guarantees data-race freedom. Compose the two and vox may
  soundly keep its **entirely sequential** Lean/grind model as the meaning of a
  well-moded concurrent program. vox never touches relaxed memory.
- **The mode system does the hard part for free.** `uncontended` state is
  thread-local ⇒ vox's existing sequential now/fin/McCarthy semantics is
  *exact* with zero new obligations. `contended` mutable access is a compile
  *error* unless it goes through `Atomic.t` or a lock — so the concurrency
  proof surface is a *small, syntactically identifiable* set of primitive
  calls.
- **Recommended MVP (≈1 month):** document Level 0 as the canonical concurrency
  story, and add **Option A — atomic invariant cells** (a trusted `atomic_lib`
  in the style of `slice_lib`) whose only spec is a single-state invariant `I`
  preserved by every atomic op. This is grind-shaped: each obligation is a
  sequential "assume `I(old)`, prove `I(new)`" contract. Everything else stays
  `uncontended` and reuses vox unchanged.
- **Explicitly out of vox's deductive reach:** full *linearizability* / logical
  atomicity (Iris atomic triples, view shifts, ghost updates). That proof style
  is interactive and step-indexed, **not** grind-shaped. A restricted "assumed-
  atomic CAS, per-attempt functional correctness" story is offered for single-
  word lock-free structures; genuine linearizability is a year+/research item
  and arguably belongs in a different tool.

---

## 0. Conditioning on the substrate (M1/M2/M3)

sharedmut chose a *staged blend* on the mode lattice, deferring explicit-heap
and SL-in-Lean. This doc's concurrency options map onto that blend
**one-to-one** — concurrency is not a separate substrate, it is the *same*
milestones with the mode axes turned up from aliasing to cross-domain sharing:

| sharedmut milestone | mechanism | this doc's concurrency counterpart |
|---|---|---|
| **M1** invariant cells (`[@@vox.cell_invariant]`) | read assumes `I`, write re-establishes `I`; ground `I(v) → I(v')` VC; sound under arbitrary aliasing *and concurrency* | **Option A** verbatim — put the invariant cell's carrier behind an `Atomic.t` (so writes are also cross-domain safe) and Option A *is* M1. The Lean encoding is identical (opaque contents, `I` re-established per op). |
| **M2** mode-backed shared-read (`@ shared` ⇒ stable ghost snapshot) | contention/visibility axes: no writer while shared ⇒ the ghost is a persistent fact | The **read side** of Option B and the `shared` contention tier (§1). A `@ shared` alias across domains (read/write lock's read side, or immutable-after-init config) exposes an *exact* snapshot, not just an invariant. |
| **M3** native `&mut` prophecy borrows (Creusot parity) | promote hand-written `borrow_lib` now/fin into checker-native loans | The substrate under **Level 0** — `par_lib.fork_join2` already forks over `borrow_lib`/`slice_lib` loans. When M3 makes borrows native, Level 0's disjoint-fork/join becomes native too, `Obj.magic`-free. |
| *(deferred)* explicit-heap, SL-in-Lean | — | Only Option **C-full** (linearizability) would need SL. See the honest note in §6. |

**Consequence for staging.** Concurrency does *not* need its own substrate
decision: it rides M1/M2/M3. In particular **Option A costs almost nothing on
top of M1** — the *only* delta is an `Atomic.t` carrier (so a write is safe not
just under aliasing but under genuine parallel writers) plus the LDRF-SC axiom
(§2) that says the atomic op is SC. sharedmut's M1 soundness argument ("reads
only ever learn `I`, writes re-establish `I`, so no alias observes a state
violating `I`, *regardless of interleaving or aliasing*", §3.5(d) of that doc)
is *already* the concurrency soundness argument. This is the single biggest
finding of the reconciliation: **the concurrency MVP is a one-line extension of
the shared-mutation MVP.**

---

## 1. Ground truth: the mode-system inventory

Read from `jane/doc/extensions/_05-modes/{intro,reference}.md`,
`_04-parallelism/{01-intro,02-capsules}.md`, and
`_01-tutorials/01-intro-to-parallelism-part-1.md`. What each axis *actually
enforces*, and what it therefore *discharges* for vox:

| Mode axis (kind) | What the compiler enforces | What it discharges for vox |
|---|---|---|
| **contended** (past) — `uncontended` / `shared` / `corrupted` / `contended` | Rule 2: you may not read or write an *unprotected* `mutable` field / array element of a `contended` value (`shared` = read-only ok; `corrupted` = write-only). `Atomic.t` fields are exempt (protected). | The **only** places a concurrency obligation can arise are protected ops. Unprotected mutable access to shared state does not compile — vox never sees it. |
| **contended** — `uncontended` specifically | Rule 1 (a whole-system invariant, backed by `fork_join`/`Domain`/capsule signatures): at most one domain may hold a value `uncontended`. | `uncontended` ⇒ **thread-local**. vox's existing sequential now/fin/McCarthy semantics is *exact* for uncontended state — **zero** new obligations. This is ~100% of vox today, and all of it stays sound under multicore. |
| **portability** (future) — `portable` / `shareable` / `corruptible` / `nonportable` | A closure crossing to another domain must be `portable`: rule 2 forces it to treat everything it captures as `portable` + `contended`. `fork_join2` / `Domain.spawn` require portable tasks. | The proof that "this closure is safe to ship to another domain" is the *checker's*, not vox's. vox does not re-derive it. |
| **shared** (contention mid-tier) | Read but not write the mutable parts (read/write locks). | A middle tier: vox may read a value that can change under it ⇒ must reason with *monotone / invariant* facts, never equality-to-a-snapshot. |
| **mode crossing** (`immutable_data`, `mutable_data`) | Deeply-immutable data *crosses* contention & portability; `mutable_data` crosses portability. | Pure functional specs (models, via images, `iarray`s) are `immutable_data` ⇒ they cross freely and need **no** concurrency treatment. vox's spec language is already in the exempt fragment. |
| **capsule brand** `'k` (a phantom type, not a mode) | Statically names *which capsule/lock* owns a piece of state; `Access`/`Password`/`Key`/`Mutex` gate entry (contention / locality / uniqueness resp.). | A static handle vox can read to know *which invariant governs which state*. |

**The single most important finding:** the concurrency obligation surface is
*tiny and syntactic*. Everything that isn't an `Atomic.t` op or a lock
acquisition is either thread-local (`uncontended`, exact) or immutable
(`immutable_data`, crosses). vox does not need a whole-program concurrent
semantics; it needs contracts for a handful of primitives.

---

## 2. The lever: LDRF-SC as an axiom of the encoding

The OCaml 5 memory model (Dolan, Sivaramakrishnan, et al., PLDI 2018) is a
*local* data-race-freedom theorem: programs whose racy accesses are all through
atomics enjoy **sequential consistency** — every execution is an *interleaving*
(not a *reordering*) of the domains' actions; racy non-atomic accesses get only
a *bounded* "catch-fire" (values are drawn from a bounded set of prior writes,
never out-of-thin-air; far weaker than C/C++ UB).

OxCaml's mode system makes the antecedent *statically checked*: a well-moded
program has no non-atomic data races (contention rules 1+2, portability). So:

> **Axiom (LDRF-SC-for-vox).** A program that type- and mode-checks in OxCaml
> has a sequentially consistent, *interleaving* semantics. Every `Atomic.t`
> denotes a single memory cell whose ops are atomic and SC; every lock critical
> section executes with exclusive (`uncontended`) access to the state it
> guards.

Under this axiom, **vox reasons in exactly one memory: the SC interleaving
one.** Concretely:

- **Unprotected shared state does not exist** in the safe fragment (rule 2), so
  there is nothing to reason about with weak memory.
- **`Atomic.t`** is the *only* concurrent primitive, and it is SC. Model it as a
  single ghost cell (a one-slot McCarthy store) with atomic read/write/CAS.
- **Locks** provide exclusion, so the critical section is *literally
  single-threaded* — vox's existing sequential machinery is exact inside it.

**Why relaxed memory is out of scope v1 — and it's not a punt.** ORC11 /
RustBelt-relaxed / GPS-for-release-acquire exist to reason about programs that
*deliberately* use relaxed/release-acquire atomics for performance. OxCaml's
safe `Atomic` is **SC**; the mode-checked safe fragment has *no* relaxed atomic
and *no* racy non-atomic. There is simply no relaxed behavior for vox to see.
Reasoning about weak memory would only be needed if vox descended below the safe
fragment (raw `Atomic` with explicit fences, `Obj.magic`), which is out of
scope. LDRF-SC is not a simplifying assumption we impose — it is the *actual
semantics* of the fragment vox verifies.

**Latent synergy worth recording.** vox already has **prophecy variables**
(`pv p`, `now`/`fin`) from its RustHorn borrow encoding. Prophecy is *exactly*
the device Iris uses for *future-dependent linearization points* in
logical-atomicity proofs (Jung et al., "The Future is Ours: Prophecy Variables
in Separation Logic", POPL 2020). vox is missing the *other* half (shared
invariants + ghost updates), so this does not make linearizability free — but if
anyone ever pushes toward it, half the vocabulary is already in the language.

---

## 3. The options ladder

Ordered easiest → hardest. Level 0 exists; A is the MVP; B is the natural
second step; C splits into a tractable restriction and an out-of-reach full
version; D is an orthogonal complement.

| # | Option | Spec strength | grind-shaped? | Mode system discharges | Lands in |
|---|---|---|---|---|---|
| **0** | Disjoint fork/join over borrows (`par_lib`) | Same as sequential | Yes (it's the sequential proof) | Disjointness + DRF entirely | **Exists today** |
| **A** | Atomic invariant cell | Single-state invariant `I` (+ monotone lower bound) | Yes — per-op contract | That the cell is the only shared access; SC of the op | ≈1 month |
| **B** | Lock/capsule-protected resource invariant `J` | `J` at every release; client sees consequences of `J` | Yes — critical section is sequential | Mutual exclusion (only one domain in the section) | ≈1 quarter |
| **C-restricted** | SC-ghost-cell CAS loop, per-attempt functional correctness | "if it returns, the cell went old↦new for the old it CAS'd" | Yes — assume CAS contract, prove pure loop step | Atomicity of the single word (assumed) | ≈1 quarter |
| **C-full** | Linearizability / logical atomicity (Treiber, MS-queue) | Composable atomic triples | **No** — view shifts / ghost updates are interactive | (would still need shared-invariant ghost state vox lacks) | year+ / research / different tool |
| **D** | Model-checking complement (DPOR/DSCheck-style) | Bug-finding only (unsound) | N/A (not deductive) | N/A | opportunistic |

---

### Level 0 — Disjoint fork/join over borrows *(exists today)*

**(a) Idea + literature.** Structured fork/join where the two tasks own
*disjoint* pieces of mutable state. This is the concurrent-separation-logic
disjointness rule (O'Hearn, *Resources, Concurrency and Local Reasoning*, 2004)
in its simplest form: `{P1} c1 {Q1}  {P2} c2 {Q2}` with `P1 * P2` separate ⇒
parallel composition is sound with no interference reasoning. In vox, "separate"
is *the mode checker's job*, not a proof obligation: each task captures one
`@ unique` loan, and the same loan cannot be captured twice.

**(b) Surface syntax (already in the tree, `lean_qsort.ml`).**
```ocaml
val fork_join2 :                              (* par_lib.mli, TRUSTED *)
  (unit -> 'a @ unique) @ once local ->
  (unit -> 'b @ unique) @ once local ->
  ('a * 'b) @ unique

(* psort: the two outer sub-loans a, c of a split are disjoint,       *)
(* so each once-closure consumes one and fork_join2 runs them on      *)
(* separate domains; each task's result is its side's conclusion.     *)
Par_lib.fork_join2
  (fun () -> (psort a : unit{ sorted (pv p1) && perm (take k (now m2)) (pv p1) }))
  (fun () -> (psort c : unit{ sorted (pv p3) && perm (drop (k+1) (now m2)) (pv p3) }))
```

**(c) Proof obligations + Lean + grind.** *None beyond the sequential proof.*
Each task's VC is verified exactly as the sequential `qsort`. There is no
interference to encode: the tasks touch disjoint loans, so no cross-task fact is
needed. grind handles it because it *is* the sequential proof.

**(d) Mode system discharges.** *Everything* about concurrency. Disjointness =
`@ unique` (a loan can't be captured twice). Region soundness = `once local`
closures + structured join (both complete before the region returns). Safety of
shipping the closure = the trusted `unsafe_globalize_task` cast, justified by the
signature.

**(e) TCB delta.** `par_lib`'s `unsafe_globalize_task` (`%identity`) +
`Obj.magic_unique` on the joined pair. Already trusted; one small library; the
*signature is the spec*. No memory-model axiom beyond "structured join with
disjoint unique loans is DRF" — which is a direct consequence of the mode
discipline.

**(f) Benchmark ladder position.** Handles *divide-and-conquer over disjoint
mutable regions* (parallel sort, parallel array map/reduce, tree fold). Tops out
the moment two tasks must touch the *same* location — that's Options A/B.

**(g) Staging.** Done. The one-month action is to **document** it as the
canonical vox concurrency demo and add a couple more disjoint-fork/join examples
(parallel map, parallel reduce) so users see the pattern.

---

### Option A — Atomic invariant cell *(the MVP)*

**(a) Idea + literature.** An `Atomic.t` holding a value that always satisfies a
fixed *invariant* `I`. This is the Iris **invariant** in its most restricted,
non-updating form (Jung et al., Iris, JFP 2018), and the atomic-points-to
resource of RustBelt. Because the cell is `contended`, the *only* thing any
observer can rely on is `I` — never the current *value* (another domain may have
changed it between your read and your use). So the spec of the *type* is `I`,
and the spec of every op is "preserves `I`". A useful strengthening for counters
is a **monotone lower bound**: a ghost that only ever grows, so a reader can
conclude `value ≥ b` for a `b` it observed (this is the monotone-counter / GPS
protocol pattern, and the "auth-nat with ≤" of Iris, in miniature).

**(b) Surface syntax.** A trusted `atomic_lib` (shape of `slice_lib`): the
invariant is a parameter of the cell's abstract type; ops carry it.
```ocaml
(* atomic_lib.mli — TRUSTED reflection of Atomic.t, one invariant I *)
type 'a cell                       (* an Atomic.t whose contents satisfy I *)

(* an atomic counter that never goes negative and only increases *)
val make   : (v:int{ 0 <= _ }) -> int cell @ portable
val get    : int cell -> int{ 0 <= _ }                 @@ portable
val incr   : int cell -> unit                          @@ portable
  (* obligation on the impl: if 0 <= n then 0 <= n+1 — trivial     *)
val get_lb : int cell -> (b:int){ 0 <= _ }             @@ portable
  (* monotone lower bound: a later get returns >= b (ghost history) *)
```
Publication idiom (the `price_of_gold` bug, now provable because `Atomic` is SC):
```ocaml
(* domain A publishes; domain B either sees the init flag unset, or   *)
(* sees it set AND — by SC — every write sequenced before it. The     *)
(* invariant ties the flag to the payload: flag=true ==> price>0.     *)
type published = { price : float; ready : bool Atomic.t }   (* immutable_data *)
(* I(s) := Atomic.get s.ready = true ==> s.price > 0.0                 *)
```

**(c) Proof obligations + Lean + grind.** Each op is a **sequential contract**:
- `make`: prove `I(v)`.
- `set`/`update`/`incr`: assuming `I(old)`, prove `I(new)`.
- `get`: no obligation; *returns* a value known to satisfy `I`.
- `compare_exchange`: on success, prove `I(new)` from `I(old)`; on failure,
  nothing changes.

Lean encoding: model the cell as an opaque ghost `contents : VoxU -> A` with a
`public opaque` invariant `I : A -> Prop`; the op axioms in the trusted block
say exactly the contracts above (`get` returns `{ I _ }`; `set` requires
`{ I new }`). No interleaving quantifier: because `I` is a **single-state**
predicate and every op is SC-atomic, "preserved by every op" ⇒ "holds in every
reachable state" needs *no* induction over interleavings — it's the standard
invariant-establishment argument, and each step is one grind-sized VC.
**Verdict: grind-shaped.** This is the whole reason A is the MVP.

**(d) Mode system discharges.** That the `Atomic.t` is the *only* shared access
to the cell (rule 2 forbids any other mutable access to the enclosing
`contended` value). That the cell can be freely shared to any domain (`Atomic.t`
crosses contention when the payload is `immutable_data`). vox proves only
invariant preservation; the checker proves there is no *other* racing access.

**(e) TCB delta.** One trusted `atomic_lib` (like `slice_lib`): the op axioms
are `assume_unchecked_` asserting that the ghost `contents` tracks the real
`Atomic.t` and that ops are atomic + SC. Plus the LDRF-SC axiom (§2). That's it.
The monotone-lower-bound variant adds a ghost `history` (a monotone `int`) — one
more trusted axiom. Small and honest.

**(f) Benchmark ladder position.** Nails **atomic counter monotonicity** and
**SC publication** (message passing / the `price_of_gold` idiom). *Tops out* at
anything that needs to count *contributions* — e.g. "the final counter equals
the number of incrementers" is a linearizability/counting spec needing
fractional/auth ghost contributions (Iris `auth (nat, +)`), which is out.
Single-state invariants and monotone bounds are the ceiling.

**(g) Staging.** ≈1 month: `atomic_lib` + demos (monotone counter; SC
publication). This is precisely the team lead's guess — "invariant cells over
`Atomic.t` with mode-checked non-sharing elsewhere."

---

### Option B — Lock/capsule-protected resource invariant

**(a) Idea + literature.** A lock guards a piece of `uncontended` state
satisfying invariant `J`; acquiring the lock *lends* you the state (you may
transiently break `J`), releasing it *demands* `J` restored. This is the
original CSL **conditional critical region / resource invariant** rule
(O'Hearn 2004; Brookes' soundness, 2004). OxCaml realizes it *exactly* with
**capsules**: `Capsule.Mutex.with_lock mutex ~f` hands `f` a password → access →
`uncontended` view of the capsule's data; when `f` returns, the lock releases.
The capsule *brand* `'k` statically names which invariant governs which state.

**(b) Surface syntax (spinlock / mutex over a protected resource).**
```ocaml
(* J(acct) := acct.balance >= 0                                        *)
type account = { mutable balance : int }        (* lives in capsule 'k *)

(* with_lock's body gets the account UNCONTENDED (exclusive) and must  *)
(* leave J intact. Inside, it's ordinary sequential vox: borrows,      *)
(* now/fin, McCarthy stores all apply because we have exclusive access.*)
val with_lock :
  'k Capsule.Mutex.t ->
  f:(acct:account @ uncontended{ J _ } -> account @ uncontended{ J _ }) ->
  unit @@ portable

let withdraw mutex amt =
  Capsule.Mutex.with_lock mutex ~f:(fun acct ->
    if acct.balance >= amt
    then { balance = acct.balance - amt }   (* J preserved: >= 0       *)
    else acct)                              (* J preserved: unchanged   *)
```

**(c) Proof obligations + Lean + grind.** One obligation per critical section:
*assuming `J` on entry, prove `J` on the returned state.* The body is verified
with vox's **existing sequential machinery** — because the lock gives exclusive
`uncontended` access, the critical section is single-threaded, so borrows/now-
fin/McCarthy are all exact. Lean: the protected state's model + `J` as a
`public opaque` Prop; `with_lock`'s trusted axiom threads `J` in and out.
**Verdict: grind-shaped** — it's a sequential contract on the critical section,
no different from any vox function contract.

**(d) Mode system discharges.** *Mutual exclusion.* The fact that only one
domain is ever in the critical section is guaranteed by the capsule/mutex (a
dynamically-unique key ⇒ locking yields exclusive access). vox never proves
non-overlap; it assumes exclusive access and proves the body preserves `J`.
This is the cleanest division of labor in the whole study: **checker proves
critical sections don't overlap; vox proves each preserves the invariant.**

**(e) TCB delta.** A trusted reflection of `Capsule.Mutex.with_lock` (and
`Data.map`/`unwrap`): its axiom says "runs `f` with exclusive `uncontended`
access; `J` in ⇒ `J` out." Plus LDRF-SC (§2). Under the **M1/M3 (mode-backed)**
substrate sharedmut chose, the "critical section touches only the capsule's
state" side condition is the mode checker's job (the capsule brand `'k` scopes
what `f` can reach) — no frame obligation falls to vox. Only if sharedmut ever
reaches for its deferred *explicit-heap* option would a frame-rule obligation
reappear here.

**(f) Benchmark ladder position.** Nails **lock-protected invariant** (bank
account `balance >= 0`; a shared data structure with a structural invariant
under a coarse lock). *Tops out* at cross-critical-section *temporal* properties
("between these two acquisitions the balance only grew") — those need a
rely/guarantee or history protocol (Owicki–Gries, Jones' rely-guarantee), which
is out of the single-invariant frame.

**(g) Staging.** ≈1 quarter: trusted capsule reflection + the sequential
critical-section encoding (mostly reuse). Natural second step because it reuses
A's "single-state invariant" insight and Level 0's borrow machinery.

---

### Option C — Lock-free structures: a tractable restriction and an out-of-reach full version

**(C-restricted) SC-ghost-cell CAS loop — grind-shaped.**

**(a) Idea + literature.** Model each `Atomic.t` as a single SC ghost cell.
Verify a CAS retry loop as a *pure state transition per successful attempt*:
read old, compute new, `compare_exchange old new`; on success the cell went
`old ↦ new`, on failure retry. This is *not* linearizability — it is
*functional correctness of one successful attempt* under an assumed-atomic CAS.
It is sound under LDRF-SC because the atomic is a single SC location.

**(b) Surface syntax (Treiber push, restricted spec).**
```ocaml
(* stack modeled as a ghost List over an SC cell; push verified as:    *)
(* "if push returns, the cell holds x :: old for the old it CAS'd".     *)
let rec push st x =
  let old = Atomic.get st in
  let nw  = x :: old in
  if Atomic.compare_and_set st old nw
  then ()                                  (* cell: old ↦ x::old  ✓     *)
  else push st x                           (* someone else won; retry   *)
```

**(c) Proof obligations + Lean + grind.** Assume the CAS contract
(`compare_and_set c old nw` = "if `contents c = old` then set to `nw`, true;
else false, unchanged" — one trusted SC axiom). Then the loop body is a pure VC:
`nw = x :: old`, and on success `contents' = nw`. grind handles it. **Verdict:
grind-shaped, but the spec is weak** — it says nothing about what happens
*between* the read and the CAS (another push/pop may have intervened; that's why
we retry), and it does *not* compose into an atomic triple usable by a concurrent
`pop`.

**(d) Mode system discharges.** Atomicity of the single word (assumed via the
CAS axiom); that the structure is reached only through the atomic (rule 2).

**(e) TCB delta.** The CAS SC axiom (folded into `atomic_lib`) + LDRF-SC. But
**a documented soundness caveat**: the per-attempt spec is only meaningful for
*single-word* structures where the CAS is the sole linearization point and there
is no ABA hazard on the modeled abstract state (a `List` model with immutable
nodes is ABA-safe; a modeled *pointer/index* is not). This caveat must be stated
loudly — it's the difference between "sound restriction" and "silently wrong."

**(C-full) Linearizability / logical atomicity — out of grind's reach.**

**(a) Idea + literature.** A genuine atomic triple `⟨ xs. stack ↦ xs ⟩ push x
⟨ stack ↦ x::xs ⟩` (TaDA — da Rocha Pinto et al., 2014; Iris logical atomicity —
Jung et al.; Jacobs & Piessens). The abstract state lives in a *shared
invariant*; the successful CAS is the *linearization point* where a **ghost
update** atomically advances the abstract state, coordinated with concurrent
ops. MS-queue and elimination stacks additionally need *helping* and *prophecy*.

**(c) Why it's not grind-shaped.** The proof obligations are **view shifts**
(`|=>`), **invariant opening/closing** (`iInv`), and **ghost updates** on
resource algebras (auth, fractional, agreement). These are *interactive*,
*step-indexed*, and discharged in Iris's tactic proof mode (`iMod`, `iApply`,
`iFrame`) — there is no decision procedure, and grind has no notion of
invariant-opening or a step index. vox would need an entire separation-logic
proof mode with ghost state and view shifts. That is a *different verifier*, not
a vox feature. **Verdict: out of scope; recommend not attempting in vox.**

**(d) Mode system discharges.** Nothing extra here — the mode system gives DRF,
but linearizability is a *functional* property orthogonal to race freedom.

**(f) Benchmark position.** C-restricted reaches **lock-free stack push**
(functional, single-attempt). C-full would reach true **Treiber
linearizability**; **producer/consumer (bounded queue)** needs two-sided logical
atomicity + emptiness/fullness protocols + (usually) blocking ⇒ *liveness*, so
it is beyond *all* deductive options here and is a model-checking (D) target.

**(g) Staging.** C-restricted: ≈1 quarter (rides on `atomic_lib`). C-full:
year+/research, and the honest recommendation is to route it to a dedicated
CSL tool rather than grow one inside vox.

---

### Option D — Model-checking complement

**(a) Idea + literature.** For the specs vox's deductive core can't reach
(linearizability of a real queue, ABA, liveness), attach a *bounded interleaving
explorer* as an unsound-but-cheap bug-finder — partial-order reduction / DPOR
(Flanagan & Godefroid, 2005) over the domains' atomic steps. There is an OCaml
tool in this space, **DSCheck**, for testing lock-free structures via systematic
interleaving exploration *(flagging uncertainty: I recall DSCheck as a
Jane-Street-adjacent OCaml DPOR checker for Atomic-based structures; verify the
exact name/authors before citing in anything load-bearing)*.

**(b)-(e).** Not a proof; produces a concrete failing interleaving or "no
counterexample within bound k." Ties into vox's *existing* disproof/quickcheck
classification pipeline (the `plausible`/witness-validation work): a concurrent
VC that vox can't prove could be handed to the explorer to *classify* — genuine
bug (with a witnessing schedule) vs. beyond-the-fragment. TCB: **zero** — it's
outside the trusted core by construction.

**(g) Staging.** Opportunistic; only worth it once A/B exist and users hit
specs the deductive core rejects.

---

## 4. Benchmark ladder — where each option tops out

| Benchmark | Reached by | Ceiling / why |
|---|---|---|
| Atomic counter monotonicity | **A** (monotone lower bound) | Can't express "final = #incrementers" (needs auth/frac ghosts). |
| SC publication (message passing) | **A** (invariant ties flag→payload) | Provable *only* because `Atomic` is SC (LDRF-SC). |
| Lock-protected invariant (`balance ≥ 0`) | **B** | Can't express cross-section temporal facts (needs rely-guarantee). |
| Lock-free stack push | **C-restricted** (per-attempt) | Not a composable atomic triple; single-word + ABA-safe model only. |
| Treiber stack linearizability | **C-full** | View shifts / ghost updates — not grind-shaped; out. |
| Producer/consumer (bounded queue) | **D** (bug-finding) only | Two-sided logical atomicity + liveness — beyond all deductive options v1. |

---

## 5. TCB delta (consolidated)

1. **LDRF-SC-for-vox axiom** (§2): well-moded ⇒ SC interleaving semantics.
   Rests on (a) the OxCaml mode checker's DRF guarantee and (b) the OCaml 5
   memory-model theorem. Large but principled — it is the same foundation the
   entire OxCaml parallelism story already assumes.
2. **`par_lib`** (Level 0): `unsafe_globalize_task` + `Obj.magic_unique`.
   Already trusted; signature is the spec.
3. **`atomic_lib`** (A, C-restricted): trusted op contracts (get returns `I`;
   set requires `I`; CAS is a single SC transition). One library, ~6 axioms.
4. **capsule reflection** (B): trusted `with_lock` contract (exclusive
   `uncontended` access; `J` in ⇒ `J` out). One library.
5. **Explicitly NOT in scope** (state loudly): fairness, liveness,
   progress/lock-freedom, deadlock-freedom, lock-ordering, and **linearizability**.
   vox is a *safety* verifier; none of these are provided. C-restricted's
   single-word/ABA caveat is a documented soundness boundary, not a guarantee.

---

## 6. Recommendation + minimum viable concurrency story

**Recommendation.** Build the ladder bottom-up and stop where grind stops.

1. **Now:** document **Level 0** (disjoint fork/join over borrows) as the
   canonical vox concurrency story — it already verifies a parallel quicksort.
2. **≈1 month (MVP):** **Option A — atomic invariant cells = sharedmut's M1 with
   an `Atomic.t` carrier.** If M1 (`[@@vox.cell_invariant]`) lands first, Option A
   is a *one-attribute* extension: point the cell at an `Atomic.t` and add the
   LDRF-SC axiom so the write is safe under parallel writers, not just aliases.
   A trusted `atomic_lib`; a monotone counter and an SC-publication demo;
   everything else stays `uncontended` and reuses vox unchanged.
3. **≈1 quarter:** **Option B — lock/capsule-protected invariants** (reuses A's
   single-state invariant + Level 0's borrows), and **C-restricted** for
   single-word lock-free structures with the ABA caveat documented.
4. **Do not build C-full in vox.** Logical atomicity / linearizability is not
   grind-shaped; route it to a dedicated CSL tool. Add **Option D**
   (model-checking) opportunistically as a bug-finder tied to the existing
   disproof pipeline.

**The honest minimum viable concurrency story:**

> vox verifies the **sequential heart** of each concurrent idiom — invariant
> preservation for atomic cells, critical-section correctness for locks. The
> **mode system** supplies data-race freedom (thread-locality of `uncontended`
> state) and mutual exclusion (capsules) *for free*. **LDRF-SC** lets vox stay
> entirely inside its existing sequential Lean/grind model — no weak memory, no
> interleaving induction, no view shifts. The result costs **no new proof
> theory**: a handful of trusted primitive libraries (`atomic_lib`, capsule
> reflection) in the exact style of the existing `slice_lib`, plus one honest
> memory-model axiom. What it *cannot* do — linearizability, liveness,
> progress — it declines cleanly rather than faking.

This is a real, shippable concurrency story that is *smaller* than it looks
precisely because OxCaml's mode system has already done the load-bearing work.

---

## 7. Does concurrency pull in SL-in-Lean? (the honest answer to sharedmut's trigger)

sharedmut deferred SL-in-Lean (its Option 3) and recorded its trigger as
*"vox commits to being a general concurrent program logic"* — and flagged that
this quest is the one that might pull it in. Weighed honestly:

- **No, for the recommended fragment (Level 0 + A + B + C-restricted).** Every
  one of these stays inside grind's reach *because* it never needs the two
  things SL exists to provide: (1) **frame inference** — the mode system supplies
  disjointness (Level 0) and exclusion (B) *statically*, so there is no
  `?frame` to infer; and (2) **ghost updates / view shifts over a shared
  invariant** — Options A/B use a *single-state* invariant that is re-established
  per op, never a resource that is transferred or updated across an interference
  boundary. LDRF-SC removes the third SL motivator (weak memory) outright. So
  the concurrency MVP does **not** trigger SL.
- **Yes, and only, at C-full (linearizability).** A composable atomic triple for
  a lock-free structure genuinely needs a shared invariant + ghost update at the
  linearization point — that is exactly Iris, exactly not grind-shaped, and
  exactly sharedmut's SL trigger. **This is why the recommendation stops before
  C-full.** The boundary where concurrency would force SL is precisely the
  boundary this doc declines to cross.

So the honest weighing: **concurrency does not advance sharedmut's SL trigger so
long as vox's concurrency ambition is "safety invariants + mutual exclusion,"
not "linearizability of lock-free structures."** If the project ever wants the
latter, that — not atomics or locks per se — is the decision that pulls in
SL-in-Lean, and it should be taken as its own deliberate, multi-quarter
commitment (or routed to a dedicated tool), not backed into via the concurrency
roadmap.
