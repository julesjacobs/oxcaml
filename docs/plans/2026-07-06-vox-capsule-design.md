# vox refinements on the Capsule API — design options

Status: design study. Author: vox-capsule agent, 2026-07-06.
Sibling studies to reconcile against: shared-mutation (general aliased-heap
semantics), conc-design (general atomics reasoning). This document owns the
**concrete Capsule API surface**; it is likely the most concrete instantiation
of the conc-design story.

---

## 0. TL;DR

A capsule key (or password) is a **mode-level proof of exclusive — or shared
read-only — access** to a piece of mutable state. That is *exactly* the
no-interference license vox already needs to reason about mutation, and which
its borrow layer today obtains only by **trusting** `assume_unchecked_` facts
about uniqueness.

The design: give a capsule's contents a **refinement invariant** (and, more
generally, a via model and a now/fin two-state contract). Every
key/password-mediated access **assumes** the invariant on entry and must
**re-establish** it on exit. The obligation attaches to the closure passed to
the access primitive (`with_password ~f` / `access ~password ~f` /
`Sync.With_mutex.with_lock ~f`). The generated VC is first-order and
grind-friendly: `inv(now) ∧ region-facts ⊢ inv(fin)`.

The prize: **the no-interference premise is discharged by the OxCaml mode
checker, not trusted by vox.** Concurrency is erased before it reaches Lean —
the access region collapses to a sequential mutable cell, which is precisely
what vox's existing borrow now/fin machinery already verifies. This is the
first genuinely *concurrent* verified vox story: the same counter you verify
single-threaded verifies when shared across domains behind a mutex, and the
mode checker certifies the sharing is race-free.

**Recommendation:** Option 1 (capsule invariants, monitor-style) as the
foundation, generalizing into Option 2 (now/fin two-state) and Option 3
(via-modeled contents) as contents get richer. They are one mechanism at three
levels of generality, all riding existing infrastructure (borrow now/fin +
via + ghost sorts). **First milestone:** an invariant on an `Owned` /
`with_password` single-key counter (`0 <= i`), no `Atomic`, no mutex, verified
end to end through a small trusted `capsule_spec` shim modeled on `borrow_lib`.

---

## 1. Phase 1 — inventory of the actual Capsule API

### 1.1 Where it lives (and where it does not)

The Capsule API is **not in the oxcaml tree**. `grep -r Capsule` over
`stdlib/`, `otherlibs/`, and the whole clone returns nothing. It lives in the
**jane monorepo**:

- `lib/capsule/src/capsule.ml` — the `Capsule` entry point (re-exported by
  `Core`). Body is `include Prim.Extended` plus `module Blocking_sync`.
  (`lib/capsule/src/capsule.ml:7-9`)
- `lib/capsule/prim/prim.mli` — the **low-level core**: `Access`, `Password`
  (+ `Password.Shared`), `Key`, `Data` (+ `Data.Shared`, `Data.Local`,
  `Data.Or_null`), `create`, `access`. This is the authoritative signature.
- `lib/capsule/prim/extended_intf.ml` — the **higher-level `Extended`
  signature** actually surfaced by `Capsule`: `Access`, `Data` (with
  `create`/`wrap`/`unwrap`/`return`/`get_id`), plus `Frozen`, `Owned`,
  `Scoped` (+ `Scoped.Shared`), `Initial`.
- `doc/ocaml/parallelism/capsule.mdx` — the narrative guide (counter example,
  `Frozen`/`Owned`/`Scoped`, the initial capsule, and the `Await`-library
  `Capsule.Sync.With_mutex` / `With_rwlock` synchronization wrappers).
- `lib/capsule/blocking_sync/`, and the `Await` library
  (`Capsule.Sync.*`, `Capsule.Await.*`) — runtime mutex/rwlock wrappers.

It is built entirely on **oxcaml modes** (portable / contended / unique /
shared / local / once / aliased / forkable), which *are* present in this tree
(`testsuite/tests/typing-modes/`). So the API can be **modeled** here against a
mode-faithful stub even though the library itself is not vendored.

### 1.2 The core types and their modes (cite: `lib/capsule/prim/prim.mli`)

Every explicit capsule carries a type-level **brand** `'k`. Four pillars:

**`Access.t` — proof of "current capsule".** (`prim.mli:42-66`)
```
type 'k t : void mod aliased external_ global many portable
```
An `uncontended` `'k Access.t` proves `'k` *is* the current capsule (exclusive);
a `shared` one proves it is current but possibly shared read-only. Captured in
a `portable` closure it becomes `contended` — which grants *no* capabilities.
This is the token `Data.wrap`/`unwrap` consume. Obtained via `current ()`
(`prim.mli:70`), `Key.access`, `Key.destroy`, or handed in by a mutex region.

**`Password.t` — permission for uncontended access.** (`prim.mli:98-161`)
```
type 'k t : void mod aliased contended external_ portable unyielding
```
"Permission for the current thread to have `uncontended` access to `'k`."
Available **only locally**, so cannot move between threads. Obtained from a
`'k Key.t @ unique` or by acquiring a mutex/rwlock. The mode system prevents
retaining it past the region ⇒ *uncontended access is granted to one thread at
a time.* `Password.Shared.t` (`prim.mli:118-146`) is the read-only analogue
(from an `aliased` key or a read-lock). `Password.shared` downgrades
(`prim.mli:149`).

**`Key.t` — ownership.** (`prim.mli:164-259`)
```
type 'k t : void mod contended external_ forkable many portable unyielding
```
`'k Key.t @ unique` = exclusive ownership (uniqueness ⇒ only one thread can
access). `'k Key.t @ aliased` = permanently shared read-only. Region brackets:
`with_password` (`prim.mli:193-198`), `with_password_local` (`:203-208`),
`with_password_shared` (`:212-217`), `access ~f` (`:233-238`, returns
`#('a * 'k t)`), `access_shared` (`:242-248`). `destroy` (`:255`) merges the
capsule into the current one and yields an `Access.t`.

**`Data.t` — a pointer to contents.** (`prim.mli:300-903`)
```
type (+'a, 'k) t : value mod everything with 'a @@ contended portable
```
A `('a,'k) Data.t` holds an `'a` inside `'k`; it **crosses contention and
portability**, so it is freely shareable across threads — but its contents can
only be reached with an `Access.t`/`Password.t` for `'k`. Load-bearing ops
(also in the friendlier `extended_intf.ml:68-151`):
- `create : (unit -> 'a) @ local once portable -> ('a,'k) t` (`prim.mli:354`)
  — runs the ctor inside a *fresh* capsule; the ctor is `portable` so it cannot
  capture the caller's uncontended state.
- `wrap : access:'k Access.t -> 'a -> ('a,'k) t` (`prim.mli:309`) — tags a
  current-capsule value as belonging to `'k`. `unwrap` is the inverse
  (`prim.mli:313`); with a `shared` access it returns the value `@ shared` and
  requires `'a : value mod portable` (`prim.mli:347-350`).
- `map ~password ~f`, `extract ~password ~f`, `iter ~password ~f`, `bind`
  (`prim.mli:404-477`) — apply a `portable` `f` to the contents under a
  password.
- `return`/`inject` (`extended_intf.ml:128`, `prim.mli:443`) — put an
  always-`contended`, `portable` value in with no access needed.
- `get_id`/`project` (`extended_intf.ml:143`, `prim.mli:449`) — read a
  `value mod portable` value out **without any access**, at `@ contended`
  (safe because the result can never be mutated in place).

### 1.3 The synchronization wrappers (cite: `capsule.mdx`, `Await`)

- `Frozen` (`extended_intf.ml:202-229`, mdx §Frozen): permanently frozen
  capsule; `unwrap : 'a t -> 'a @ portable shared` — everyone reads, nobody
  writes, no synchronization.
- `Owned` (`extended_intf.ml:231-271`, mdx §Owned): uniqueness *is* the access
  proof. `with_ : 'a t @ unique -> f:('a -> 'b) -> #('a t * 'b)`; `freeze`
  turns an aliased owned capsule into a `Frozen.t`.
- `Scoped` (`extended_intf.ml:273-373`, mdx §Scoped): bundles a `Password.t`
  with a `Data.t`; `get`/`iter`/`map` each take a `portable` `~f`. Crosses
  contention (so it can be held open alongside another capsule) but is
  `local`. `Scoped.Shared` is the read-only analogue.
- `Capsule.Sync.With_mutex` (mdx §"Let's synchronize"): the everyday form.
  `create : (unit -> 'a) -> 'a t`; `with_lock sync t ~f:(fun _sync r -> ...)`
  hands `f` **uncontended** access to the contents `r` for the duration of the
  lock; `f` must be `portable`. `with_scoped` hands a `Scoped.t` so two locks
  can be held at once. `With_rwlock` splits `with_write` (exclusive) /
  `with_read` (shared).

### 1.4 What the mode system GUARANTEES vs what is conventional

**Guaranteed by the type/mode checker (no runtime cost, no TCB for a client):**
1. **At most one uncontended accessor at a time.** A `Password.t`/`Access.t @
   uncontended` for `'k` exists only while its region is open; uniqueness of
   the key or the held mutex serializes regions. This is the *data-race-freedom*
   theorem. (`prim.mli:98-108`, `:164-176`.)
2. **No leakage of contained uncontended values across the boundary.** Access
   tokens become `contended` when captured in a `portable` closure; the access
   `~f` is `portable`, so it cannot smuggle another capsule's uncontended
   references in, nor let this capsule's escape. (`prim.mli:49-51`; mdx leak
   example lines 413-429; nested-lock rejection lines 688-710.)
3. **Shared access is read-only and portability-crossing.** `unwrap @ shared`
   forces `'a : value mod portable` and returns `@ shared`, so concurrent
   readers cannot mutate or observe a torn write. (`prim.mli:344-350`.)
4. **`Data.t` is safe to move between threads** but inert without a token
   (`contended portable`). (`prim.mli:305`.)

**Enforced dynamically (by `Await`/`Sync`), not by types:** *which* thread gets
the lock next, blocking/waiting, and deadlock-avoidance (mdx explicitly warns
`transfer` can deadlock, lines 773-788).

**Conventional / not enforced:** exception safety — if `with_password`'s `f`
raises, "the key is destroyed, leaking the contents" (`prim.mli:191-192`,
`:230-232`). This matters for the invariant discipline (see §6, and the
exceptions sibling).

---

## 2. Phase 1 — baseline: what does vox do with a capsule program *today*?

### 2.1 The setup constraint

Because the Capsule library is not vendored in the oxcaml tree, we cannot
compile a literal `open Core; Capsule.Data.create ...` program here. The
honest baseline is a **mode-faithful stub** `.mli` reproducing the capsule
signatures with their real modes, plus a tiny client, compiled with the
vox-enabled compiler. This measures the two things that matter: (a) do the
capsule *modes* typecheck in this tree, and (b) what does vox make of the
abstract capsule types with no vox annotations — opaque? sorts? mode errors?

### 2.2 Baseline result (measured)

Compiled a mode-faithful stub (`_probe/capstub.mli`) of `access` / `data` /
`create` / `wrap` / `unwrap` with their real modes
(`@ local once portable`, `access:'k access`, `: value mod portable`), plus a
trivial `roundtrip` client, using the freshly-built vox compiler
(`_install/bin/ocamlc.opt -c -dump-vc -vox-dry-run`):

- The stub **`.mli` and the client compiled cleanly, exit 0, with zero VCs
  dumped.** vox introduced no mode errors and emitted nothing — the capsule
  types are **opaque sorts** with no contracts, so verification is a sound
  no-op. This confirms the predicted baseline: *capsules are invisible to vox
  until you give their contents refinements.*
- (The stub `.ml` fails a kind check because a toy `{ v : 'a }` record does not
  cross portability the way the real `Data.t`'s `value mod everything with 'a`
  does — an artifact of the mock, unrelated to vox.)

So the baseline is: **capsules are opaque to vox; verification is vacuous.**
Every option below is about giving these types contracts. The concrete
Milestone-1 result — a *real generated VC* — is in §7.

---

## 3. The load-bearing insight: mode ⇒ no-interference ⇒ sound to thread state

vox already verifies mutation, via the **borrow layer**
(`testsuite/tests/vox/lib/borrow_lib.mli`). That layer is *not* a checker in
the compiler; it is a **library encoding** of RustHorn/Creusot mutable borrows:

- `vref [@@vox.sort int]` — a mutable int cell whose logical representative *is*
  its contents.
- `borrow_mut (p:proph) (x:vref) k` — a CPS bracket handing `k` a loan
  `m : mut{ now _ = x && fin _ = p } @ local unique`, returning
  `(vref{ _ = p } * 'b)`. `now`/`fin` are opaque `Int` projections declared in
  a `[%%vox.lean]` block.
- The whole thing is **trusted**: `borrow_lib.ml` implements every op as
  `assume_unchecked_` (the `Assume` VC kind — never sent to Lean). The header
  says so: *"TRUSTED: the implementation asserts the invariants above with
  `assume_unchecked_` / mode casts; everything else is proved."*

**What makes those `assume_unchecked_` facts sound?** The `@ local unique` loan
cannot escape the continuation, and uniqueness forbids stale aliases — so no
interference can invalidate `now`/`fin` mid-borrow. In other words, **the
soundness of the trusted borrow contract rests on a mode-system property
(uniqueness ⇒ exclusive access ⇒ no interference).** vox trusts it because it
cannot *see* the mode-level argument; it just asserts the conclusion.

The Capsule API is the same property, **named and generalized to shared state**:
- `Key.t @ unique` / an open `with_password` region = exclusive access =
  *exactly* the borrow's `@ local unique` loan, but now over state that may be
  *shared across domains* between regions.
- `Password.Shared` / `with_read` / `Frozen` = shared read-only access = a
  borrow that may alias but not write.

Therefore: **inside an access region the capsule contents behave as a
single-owner mutable cell, and threading now/fin (or an invariant) is sound —
by the mode checker's data-race-freedom theorem, not by an `assume_unchecked_`
on uniqueness.** The concurrency is *erased* at the region boundary; Lean sees
sequential mutation. This is the bridge every option below stands on.

### 3.1 Is trusting the capsule discipline TCB, or type-safety-free?

Split the guarantee in two:

| Guarantee | Discharged by | In vox's TCB? |
|---|---|---|
| No two threads mutate `'k` concurrently; access region is interference-free | OxCaml **mode checker** (data-race freedom); vox runs *after* mode-checking | **No** — same trust vox already places in `@ unique` |
| The ghost model / now / fin / invariant *relate correctly to the runtime* (specs of ~8 capsule primitives) | A small **`capsule_spec` shim** using `assume_unchecked_`, exactly like `borrow_lib.ml` | **Yes**, but tiny & auditable |
| The invariant holds on region entry ⇒ must hold on exit | **Lean** (the emitted VC) | **No** — proved |

Net new TCB beyond today's borrow layer: **the specs of the capsule
primitives** (`wrap`/`unwrap`/`with_password`/`map`/`extract`), a shim on the
order of `borrow_lib.ml`. The interference-freedom that *justifies* those specs
is the mode checker's — free. This is strictly better than the borrow layer,
whose no-interference premise vox cannot even name.

---

## 4. Design options

Notation follows real vox style: refined types `t{ P }`, dependent binders
`(x : t) -> ...`, ghost sorts `type s [@@vox.sort int]`, model functions and
laws in `[%%vox.lean {lean| ... |lean}]` blocks, and `[@vox.via (f : target)]`.

Throughout, we model a capsule cell's **logical contents** with a ghost
projection `contents : ('a,'k) data -> <model>` (the capsule analogue of
`now`), declared opaque in a Lean block. `data` is the vox-specced stand-in for
`('a,'k) Capsule.Data.t`.

### Option 1 — Capsule invariants (monitor style) — RECOMMENDED FOUNDATION

A capsule cell carries a declared refinement **invariant** on its contents.
Every access assumes it on entry and must re-establish it on exit. This is the
classic monitor / representation invariant of a lock-protected resource, made
concrete: the "invariant-cells" idea from the shared-mutation sibling, pinned
to a real access token.

**(a) Surface syntax — capsule-held counter with `0 <= i`.**

```ocaml
(* capsule_spec.mli — a vox-specced, trusted shim over Capsule.
   'inv is the monitor invariant; contents is the ghost model. *)

type ('a, 'k) cap                       (* models ('a,'k) Capsule.Data.t + its invariant *)
type 'k pass                            (* models 'k Password.t @ local *)

[%%vox.lean {lean|
public opaque contents : VoxU -> Int    (* the ghost logical value of the cell *)
|lean}]

(* create with an invariant the initial value must satisfy *)
val create_counter : (init : int){ 0 <= init } -> (counter, 'k) cap{ 0 <= contents _ }

(* the access region: the closure sees the contents as a mutable loan whose
   entry state satisfies the invariant and whose exit state must too. *)
val with_counter :
  (c : (counter,'k) cap{ 0 <= contents _ }) @ local ->
  (p : 'k pass) @ local ->
  f:( (m : counter_loan{ 0 <= now _ }) @ local unique ->
      (counter_loan{ 0 <= now _ } * 'b) @ unique ) @ local once portable ->
  'b @ contended once portable
```

The invariant `0 <= contents _` rides on the `cap` type (declared at
`create`). `with_counter` hands `f` a loan whose **entry** refinement
`0 <= now _` is the invariant *assumed*, and whose **exit** refinement
(the returned loan's `0 <= now _`) is the invariant *required*. Increment
verifies; a body that could drive `i` negative fails the exit VC.

**(b) VC generation + mode discharge.** Under the hood `with_counter` is the
borrow bracket: `f` gets a `mut`-style loan (now/fin). Two extra facts vs a
bare borrow:
- On entry, `emit_vc` adds the **assumed** fact `inv(now m)` to the region's
  fact set (kind is irrelevant — it is a hypothesis, sourced from the `cap`
  refinement carried into the region).
- On the closure's return, `emit_vc` produces goal `inv(now m')` for the
  returned loan `m'`, `kind = Prove` — the re-establish obligation.

What the **mode system discharges**: that no other thread mutates `'k` during
the region (the `pass` is `local`, obtained from the key/mutex; the mode
checker's data-race-freedom theorem). vox does *not* emit any framing VC — the
`cap`/`pass` types being well-moded *is* the frame. What becomes a **Lean
obligation**: only `inv(now) ∧ region-facts ⊢ inv(fin)`.

Soundness argument (mode ⇒ logic): the interference-free region is guaranteed
by modes; within it the cell is a single-owner mutable, for which vox's borrow
now/fin is already sound; the monitor invariant is the standard rely-guarantee
degenerate case (rely = "invariant holds", guarantee = "restore invariant").
The only TCB is `capsule_spec.ml`'s `assume_unchecked_` giving `contents`/loan
their meaning — sound *because* the region is interference-free.

**(c) Lean encoding + grind.** `contents`/`now`/`fin` are opaque `Int`; the
VC is `0 <= now m ∧ (facts from increment: now m' = now m + 1) ⊢ 0 <= now m'`.
First-order linear arithmetic — grind closes it instantly. Identical shape to
the borrow demos that already grind (`demo/lean_borrow.ml`).

**(d) What it unlocks.** A verified lock-protected counter/bank-account whose
invariant is machine-checked and whose *sharing* is mode-certified race-free —
the smallest end-to-end concurrent verified structure. Directly generalizes to
`Sync.With_mutex` (§6): `with_lock sync c ~f` is the same region.

**(e) Incremental path.** Milestone 1 (see §5). Needs: a `capsule_spec` shim
(a `.mli` + trusted `.ml` like `borrow_lib`), zero compiler changes.

**(f) Contention interaction.** With a `shared` password / `with_read` /
`Frozen`, the loan is read-only (`@ local`, not `unique`): entry assumes
`inv`, exit obligation is vacuous (contents unchanged), read results are
`@ shared`. See §6.

### Option 2 — now/fin through the key (two-state contracts)

Generalize the fixed monitor invariant to a **Creusot-style two-state
contract**: the access closure's contract relates contents before/after,
`fin d = f (now d)`. This is Option 1 with an arbitrary relational post instead
of "restore inv". Key possession replaces the trusted framing.

**(a) Surface — a caller-chosen post.**
```ocaml
val with_ :
  (c : ('a,'k) cap) @ local -> (p : 'k pass) @ local ->
  f:( (m : ('a,'k) loan{ now _ = contents c }) @ local unique ->
      ('a,'k) loan @ unique ) @ local once portable ->
  (('a,'k) cap{ contents _ = <fin of the loan> } * unit) @ ...
```
i.e. exactly `borrow_mut`, but the loan is licensed by `p : 'k pass` (a
password from the key/mutex) rather than by an `assume_unchecked_` on a
`@ local unique` vref. `mget`/`mset`/`mdrop` transfer verbatim; the residual
`cap` is viewed at the prophesied `fin`.

**(b) Mode discharge / obligation.** Same split as Option 1. The prophecy `fin`
is honest because the region is interference-free — and *that* is the mode
checker's theorem, whereas `borrow_lib` trusts it. So Option 2 is
`borrow_lib` with its central `assume_unchecked_` **replaced by a mode
obligation** (you must present a well-moded `pass`). Strictly less trust.

**(c) Lean + grind.** Identical to borrow now/fin, which grinds today.

**(d) Unlocks.** Full functional two-state specs of lock-protected operations
(swap, compare-and-set expressed functionally, transfer between two capsules
via nested `with_scoped`). Option 1's invariant is the special case
`fin = now, both ⊨ inv`.

**(e) Path.** Milestone 2: reuse the borrow prophecy machinery; the delta is
plumbing the `pass` where borrow plumbs uniqueness.

**(f) Contention.** A two-state relation requires *exclusive* access — a
`shared` password only licenses `fin = now` (no write), collapsing Option 2 to
"read the invariant". So shared regions get Option-1 semantics only.

### Option 3 — via-modeled capsules

The capsule's contents get a **via logical model** (a capsule-held hashtable
models as a Lean `Map`; a counter as `Int`); operations are specced against the
model; the result composes with the **sealed-interface** story — a library
exports a capsule-based verified concurrent-safe structure whose `.mli`
specifies it against the model, verified across compilation units.

**(a) Surface — a concurrent memo table specced against a Lean map.**
```ocaml
(* memo.mli — a mutex-protected memo table for a pure spec function [spec]. *)
type tbl                                 (* the mutable hashtable *)
[%%vox.lean {lean|
public opaque model : VoxU -> Std.HashMap Int Int   (* tbl's logical map *)
public opaque spec  : Int -> Int
|lean}]

(* invariant: the table only ever caches correct results of [spec]. *)
type ('k) memo = (tbl,'k) cap{ agrees (model _) }    (* agrees m := ∀k v, m[k]=v → v = spec k *)

val get : (t : 'k memo) @ local -> ('k pass) @ local -> (key:int) ->
          int{ _ = spec key }            (* result is correct regardless of hit/miss *)
```
`get` verifies that whether it returns a cached value (invariant ⇒ correct) or
computes `spec key` and inserts it (re-establishes `agrees`), the result equals
`spec key`. The table type carries a `[@vox.via (model : lmap)]` model exactly
as `set = tree{ bst _ } [@vox.via (elems : iset)]` does today.

**(b) Mode discharge / obligation.** Region interference-freedom: modes.
Model laws (`agrees` preserved by insert of a correct entry; lookup soundness):
`[@@vox.lemma]`s, re-proved by Lean, same as `bst`/`rbt`. The invariant
assume/prove: Option 1's VC over the map model.

**(c) Lean + grind.** The map model + its lemmas are the *only* Lean content;
they grind exactly as the existing via structures (`bst.mli`, `rbt`,
`ptrie`). Concurrency contributes nothing to the proof.

**(d) Unlocks.** The **sealed concurrent structure**: `memo.mli` exports
`get`/`put` with model contracts and a `[%%vox.lean]` interface block
(compiled to `VoxSig_Memo.olean`); a *client in another unit* verifies against
those contracts, and the mode checker certifies the client shares the table
across domains race-free. This is the "verified concurrent library" endgame.

**(e) Path.** Milestone 3, after Options 1–2 and the mutex wrapper (§6).
Depends on the existing cross-unit via/lemma packaging ([[vox-stdlib-design]]).

**(f) Contention.** `with_read`/shared readers get `agrees` (the standing
invariant) but not two-state facts; a reader's result `spec key` is still
provable from `agrees` alone. Contended (no-password) observers using
`get_id`/`project` on a `value mod portable` projection get the *type* but no
refinement facts (a concurrent writer may be mid-insert) — unless the fact is
published as monotone (§4-note below).

### Option 4 — ghost-key refinements (typestate)

The **key value itself** carries a refinement tracking a protocol/state-machine
over the capsule (typestate). Honest assessment of expressibility follows.

**(a) Surface — a two-phase resource (`Building → Sealed`).**
```ocaml
type 'k key [@@vox.sort int]             (* phase: 0 = Building, 1 = Sealed *)
val create   : unit -> ('k key{ _ = 0 } * ('a,'k) cap) @ unique
val add      : (k : 'k key{ _ = 0 }) @ unique -> ('a,'k) cap @ local -> 'k pass @ local
               -> item -> 'k key{ _ = 0 } @ unique
val seal     : (k : 'k key{ _ = 0 }) @ unique -> 'k key{ _ = 1 } @ unique
val read     : (k : 'k key{ _ = 1 }) -> ('a,'k) cap @ local -> result   (* only when Sealed *)
```

**(b)–(c).** The phase is a ghost `[@@vox.sort int]` on the key; transitions
thread it through refined returns; **uniqueness** (`@ unique`) forbids replaying
an old phase — identical to how `proph`/`mut` are consumed once in
`borrow_lib`. VCs are equalities/inequalities on the phase int; grind-trivial.

**(d) Honest verdict.** *Expressible today* for **linear (unique-key)
typestate** via the ghost-sort + uniqueness trick already proven by `proph`.
`aliased` (shared) keys correctly cannot advance the phase (read-only), which
matches. What is **future work**: coupling the key's phase-ghost to the *actual
contents* (needs a shared-brand invariant relating `key`'s ghost and the cell's
`contents` — a coupling predicate vox can state but which needs care), and
multi-key / condition-variable protocols. Recommend deferring Option 4 until
Options 1–3 land; it adds no new mechanism, only modeling discipline.

**A note on monotone/published facts (relevant to (f) everywhere).** A
`contended` observer (bare `Data.t`, no password) can soundly assume a fact
about the contents *only if that fact is monotone* — established once and never
falsified by any writer (e.g. "the table is append-only", "the counter never
decreases", a `Frozen` capsule's permanent invariant). vox could support this
with a `[@@vox.monotone]` marker on a `cap` invariant, licensing its assumption
without a password. This is the sound generalization of "the invariant always
holds"; without it, the safe default is **contended ⇒ no refinement facts.**

---

## 5. Options at a glance

| Option | Idea | Unlocks | Mode discharges | New TCB | Effort |
|---|---|---|---|---|---|
| **1. Capsule invariants** | monitor inv on contents; assume-entry/prove-exit | verified lock-protected counter/account, mode-certified race-free | region interference-freedom (whole frame) | `capsule_spec` shim (~borrow_lib size) | **S** — 0 compiler changes |
| **2. now/fin thru key** | Creusot two-state region contract | full functional specs of lock ops; transfer | same; replaces borrow's central `assume_unchecked_` with a mode obligation | reuses borrow shim | **S–M** — reuse prophecy plumbing |
| **3. via-modeled** | contents = Lean model (Map/…); sealed .mli | verified concurrent *library* across units | same | model laws are proved lemmas (no trust) | **M** — needs §6 + cross-unit via |
| **4. ghost-key typestate** | key carries protocol state-machine | phase-safe capsule protocols | uniqueness forbids replay | none new (modeling only) | **M–L** — contents-coupling is future work |

All four ride the *same* infrastructure already in the tree: borrow now/fin,
via/`vox_map`, ghost sorts (`[@@vox.sort]`), `[@@vox.lemma]`, and Lean blocks.
None requires a new checker; the compiler is untouched for Options 1–2.

---

## 6. Interaction with contention modes and the `Sync` wrappers

- **Uncontended (unique key / `with_password` / `Sync.With_mutex.with_lock` /
  rwlock `with_write`):** read + write; assume invariant on entry, re-establish
  on exit; may thread a full now/fin two-state contract. `with_lock sync c ~f`
  is literally the Option-1/2 region — the `~f` closure is where the obligation
  attaches.
- **Shared (aliased key / `with_read` / `Scoped.Shared` / `Frozen`):** read
  only; assume the **standing invariant only**; exit obligation vacuous; results
  `@ shared`, requiring `'a : value mod portable`. Multiple concurrent readers
  are fine (none writes ⇒ invariant stable). What may be assumed: the invariant;
  what may **not**: any two-state/relational fact, and any *staleness-sensitive*
  fact (a reader sees a consistent snapshot under the read-lock, but between two
  separate read regions the value may have advanced).
- **Contended (bare `Data.t`, no token):** contents unreachable
  (`unwrap` needs a token); `get_id`/`project` on `value mod portable`
  projections give the value at `@ contended` with **no refinement facts**,
  unless published monotone (§4 note). This is the soundness firewall: *facts
  live where access lives.*
- **Exception leak:** `with_password`/`access` destroy the key and leak
  contents if `~f` raises (`prim.mli:191-192`). For the invariant discipline
  this is *safe* (a leaked capsule is never accessed again, so a broken
  invariant is unobservable), but the exit VC must be discharged on the
  **normal** path only — align with the exceptions sibling's treatment of
  `raise` inside a region.

---

## 7. Milestone-1 built and the VC observed

To go past the vacuous baseline I built the Milestone-1 shim end to end
(`_probe/capsule_spec.{mli,ml}`, modeled line-for-line on `borrow_lib`): a
single-key capsule holding an int counter with the monitor invariant
`0 <= now _`, an access region `with_pos`, an exit checkpoint `close`
(the invariant re-establish point, playing `mdrop`'s role), and `get`/`set`.
Both the interface and the trusted implementation **compile cleanly** on the
current vox tip — *no compiler changes*.

Then two clients, a positive `bump` (`v+1`) and a negative `bad` (`v-5`):

```
(* cap_pos.ml *)                          (* cap_neg.ml *)
with_pos c (fun m ->                      with_pos c (fun m ->
  let (v, m1) = get m in                    let (v, m1) = get m in
  let m2 = set m1 (v + 1) in                let m2 = set m1 (v - 5) in
  let () = close m2 in ())                  let () = close m2 in ())
```

`ocamlc.opt -c -dump-vc -vox-dry-run` emits **exactly one VC per client**, and
it is precisely the Option-1 assume-entry / prove-exit obligation:

```
POSITIVE (v+1):                           NEGATIVE (v-5):
  goal: 0 <= now m2                         goal: 0 <= now m2
  hypotheses:                               hypotheses:
    now m2 = v + 1                            now m2 = v - 5
    v = now m                                 v = now m
    now m1 = now m                            now m1 = now m
    0 <= now m        <- assumed inv          0 <= now m        <- assumed inv
```

The **entry invariant `0 <= now m` appears as a hypothesis** (assumed on the
loan the region hands the closure) and the **exit obligation `0 <= now m2` is
the goal** (demanded by `close`). The positive VC is closable by linear
arithmetic (`now m2 = now m + 1`, `0 <= now m` ⊢ `0 <= now m2`); the negative
is not (`v` may be `0..4`). This is the design's central VC, generated by an
unmodified compiler.

Caveat: `lean` is not installed in this environment (`sh: lean: command not
found`), so I ran under `-vox-dry-run` — the VCs are *generated and shown* but
not sent to the solver; I did not obtain a machine-checked pass/fail verdict.
The proof is trivial linear arithmetic that grind closes routinely (identical
in shape to the borrow demos that verify today). A cosmetic wart also shows: the
tuple returned by `get` renders as `fst *unknown1*` / `snd *unknown1*`
(a known placeholder-naming gap, unrelated to this design). Probe artifacts
live in `_probe/` in this clone (untracked).

---

## 8. Recommendation and first milestone

**Recommendation.** Adopt Options 1→2→3 as a single mechanism at increasing
generality, in that order. Option 1 (capsule invariants) is the foundation and
the honest first deliverable; Option 2 (now/fin) is a small generalization
reusing borrow prophecy; Option 3 (via-modeled, sealed) is the concurrent-
library payoff. Option 4 (typestate keys) is expressible in a limited
(linear-key) form today but should wait — its contents-coupling is genuine
future work.

The strategic point for reconciliation: **this is the first vox story where the
no-interference premise is discharged by the type system rather than trusted.**
The borrow layer (borrow-lang sibling) and the shared-mutation sibling both
ultimately trust `assume_unchecked_` framing; the capsule layer turns that same
framing into a mode obligation. conc-design should treat capsule invariants as
the concrete, mode-checked instantiation of its atomics/shared-state reasoning.

**First milestone (smallest thing that verifies a shared structure end to
end):** an **invariant on an `Owned` / `with_password` single-key capsule** — a
counter with `0 <= i`, **no `Atomic`, no mutex**. Deliverables:
1. `capsule_spec.mli` + trusted `capsule_spec.ml` (a `borrow_lib`-style shim):
   `cap`/`pass`, `contents` ghost projection, `create_counter`, `with_counter`
   with the assume-entry/prove-exit loan contract.
2. A client that increments under `with_counter` and **verifies** `0 <= i`
   preserved; a negative client (a decrement that can underflow) that **fails**
   the exit VC — the disproof witness.
3. No compiler changes; runs on the current vox tip.

Then Milestone 2 threads now/fin (Option 2) and Milestone 3 swaps the shim's
`with_password` for `Sync.With_mutex.with_lock` and gives the contents a via
map model (Option 3) — at which point the counter is verified *while shared
across domains*, and the concurrency has never once reached Lean.
