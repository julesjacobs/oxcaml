# First-class borrowing / uniqueness / mutable-borrow in vox — design options

Author: borrowing/mode-integration quest (2026-07-06)
Repo: `vox-borrowlang` branch (clone of the `vox-proof-pane` tree), which
already carries OxCaml's shipped `borrow_` operator.

Scope: this study is about the **surface language and mode-system
integration** for unique and borrowed state — turning today's *half-trusted*
borrow story into checked, first-class constructs. It is a sibling of two
concurrent quests whose boundaries it respects: **shared-mutation semantics**
(`vox-sharedmut`: aliased-heap encodings, Verus-style points-to tokens) and
**exceptions** (`vox-exn`: exceptional exits). Where borrows meet aliasing or
exceptional exits, this doc describes the interface and defers.

---

## 0. TL;DR

vox already verifies real in-place, mutating code (in-place quicksort,
imperative tree flip, a mutable set, array reverse) against `now`/`fin`
contracts that are a hand-rolled **RustHorn/Creusot prophecy encoding**. But
the encoding lives in **per-model trusted libraries** (`slice_lib`, `bslice`,
`pslice`, `borrow_lib`, `mset_lib`, `flip_proph`'s `P` module): each declares
`now`/`fin`/`pv` as opaque Lean functions and *asserts* — with
`assume_unchecked_` + `Obj.magic_unique` — that they track real memory and
obey the prophecy laws. That is the amber. Clients built on top prove
everything honestly.

Meanwhile the **host** already gives vox, for free, the exact discipline that
*justifies* the prophecy encoding: OxCaml's uniqueness analysis is a mandatory,
sound (if conservative) post-typecheck pass, and the language has a **shipped
`borrow_` operator** (shared/aliased borrow) plus a **detailed RFC** for
mutable borrows (`&x` + `exclusive` mode + `exclusively mutable` fields,
`jane/doc/proposals/modes/data-race-freedom.md`). vox is currently *ignoring*
this: it re-asserts by hand the frame conditions the mode checker already
enforces.

The recommendation is to **consume the host's uniqueness/borrow guarantees as
license for a built-in Creusot-style `(now, fin)` prophecy calculus**, so the
prophecy laws become derived (checked) rather than assumed — and to reach it
through a small first milestone that de-ambers `slice_lib`'s
`borrow`/`split`/`drop` prophecy machinery (the qsort trusted surface),
leaving only the raw array-cell memory model trusted (which is the
shared-mutation sibling's problem, not ours).

---

## Part I — The trusted-today inventory (what is amber, and why)

vox classifies every obligation into one of three kinds
(`typing/vox_verify.ml:18-24`):

- `Prove` — sent to Lean; a real proof obligation.
- `Runtime_check` (`assume_`) — compiled into a runtime check.
- `Assume` (`assume_unchecked_`) — **trusted outright**, never checked. The
  editor paints these amber ("trusted" badges, task #52).

In an `.ml`, `assume_unchecked_` and interface `axiom`s are the whole TCB
beyond the type-checker and Lean. Every borrow demo's trust reduces to a fixed
handful of `assume_unchecked_` sites inside a *trusted borrow library*, whose
signatures the clients then prove against.

### The one shared idiom: RustHorn prophecy in a trusted library

Every mutating vox demo is built the same way. A trusted library declares:

- an **owned** carrier type (`varr`, `vref`, `vtree`) whose ghost is its model
  (`cts`, contents; `model`, a tree; `setof`, an `ISet`);
- a **loan** type (`slice`, `mut`, `tmut`) threaded `@ local unique`, whose
  ghosts are the RustHorn pair `now` (current contents) and `fin` (prophesied
  final contents);
- a **prophecy** token (`proph`) consumed `@ unique`, whose ghost `pv`/`p` is
  the value it will resolve to;

and then asserts, per operation, the prophecy laws with `assume_unchecked_`.
Representative signature (from `lib/borrow_lib.mli:53-70`, the single-cell
case):

```ocaml
val borrow_mut :
  (p : proph) @ unique -> (x : vref) @ unique ->
  ((m : mut{ now _ = x && fin _ = p }) @ local unique -> 'b @ unique)
    @ once local ->
  (vref{ _ = p } * 'b) @ unique
val mset : (m : mut) @ local unique -> (w : int) ->
           mut{ now _ = w && fin _ = fin m } @ local unique
val mdrop : (m : mut) @ local unique -> unit{ fin m = now m }
```

and the trusted body (`lib/borrow_lib.ml:24-77`) is nothing but
`assume_unchecked_ (Obj.magic_unique ...)` casts.

### What exactly is trusted (the amber), enumerated

For each op in a borrow library, three distinct things are being assumed:

1. **Memory model** — that the ghost function tracks the real mutable cell:
   `mset m w` really writes the box so `now` advances to `w`. This is the
   McCarthy-store / heap axiom layer. (Shared with the shared-mutation
   sibling — a mutable array/cell's `upd`/`elem` model.)
2. **Prophecy laws** — the RustHorn discipline: `borrow`'s entry
   (`now m = x && fin m = p`), every write's frame (`fin _ = fin m`
   unchanged), and `drop`'s resolution (`fin m = now m`). These are *logical*
   axioms, not memory facts.
3. **Mode-cast soundness** — the `Obj.magic_unique` retypes lean on OxCaml's
   uniqueness/locality being honestly enforced (so the loan really is unique
   and really can't escape). vox does not *check* this; it trusts the library
   author wrote the modes correctly and the host enforces them.

### The five trusted demos and their amber budgets

| Demo | Trusted library | `assume_unchecked_` sites | What the client PROVES on top |
|---|---|---|---|
| In-place quicksort (`demo/lean_qsort.ml`) | `demo/slice_lib.{mli,ml}` (int array segments) | ~10 fns: `anew/alen/aget`, `borrow`, `slen/sget/sset`, `split/split3`, `sdrop/sdropa` | `sorted (fin m) && perm (now m) (fin m)`, sequential AND parallel (`Par_lib.fork_join2`); reborrow via `split3`; ~500-line list-theory prelude all proved |
| Imperative tree flip (`demo/lean_flip_proph.ml`) | inline `P` module (tree loans) | ~7 fns incl. `borrow_left/borrow_right` (child reborrows), `tswap_kids`, `tdrop` | `tfin m = mirror (tnow m)`; roundtrip = identity |
| Array reverse (`demo/lean_reverse.ml`) | inline `A` module (McCarthy array) | 5 fns (`anew/alen/aget/apeek/aset`) | full reversal spec; **no prophecy** — pure `@ unique` strong-update threading + McCarthy `upd` store axioms |
| Mutable set behind `via` (`lib/mset.{mli,ml}`) | `lib/mset_lib.{mli,ml}` (ISet loans) | 6 fns (`empty/borrow/sinsert/smem/sdrop`) | `mset.ml` proves `insert`/`member` with **ZERO** `assume_unchecked_` — the `via` seam hides the whole borrow discipline behind `type t : value refines (iset)` |
| Bucket-array hash table | `lib/bslice.{mli,ml}` (Htbl `table` loans), generic `lib/pslice` (`[@@vox.poly]`) | 6–7 fns | table model reused verbatim from `Htbl`; `pslice` is ONE trusted lib for every element type |

Two structural observations that shape the options:

- **The prophecy laws are identical across all five libraries** — only the
  model sort changes (`List Int`, a tree datatype, `ISet`, `Htbl.table`).
  That uniformity is the argument for a *built-in* calculus: today each model
  re-pays the same ~6–10 axioms.
- **Reverse proves that prophecies aren't always needed.** A single
  `@ unique` value threaded with strong updates (`aset`'s result `= upd a j w`)
  needs only the memory model, no `now`/`fin`. Prophecies buy you *reborrows*
  (lending a sub-structure and getting the parent back advanced) and
  *shared/parallel* sub-loans — not plain sequential ownership.

### Adjacent (sibling) trusted surface, for reference

`lib/pcell_lib.mli` models **interior mutability** with Verus-style
PCell/PointsTo separation tokens (`itoken` consumed `@ unique`, ghosts
`cid`/`tid`/`cts`), *not* prophecies. This is the aliased-heap story and
belongs to `vox-sharedmut`; I flag it because "borrow a cell that lives in a
shared structure" is exactly the seam between the two quests (see §7).

---

## Part II — OxCaml mode-system guarantees (and the borrow-RFC verdict)

(Full findings from a dedicated read of `typing/` + `jane/doc/`; citations
below.)

### Uniqueness

`unique` means **"there is only one reference to this value"**; its dual is
`aliased` (`jane/doc/extensions/_05-modes/intro.md:201-209`,
`_07-uniqueness/intro.md:16-26`). It is a *deep, path-granular* property:
uniqueness analysis tracks consumption at the granularity of projection paths
(fields, sub-fields), not whole identifiers
(`typing/uniqueness_analysis.ml:67-71`;
`_07-uniqueness/reference.md:43-59`).

Crucially for the TCB question: uniqueness is enforced by a **mandatory,
separate soundness pass that runs after type-checking** and *rejects* programs
that violate it — it is not an optimization and not optional when the feature
is on (`typing/uniqueness_analysis.ml:16-48`, invoked from
`typing/typecore.ml:12938-12994`). So if you already trust the OxCaml
type-checker (vox runs *after* it, over the final typedtree), the uniqueness
guarantee comes along at **no additional trust cost** — modulo four documented
holes:

1. **Uniqueness mode-crossing** — a value whose type has no
   overwritable memory locations (`int`, immediates, functions) may be used as
   `unique` *even when aliased* (`_07-uniqueness/intro.md:150-153`). This is
   **benign for borrows**: strong update is vacuous on immutable/immediate
   data, so "unique ⇏ unaliased" here never licenses an unsound frame.
2. **`@@ aliased`-modality fields** — a `unique` record may hold an aliased
   field; reads of it yield `aliased` (`intro.md:50-67`). vox must **not**
   treat such a field as strong-updatable through a unique receiver.
3. **A documented pattern-match unsoundness** being worked around
   (`_07-uniqueness/intro.md:155-182`) — about use-after-free when returning
   an aliased field while freeing the scrutinee. This is a memory-safety hole
   in the host, orthogonal to vox's *logical* framing, but worth naming.
4. **Conservatism** — consumed-child tracking is lost across opaque function
   calls / fresh allocations (`reference.md:83-141`). This rejects valid
   programs; it never mis-accepts. Safe for a verifier.

`unique`/`aliased` (a *past* axis on values) is **orthogonal** to
`once`/`many` (a *future* axis on closures: a closure capturing a `unique`
value is `once`) (`_05-modes/intro.md:229-248`). vox already uses `@ once
local` on borrow continuations to force one resolution per bracket.

### Locality

`local`/`global` tracks **escape from a region** (each function/loop body is a
region). The type-checker soundly forbids `local` values from escaping their
region — this is the basis of stack allocation
(`_05-modes/intro.md:78-107`, `_02-stack-allocation/intro.md:36-49`). This is
what makes a `@ local unique` loan *unable to outlive its bracket*, which is
precisely why the residual "the ref viewed at the final value" is honest. vox
trusts this today (via `Obj.magic`) rather than consuming it.

### Borrowing — VERDICT: **OxCaml has borrow syntax. vox should design against it, not invent a rival.**

Two layers exist:

- **Shipped: the `borrow_` operator** (this tree). `borrow_ x` temporarily
  uses a `unique` value as **`aliased` and `local`** for an implicit *borrow
  region*, recovering it to `unique` after
  (`jane/doc/extensions/_07-uniqueness/borrow.md`). Regions are the RHS of a
  `let`, a function argument, or a `match` scrutinee. This is the **shared /
  read borrow**. Fully wired: `parsing/lexer.mll:59`,
  `parser.mly:2971-2972`, `Pexp_borrow`/`Texp_borrowed`,
  `typecore.ml:7664-7690` with region kind `Borrow` (`mode.ml:4728`).
  Two conservative gaps the doc flags: closure capture is treated as a
  permanent aliased use (not yet a borrow), and the locality axis conflates
  stack-`local` with borrow-`local` (both slated to be fixed).

- **RFC: `&x` + `exclusive` + `exclusively mutable`**
  (`jane/doc/proposals/modes/data-race-freedom.md:431-653`). This is the
  **mutable borrow** — the Rust `&mut` analogue. `&x` borrows `x` as
  `exclusive` for an implicit region; `exclusive` means **"not aliased with
  any other value active in the current region"** (`:568-571`). An
  `exclusively mutable` field can be written `x.a <- v` when `x` is
  `exclusive`, *without* threading the unique value through every call
  (`:548-598`). The soundness statement that matters to us: *"if you are
  reading the value of an `exclusively mutable` field then no other piece of
  code can be updating it"* (`:649-651`) — determinism of the final value.

**This is the load-bearing finding.** vox's `@ local unique` loan + `now`/`fin`
is a *hand-rolled stand-in for the not-yet-shipped `exclusive`/`&x` mutable
borrow.* When `exclusive` lands, a function `foo : (exclusive x : t) -> unit`
called `foo &x` is *exactly* the mutable-borrow-with-contract we want to spell.
The design must be a first-class citizen of that world, not a parallel notion.

---

## Part III — Creusot `&mut = (current, ^final)` ⇔ vox `now`/`fin`

The strongest candidate semantics for checked mutable borrows is the
**prophecy encoding** from RustHorn (Matsushita, Tsukada, Kobayashi, ESOP
2020) as realized in **Creusot** (Denis, Jourdan, Marché, 2022). vox's
`now`/`fin` is already this encoding — the correspondence is essentially
exact, which is why adopting it as a built-in is low-risk.

| Creusot / RustHorn | vox today | Notes |
|---|---|---|
| `&mut T` value = pair `(*x, ^x)` | loan `m : slice` with ghosts `(now m, fin m)` | `*x` = current dereference; `^x` = *final* value at end of borrow |
| current value `*x` | `now m` | reads (`sget`) return `elem (now m) i`; writes advance it |
| prophecy `^x` (fixed at creation, observed at end) | `fin m` (and the explicit `proph` token `pv p`) | vox reifies the prophecy as a first-class `@ unique` token so one prophecy resolves one borrow; Creusot's is implicit in the `&mut` |
| write `*x = v` updates `*x`, leaves `^x` | `sset`: `now _ = upd (now m) i v && fin _ = fin m` | frame on `fin` is the RustHorn invariant "the prophecy is untouched by writes" |
| **`resolve(x)`** at end of lifetime: assert `^x = *x` | **`sdrop m`**: `unit{ fin m = now m }` | *the* prophecy-resolution rule |
| reborrow `&mut *x` nests prophecies | `split`/`borrow_left`: parent residual `now _ = plugl (tnow m) (tpv p)` in terms of the child's prophecy, parent `fin` threaded | nested prophecy = child's `pv` feeds the parent's `now` |
| soundness rests on rustc's borrow checker: the borrow is **unique**, so `^x` is a deterministic function of the borrow's own writes | soundness rests on `assume_unchecked_` **plus** OxCaml `@ local unique` (enforced but not *consumed* by the logic) | see below |

**Two differences, both decisive for the design:**

1. **Where resolution happens.** Creusot *auto-inserts* `resolve` at the
   borrow's end-of-lifetime, which it reads off rustc's borrow analysis. vox
   makes it a **manual** `sdrop`/`mdrop`, and notes an undropped borrow is a
   *sound leak* (`pv` just stays opaque; `lib/slice_lib.mli:27-29`). This is
   the single biggest ergonomic gap — and it is exactly what OxCaml's **borrow
   region has already delimited syntactically.** A mode-consuming design can
   auto-resolve at region exit, matching Creusot.

2. **The soundness link is missing inside vox.** In Creusot the prophecy
   soundness *is* the borrow checker's uniqueness theorem, mechanically. In vox
   the two facts coexist but are unconnected: the library *asserts* the
   prophecy laws (`assume_unchecked_`), and *separately* the mode checker
   enforces `@ local unique`, but nothing inside vox says "because this is
   unique+local, the prophecy encoding is valid." Closing that gap — making
   the prophecy calculus a *derived rule licensed by the mode judgement* — is
   the whole game.

---

## Part IV — Design options

Notation used below for vox surface (my proposed spellings; vox uses `now`/
`fin`/`_` today):

- `&mut s` — a mutable (exclusive) borrow of `s`; contract may mention `now s`
  and `fin s`.
- `&s` — a shared (read) borrow; contract may mention `now s` only.

### Option 1 — MODE-CONSUMED PROPHECY BORROWS (the destination)

**Core idea.** Teach vox to read OxCaml's uniqueness/locality/borrow results
and treat an `exclusive`/`&x` mutable borrow as a built-in Creusot `(now, fin)`
prophecy pair. The borrow *region* (which the host already delimits) is the
prophecy's lifetime; resolution is **auto-inserted at region exit**. No
per-model trusted library, no `assume_unchecked_`, no `proph` token to thread
by hand. Literature: RustHorn/Creusot (prophecies), Mezzo (Balabonski,
Pottier, Protzenko — permission-passing over a mode/permission system), and
the "modes as a permission discipline" line that OxCaml itself descends from.

**(b) Surface syntax.** A function taking a mutable borrow with a two-state
contract:

```ocaml
(* against the shipped RFC: exclusive param, in-place field write *)
val push : (s : &mut stack) -> (x : int) -> unit{ fin s = push_model x (now s) }
val pop  : (s : &mut stack) -> int{ fin s = tail_model (now s)
                                    && _ = head_model (now s) }
```

At the call site the user writes the host's borrow syntax; vox reads it:

```ocaml
push &s 42;          (* region = this application; fin s resolved here    *)
let h = pop &s in    (* region = the let; s recovered to unique after     *)
use_unique s
```

Shared (read-only) borrow — contract mentions only `now`, no prophecy:

```ocaml
val peek : (s : &stack) -> int{ _ = head_model (now s) }
```

Reborrows are just nested `&`: `push &(s.left) x` inside a borrow of `s`
produces a nested prophecy automatically. Borrow scopes are the host's
regions; vox does not introduce its own scoping keyword.

**(c) What the mode system discharges vs. what becomes a VC.** This is the
crux — stealing frame conditions from mode checking:

- **Discharged by modes (no VC):** (i) *non-interference* — an `exclusive`
  borrow is "not aliased with any value active in the region"
  (`data-race-freedom.md:568-571`), so vox may frame **everything else** across
  the mutation with no havoc and no footprint VC. (ii) *residual honesty* — the
  loan is `local`, cannot escape the region, so at region exit the owned value
  *is* the loan at `fin`. (iii) *one-resolution* — `exclusive`/`unique` +
  region gives exactly one live borrow of the path, so the prophecy needn't be
  a manual `@ unique` token.
- **Becomes a checked VC:** (i) the two-state contract itself
  (`fin s = push_model x (now s)`) at the function's exit; (ii) preconditions
  on `now s`; (iii) the memory model of the primitive write (`x.a <- v`
  advances `now` by a field store) — this is the only irreducible axiom, and it
  is the **shared-mutation sibling's** heap model, not ours.

**TCB / mode⇒logic interface.** Consuming `exclusive`/`unique` for framing is
**type-safety-adjacent and nearly free**: uniqueness analysis is a mandatory
sound pass vox already runs behind (Part II), so trusting it adds **no axiom
beyond the type-checker vox already trusts** — a categorical improvement over
today's per-library `assume_unchecked_`, which are pure unchecked axioms. The
residual TCB is: (a) the four documented uniqueness holes (§II) — of which only
`@@ aliased` fields (#2) can bite a borrow, and vox must refuse to
strong-update through them; (b) the raw heap-store model (shared with
`vox-sharedmut`). *Caveat to verify:* vox builds must run with the `Unique`
extension maturity gate on so the pass actually executes
(`typing/typecore.ml:12938-12946`) — the demos already type-check `@ unique`,
so it is live, but this should be asserted explicitly.

**(d) VC generation.** Model each `&mut s` binding as introducing two logical
names `now_s`, `fin_s` with entry fact `now_s = <owned model of s>` and `fin_s`
fresh-opaque. Each in-place write emits a **ground** update fact
(`now_s' = store(now_s, ...)`, `fin_s' = fin_s`) — never a quantified frame
(the memory note is emphatic: grind cannot instantiate ∀-frames at goal
indices; use McCarthy stores / ground threading, as `lean_reverse.ml` does).
**Prophecy resolution rule:** at region exit, emit `fin_s = now_s` (Creusot's
`resolve`) — auto-inserted, discharged by locality. Reborrow of `&mut s.f`:
introduce child `(now_c, fin_c)`; on region exit splice
`now_s' = plug_f(now_s, fin_c)` (parent's current updated by child's resolved
prophecy) and thread `fin_s' = fin_s`. Version threading / havoc-join: because
non-interference is discharged by modes, a join over branches only needs to
unify the per-branch `now`/`fin` names (as `lean_borrow.ml:branch_even` does
by hand today), not havoc a heap.

**(e) Lean encoding + grind survival.** Identical to today's proven encoding:
`now`/`fin`/`pv` stay opaque `Model`-valued functions; all per-statement facts
are ground equations; the model theory (list/tree/set lemmas) is the proved
prelude with `grind_pattern`s. The *only* change is provenance: the prophecy
laws move from `assume_unchecked_`-emitted facts to compiler-emitted derived
facts. Grind survival is unchanged because the fact *shapes* are unchanged.

**(f) De-ambered demos.** **All of them.** qsort, flip, reverse, mset lose the
prophecy-and-residual amber entirely; only the raw heap-store axiom remains
(and that is the sibling's). This is the maximal green.

**(g) Incremental path + composition.** Requires the host's `exclusive`/`&x`
to ship (or an interim: recognize the existing `@ local unique` loan
convention, §Option 4). Composes with `via` for free — `mset` already borrows
*at the image sort* (`lib/mset.ml`), and a built-in calculus that is
model-sort-agnostic inherits that. Composes with `vox-sharedmut` at the
heap-store axiom (their model, our calculus). Composes with `vox-exn` at
resolution: an exceptional exit from a borrow region must still resolve (or
soundly *not* resolve — a leaked prophecy), which the region-exit rule handles
uniformly if exit edges are modelled.

### Option 2 — EXPLICIT BORROW REGIONS (`borrow s as b in ...`)

**Core idea.** A vox-level scoped construct with entry/exit VCs, **independent
of the host's borrow feature** — works on today's compiler. This is the
current CPS bracket (`borrow_mut p x (fun m -> ...)`) promoted to real syntax,
with *checked* entry/exit instead of `assume_unchecked_`. Literature: the CPS
bracket is exactly a scoped separation-logic "borrow" (Charguéraud-style
frames); the region is the frame's scope.

**(b) Surface.**
```ocaml
borrow s as b in         (* entry: now b = model s; fin b fresh *)
  set b (push_model x (now b));
  ...                    (* s not usable here *)
                         (* exit: check body established fin b; s' = fin b *)
```
Shared borrows: `read_borrow s as b in ...` (no `fin`). Reborrows: nested
`borrow (sub b) as c in ...`. Scopes are the `in` body — explicit and visible.

**(c) Modes vs VC.** Independent of mode *inference*: vox enforces the borrow
discipline with its **own** scoping (the borrowed var is removed from the
context inside the region, restored at exit — a linear check vox already has
the machinery for). But the *memory* soundness (strong update = no interfering
alias) still needs a license: either trust `@ unique` on `s` (a small, local
TCB item), or emit a runtime aliasing check. So this option is **more
annotation, weaker automation** than Option 1: the user writes the region, and
one memory-model assumption per borrowed type remains.

**(d) VC generation.** Entry VC binds `now b`, `fin b`; body writes thread
`now` (ground stores); **exit VC**: (i) the region-close obligation
`residual = fin b` and (ii) resolution `fin b = now b` — both *checked* here,
not assumed. This is the current library's `borrow`/`drop` behaviour with the
`assume_unchecked_` replaced by an emitted obligation.

**(e) Lean.** Same ground-fact encoding as Option 1.

**(f) De-ambered demos.** The *prophecy* amber (borrow entry/exit, drop
resolution, reborrow glue) turns green; the raw heap-store axiom remains
trusted (same residual as Option 1). qsort/flip/mset lose their `borrow`,
`split`/`borrow_left`, `sdrop` amber. Net: same green as Option 1 for the
prophecy layer, but achieved without consuming mode inference — at the cost of
explicit region syntax and a retained per-type memory assumption.

**(g) Path/composition.** Buildable **now**, no host dependency — this is the
natural *fallback* for constructs the host's borrow feature can't yet express
(e.g. the closure-capture gap `borrow.md:159-191` flags). Composes with `via`
and the sibling exactly as Option 1.

### Option 3 — TWO-STATE CONTRACTS (implicit `now`/`fin` on mutable arrow positions)

**Core idea.** Rather than a borrow *construct*, make it a **calling
convention**: every `@ unique` (or `&mut`) parameter of mutable type
*implicitly* carries a `(now, fin)` pair; the function contract relates them;
and at each **call site** vox generates the exit obligation (the residual
equals the callee's `fin`) as a checked VC. This is the current slice-framing
(`slen`/`sget`/`sset` all write `now _ = ... && fin _ = fin m` by hand)
*generalized and checked*. Literature: Dafny/Viper two-state postconditions
(`old(x)` vs `x`); Creusot's function contracts over `&mut` params.

**(b) Surface.** No new keyword; the contract syntax carries it:
```ocaml
val bump : (s : int ref) @ unique -> unit{ fin _ = now _ + 1 }
(* call site: *)
bump s;   (* vox threads: s' has now' = fin(from bump) *)
```

**(c) Modes vs VC.** The `@ unique` on the param is the license (same interface
as Option 1); the two-state *threading* at call sites becomes generated VCs.
Less than Option 1 discharges (no region-exit auto-resolution — the "final" is
just the post-state, no prophecy over a *future* lifetime), but it needs no
borrow region at all. Good for straight-line sequential ownership (the reverse
style); *cannot* express reborrows or shared sub-loans (those genuinely need
prophecies over an open borrow, i.e. Option 1/2).

**(d) VC generation.** `now`/`fin` become `old`/`new` around each call;
threading is post-state substitution. No prophecy resolution rule (there is no
open borrow), which is why it's simpler but strictly less expressive.

**(e) Lean.** Ground pre/post equations; trivially grind-friendly.

**(f) De-ambered demos.** `lean_reverse.ml`'s `A` module (the strong-update,
no-prophecy style) loses its amber cleanly. qsort/flip/mset do **not** fully
de-amber under Option 3 alone — their reborrows need Option 1/2.

**(g) Path/composition.** This is the **cheapest first increment** and it is a
strict subset of Option 1's VC generation — build it first, then add the
open-borrow prophecy layer. Composes with Option 1/2 (it *is* their
non-reborrow special case).

### Option 4 — BLESSED BORROW-PRIMITIVE CHECKING (`[@vox.borrow]`) — the bridge

**Core idea.** Keep the trusted-library *architecture*, but let vox **verify**
the library's `assume_unchecked_` claims against a built-in prophecy calculus
when the signature is tagged. A fixed vocabulary of primitives
(`borrow`/`get`/`set`/`drop`/`split`) is `[@vox.borrow]`-annotated; vox knows
their `now`/`fin` laws as a *schema* and checks the OCaml body implements them
(using `@ unique`/`@ local` for the strong-update/non-escape parts, and the
underlying owned-model for the rest). Turns "6 trusted functions" into "6
functions checked against a fixed borrow calculus."

**(b) Surface.** Library author writes today's signatures plus a tag:
```ocaml
val sset : (m : slice) @ local unique -> (i : int) -> (v : 'a) ->
  slice{ now _ = upd (now m) i v && fin _ = fin m } @ local unique
  [@vox.borrow set]
```
Clients are **unchanged** — they still see `slice`/`now`/`fin`.

**(c) Modes vs VC.** Same interface as Option 1, but scoped to the blessed
primitives instead of the whole language. The memory-store axiom is still
assumed (sibling's), but the *prophecy laws* are checked against the schema.

**(d)/(e)** As Option 1, restricted to the tagged primitives; Lean encoding
identical.

**(f) De-ambered demos.** Turns the *prophecy* half of every library
(`borrow`/`split`/`drop` and the `fin`-frame on writes) from assumed to
checked, across qsort/flip/mset/bslice/pslice at once — while the raw
`Obj.magic`/store cast stays as the single remaining per-type memory
assumption. Strictly more green than today, strictly less than Option 1's
"no library at all," but **buildable now** and with the smallest blast radius.

**(g) Path.** This is the **incremental bridge to Option 1**: it validates the
built-in calculus against the five existing libraries (a ready-made
regression suite) before that calculus is wired to the host's `&x`/`exclusive`.

### Option 5 — FULL RUST-STYLE LIFETIMES — *rejected*

**Assessment: does not earn its complexity in an OCaml-modes world.** Rust
needs explicit lifetime variables `'a`, lifetime parameters on types, and a
borrow checker because it has no other region discipline. OxCaml **already has
the region machinery**: the locality axis *is* the region, borrow regions are
delimited by the host, and non-interference comes from `exclusive`/`unique`.
Re-introducing lifetime variables in vox would duplicate the host and force
users to annotate what the mode checker already infers. The honest verdict:
lifetimes are the *host's* job; vox's job is to *consume* the host's region and
uniqueness results (Option 1). The one thing lifetimes give that regions don't
— *returning* a borrow that outlives the call — is exactly what OxCaml
deliberately forbids (`local` can't escape), so there is nothing to recover.

---

## Part V — Options comparison

| | Core idea | De-ambered demos | Automation (user writes) | TCB delta vs today | Host dependency | Effort |
|---|---|---|---|---|---|---|
| **1. Mode-consumed prophecy** | host `&/exclusive` ⇒ built-in `(now,fin)`, auto-resolve at region exit | **all** (qsort, flip, reverse, mset) — only heap-store axiom left | least (host borrow syntax only) | **near-zero**: consumes a mandatory sound pass; drops all prophecy `assume_unchecked_` | needs `&x`/`exclusive` to ship | high |
| **2. Explicit borrow regions** | `borrow s as b in` with checked entry/exit | prophecy layer of all; heap-store + per-type `@unique` memory assumption left | region syntax by hand | drops prophecy axioms; keeps one memory assumption/type | **none — today** | medium |
| **3. Two-state contracts** | implicit `now`/`fin` on `@unique` params, call-site threading | reverse fully; qsort/flip/mset only partly (no reborrow) | contracts only, no regions | drops sequential-ownership axioms | uses `@unique` (free) | **low** |
| **4. `[@vox.borrow]` blessed primitives** | check the library's prophecy laws vs a schema | prophecy half of all five libraries | one tag per primitive | drops prophecy axioms; keeps store cast | **none — today** | low–medium |
| **5. Full lifetimes** | Rust `'a` + borrow checker in vox | (n/a) | most | large | rival to host | rejected |

---

## Part VI — Recommendation and first milestone

**Recommendation.** Aim for **Option 1** as the north star — it is the only
option that makes the prophecy laws *free* (consumed from a pass vox already
trusts) and that is a first-class citizen of the `&x`/`exclusive` world OxCaml
is heading to. Reach it along a spine of shippable increments that are each a
strict subset:

1. **Option 4** (`[@vox.borrow]`) to build and *regression-test* the built-in
   prophecy calculus against the five existing trusted libraries **now**,
   before the host feature lands.
2. **Option 3** (two-state contracts) as the sequential-ownership core of the
   same calculus, de-ambering the `reverse` style with the least machinery.
3. **Option 2** (explicit regions) as the mode-independent surface for
   open borrows/reborrows, and the permanent fallback for host gaps
   (closure capture, stack/borrow locality split).
4. **Option 1** wiring the calculus to `&x`/`exclusive` and auto-resolution at
   region exit once the host ships them.

Keep Option 2 available permanently: it is the only fully mode-independent path
and covers exactly the constructs `borrow.md` says the host can't yet express.

**First milestone (smallest change that turns qsort's assumed slice-framing
into checked).** Implement the **built-in prophecy-resolution rule** as the
first slice of Option 4, targeting `slice_lib`'s three purely-logical ops:

- Teach vox a `Borrow`/prophecy builtin so that, for a `@ local unique` loan
  with `now`/`fin` ghosts and a `@ unique` prophecy token, the compiler
  **emits** (rather than the library asserting): `borrow`'s residual
  `cts _ = pv p`, `split`'s recombination `now _ = app (pv pl) (pv pr)`, and
  `drop`'s resolution `fin m = now m` — each *discharged* from (a) the owned
  model `cts` of the root and (b) the locality guarantee that the loan does not
  escape the bracket.
- **Validation:** delete the `assume_unchecked_` from `slice_lib.ml`'s
  `borrow`, `split`/`split3`, and `sdrop`/`sdropa` and confirm
  `demo/lean_qsort.ml` still verifies end-to-end (sequential and parallel).
  The remaining `assume_unchecked_` in `slice_lib.ml` — `anew`/`aget`/`sset`'s
  raw array-cell stores — stay amber and are explicitly handed to
  `vox-sharedmut` (the heap-store model), not de-ambered here.

Net effect: qsort's trusted surface shrinks from ~10 asserted functions to the
~3–4 raw array-cell stores, and the *borrow discipline itself* becomes checked
compiler-emitted logic — the first concrete amber→green flip, with a built-in
calculus that the remaining milestones extend rather than replace.

---

## Part VII — Composition and cross-quest interfaces

- **With `via`** (already half-designed): `lib/mset.ml` borrows *at the image
  sort* — the loan's ghosts are `ISet`, and the `via` seam hides the whole
  discipline behind `type t : value refines (iset)` with **zero**
  `assume_unchecked_` in the payoff module. A model-sort-agnostic built-in
  calculus (Options 1/2/4) inherits this: the prophecy laws are stated over an
  abstract model sort, so `via`-abstracting a borrowed carrier is free. The one
  known wart is the inline-injection sort bug (`[[via-injection-sorting-rules]]`,
  refines task #31) — let-bind before injecting.

- **With `vox-sharedmut` (aliased heap).** The seam is the **raw heap-store
  axiom**. My calculus *consumes* a memory model (`upd`/`elem` on an array, a
  field store on a record) and adds the borrow/prophecy layer on top; their
  quest *provides and de-ambers* that memory model (and the aliased case where
  a borrow's target lives in a shared structure — their Verus-style
  points-to token, `lib/pcell_lib.mli`, is what would *license* a borrow of a
  cell inside an aliased heap). Concretely: a borrow of an `exclusively
  mutable` field is *mine*; a borrow of a cell reached through a shared
  points-to token needs *their* token to establish the exclusive access first.
  **Reconcile:** agree on one heap-store interface so a borrow's `now`/`fin`
  can sit on top of either a unique array or a token-guarded aliased cell.

- **With `vox-exn` (exceptional exits).** A borrow region can be exited by an
  exception, not just normal return. The region-exit resolution rule must fire
  (or soundly *not* fire — a leaked, still-opaque prophecy is sound, per
  `slice_lib.mli:27-29`) on the exceptional edge too. If they model exceptional
  exit edges out of a scope, my resolution rule attaches to those edges
  uniformly. **Reconcile:** the borrow region's exit set (normal + exceptional)
  is the join point for both quests.

---

## Appendix — key files

- Trusted borrow libraries: `testsuite/tests/vox/demo/slice_lib.{mli,ml}`,
  `testsuite/tests/vox/lib/{borrow_lib,bslice,pslice,mset_lib,pcell_lib}.{mli,ml}`.
- Borrow demos: `testsuite/tests/vox/demo/{lean_qsort,lean_reverse,lean_flip_proph,lean_borrow}.ml`,
  `testsuite/tests/vox/lib/mset.{mli,ml}` (borrow-behind-`via`).
- VC kinds / trust classification: `typing/vox_verify.ml:18-24`,
  `typing/typecore.ml:9108-9138`.
- OxCaml modes: `jane/doc/extensions/_05-modes/`,
  `_07-uniqueness/{intro,reference,pitfalls,borrow}.md`,
  `_02-stack-allocation/`; uniqueness pass `typing/uniqueness_analysis.ml`.
- Borrow RFC: `jane/doc/proposals/modes/data-race-freedom.md:431-653`
  (`&x` + `exclusive` + `exclusively mutable`).
