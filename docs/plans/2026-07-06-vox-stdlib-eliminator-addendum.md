# vox stdlib — eliminator + relational-def addendum (Phase C, F-2/F-3)

*Design proposal, 2026-07-06 (integrator). PROPOSED — pending team-lead
approval before it farms to the owning builders as the Phase-C fix wave.
Addresses usability findings F-2 (via-abstract containers have no eliminator)
and near-term F-3 (no client-side quantifier) from
`vox_stdlib/LANGUAGE_NEEDS.md`.*

## The problem (F-2)

A via-abstract container ships **producers** (`empty`/`add`/`cons`) and
**point queries** (`mem`/`find`) but no **eliminator**: a client holding an
abstract `Vset.t` / `Vmap.t` / `Vlist.t` cannot enumerate, traverse, or
convert it back to concrete data. So "list the elements", "dedup a list into a
set and read it back", "iterate the bindings" are all impossible at the
interface — the reviewer's dedup task has no expressible solution.

## Design principle: first-order eliminators, NOT higher-order fold

`fold : (int -> 'acc -> 'acc) -> t -> 'acc` is out — vox does not model
function-typed arguments (an independent MAJOR gap). Every mechanism below is
**first-order**: a value out, characterized by a *shipped* membership/agreement
def (which also mitigates F-3, since the quantifier lives in the shipped def,
not the client's refinement).

### Mechanism A — `elements` into the stdlib's own `Vlist` (primary)

Return the enumeration as a **`Vlist.t`**, not a raw `int list`, and bridge
membership to the source container's model:

```
(* in Vset.mli, importing Vlist's LList model (R7) *)
@[grind, expose] public def vs_elements_spec (l : LList) (s : ISet) : Prop :=
  ∀ x, ll_mem x l = vs_mem x s
val elements : (s : t) -> Vlist.t{ vs_elements_spec _ s }
```

- The `∀` lives in `vs_elements_spec` (a shipped def), so the client writes
  no quantifier — it just gets `ll_mem x (Vset.elements s) = vs_mem x s` as an
  ambient fact and can then use Vlist's own algebra. This is the F-3
  mitigation *and* the eliminator in one.
- Returning `Vlist.t` (not `int list`) sidesteps the open question of whether
  vox models a raw OCaml `int list` with a usable `List.mem`; it reuses the
  stdlib's already-verified `LList` model. It is also a second in-stdlib R7
  composition (Vset eliminated into Vlist), symmetric to `client_set_of_list`
  building a Vset from a Vlist.
- **Ordering/dedup:** the spec is membership-only, so it says nothing about
  order or duplicates — correct and minimal. If a client needs "sorted, no
  dups", that is a *stronger* eliminator (`elements_sorted` with an
  `ll_sorted`+`ll_nodup` conjunct) — defer unless a client needs it.

### Mechanism B — uncons-style view (structural recursion for clients)

For `Vlist` specifically (a sequence, not just a set), ship a view destructor
so clients can recurse:

```
type vlist_view = VNil | VCons of int * t      (* exposed view ADT *)
val uncons : (l : t) -> vlist_view{ matches ll_isnil / ll_cons of l }
```

A client matches `VNil | VCons (x, rest)` and recurses — first-order
traversal without `fold`. (Vset/Vmap could ship a `pop_min : t -> vpop`
BST-min view, but Mechanism A covers their enumeration need more simply;
recommend B for Vlist only.)

### Mechanism C — `remove` (completes the set/map algebra)

```
val remove : (x : int) -> (s : t) -> t{ vs_removespec _ x s }
   where vs_removespec r x s := ∀ y, vs_mem y r = (y ≠ x ∧ vs_mem y s)
```
Not an eliminator, but the missing producer that makes the set/map algebra
closed (add/remove/mem). Cheap given the backend; recommend for Vset and Vmap.

## Per-module recommendation

| Module | Add | Why / notes |
|---|---|---|
| **Vset** | `elements : t -> Vlist.t` (Mech A) + `remove` (Mech C) + relational defs `vs_subset`, `vs_equal` (F-3) | enumeration + closed algebra; imports Vlist's LList (new R7 edge — probe first) |
| **Vmap** | `keys : t -> Vlist.t` (Mech A over keys) + `remove` (Mech C) + relational `m_agree` (∀ k, find k a = find k b) | `keys` (int list) dodges the pair-as-value hazard; **values-enumeration deferred** to v1.1 (needs tuple/record-valued modeling) |
| **Vlist** | `uncons` view (Mech B) | it is a sequence — clients want head/tail/traversal; a raw eliminator, no bridge needed |
| **Voption / Vresult** | *nothing* | already exposed ADTs — clients match constructors directly; fully eliminable today |
| **Vint / Viarray** | *nothing* | not containers (Viarray already has `get`/`length`) |
| **Vset_bst** | *nothing* | backend tier; not client-facing |

## Feasibility risks the builders must probe first (before implementing)

1. **Mech A cross-module model import:** does `Vset.mli`'s block importing
   `Vlist`'s `LList`/`ll_mem` (R7, `open Vlist`) elaborate, and does
   `vs_elements_spec` (a `∀` over the *imported* `LList` and the *own* `ISet`)
   discharge? The `uset`/`dcount` probe showed a downstream *interface* block
   CAN reference an upstream via-abstract model — so this is expected to work,
   but it is a NEW combination (two model imports in one face) and must be
   probed. If it fails, fall back to `elements : t -> int list` and probe
   whether vox reflects `int list`/`List.mem` (the open question Mech A avoids).
2. **The eliminator's own proof:** `elements` recurses the backend tree
   building a `Vlist`; its VC needs the bridge `ll_mem x (result) = vs_mem x
   (elems tree)` by induction — a genuine proof, not a wrapper. Budget it like
   `append` (a recursive via-returning op → the #31 skeleton-thread workaround
   applies; this clone is pre-#31).
3. **F-3 quantified relational defs (`vs_subset`/`vs_equal`/`m_agree`)** are
   just more `∀`-defs like `vs_addspec` — low risk, but confirm a client can
   *consume* `vs_subset a b` as a hypothesis/goal without writing its own `∀`.

## Scope / priority

- **Minimum viable F-2 unblock:** Mech A `elements` on Vset (+ its Vlist
  import probe) and `uncons` on Vlist — that makes the reviewer's dedup task
  expressible end to end. Do these first as the proof of the eliminator story.
- **Then:** `remove` (Vset, Vmap), `keys` (Vmap), and the F-3 relational defs.
- **Deferred to v1.1:** higher-order `fold`/`map` (needs the function-arg
  model), Vmap values-enumeration (needs pair/record modeling), sorted/nodup
  strengthened eliminators (only if a client needs them).
- Each new op ships with a smoke client forcing its spec, and the Vset
  `elements` gets a cross-module client that enumerates into a Vlist and reads
  membership back (the F-2 acceptance analogue of `client_set_of_list`).
