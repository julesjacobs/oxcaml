# vox stdlib: language/compiler needs (probe-grounded inventory)

*Design inventory, 2026-07-06. Successor to
`2026-07-06-vox-stdlib-design.md`; refines its §4.*

This document catalogs the language/compiler improvements a genuinely nice
verified vox stdlib needs, each grounded in a probe I compiled against the
installed compiler (`_build/_bootinstall/bin/ocamlc.opt`) + pinned Lean
4.31, or in a named sighting. Probe sources are committed under
`scratch_probe/inventory/` and every one is cited by filename below.

It has a **special focus**: the *axioms-in-`.mli` / proofs-in-`.ml`*
obligation pattern, which the user has ruled the correct interface-hygiene
mechanism (interfaces state the client contract; implementation-side proof
scaffolding belongs in the `.ml`). Part 1 is the support matrix for that
pattern at stdlib scale. Part 2 is the ranked inventory of everything else.
Part 3 is the "ship v1 on today's compiler" bottom line.

Baseline for the whole document: gaps **#31** (transparent-Trefine
let-binders bind at skeleton), **#32** (refined-bool branch facts), and the
**sequence fact-threading + raise-as-⊥** work are treated as *landing*.
This clone predates them (probe `noletbind.ml`: an inline `1 + length t`
recursive measure fails — `Goal: *unknown1* = llen l, Hypotheses: l = Cons
…` — because the callee's postcondition does not surface without a
let-binding); every probe below that uses the let-bind workaround does so
only to stand in for that landing. Items those three fixes already cover
are pruned from the inventory.

---

## Part 1 — the obligation-pattern support matrix

Mechanism recap: an interface-block `axiom` is an **obligation** — the
implementation's solver input ends in a *seal* that re-elaborates the
interface block and demands a same-named, same-typed proved theorem
(`step.mli`, `oset.mli` are the shipped exemplars). A `[%%vox.lean]` block
in an `.mli` compiles to `VoxSig_<Unit>.olean`, which rides the `.cmi` to
clients. This is the machinery that lets the *statement* of a law live in
the interface while its *proof* lives in the implementation.

| # | Capability | Status | Probe / evidence |
|---|---|---|---|
| 1 | Interface `axiom` discharged by a same-named theorem in the **implementation's** block (not an inline `.mli` proof) | **works** | `mlist.{mli,ml}`: `.mli` states `llen_lapp` as `public axiom`, `.ml` proves the same-named theorem; both compile. Matches `oset`. |
| 2 | `.ml` block carries **private scaffolding** lemmas the interface never sees | **works** | `mlist.ml` proves `llen_nonneg` (absent from `mlist.mli`) with no complaint; `oset.ml` keeps `not_mem_lt`/`all_lt_insert`/… private. |
| 3 | Client receives the interface axiom as a **usable ambient fact** | **works** | `mlist_client.ml`: `total a b : int{ _ = llen a + llen b }` verifies via `Mlist.append`+`Mlist.length` without citing `llen_lapp`. |
| 4 | Attribution (`grind_pattern` / `@[grind]`) is **accepted on an axiom** and **rides** to clients | **works** | Same probe: the ride happens *because* `mlist.mli`'s axiom carries `grind_pattern llen_lapp => llen (lapp a b)`. `oset.mli` axioms carry them too. |
| 5 | An **un-attributed** interface axiom is **inert** for clients | **works as designed (hazard)** | `mbare.{mli,ml}` + `mbare_client.ml`: identical to (3) but the axiom has no `grind_pattern` → client fails `NOT PROVED — Goal: *unknown2* = llen a + llen b`. The dead-theorem lint (Part 2) should cover interface axioms, not just `.ml` theorems. |
| 6 | **Statement drift** between `.mli` axiom and `.ml` theorem is caught, with a legible message | **works** | `mdrift.{mli,ml}`: `.ml` states `… = llen a + llen a` → `vox seal: law llen_lapp: the implementation's statement does not match the interface's`. Sound rejection, clear error. |
| 7 | A **prove-only** form (prove the interface law without re-typing its statement) | **MISSING** | `mproveonly.ml`: `theorem llen_lapp := by …` → Lean parse error `unexpected token ':='; expected ':'`. The statement must be written **verbatim twice**. No referencing form exists. |
| 8 | Composes with **`via`/Trefine** models: interface states a law as an axiom over the via *image*; impl discharges it | **works** | `viaob.{mli,ml}`: `.mli` declares image model `memv`/`insv` (`public def`) + client law `memv_insv` (`public axiom`); `.ml` (a via tree) proves it and the specs via `refine_`. |
| 9 | Composes with **opaque sorts** (`[@@vox.sort opaque]`) | **works** | `mlist`/`mbare`/`mdrift` are all opaque-sort; `oset.mli` is the shipped case (own sort `Vox_Oset_t`, model constants as axioms). |
| 10 | Composes with **multiple implementations** of one interface | **works** | `mstep.mli` (opaque `step` + axiom `step_gt`) discharged by two different bodies — `mstep.ml` as `x+1` and as `x+5` — each paying the same seal; `mstep_client.ml` (`_ > x`) verifies against **either** from the law alone. This is the `step_incr`/`step_double` story, reproduced minimally. |
| 11 | Composes with **`total_`** spec functions | **BROKEN / does not compose** | `totob.mli`: `val total_ len` + a block `public axiom len_nonneg (l) : 0 <= len l` → `VoxSig_Totob.lean: Function expected at len … The identifier 'len' is unknown`. The interface block elaborates in isolation; a `total_` name is not in its scope. `total_` (name-only measure export) and block-axiom laws are **two disjoint mechanisms** — you cannot state a rideable block law *about* a `total_` measure. |
| 12 | Composes with **`[@@vox.lemma]`** / would lemma→VoxSig export subsume it | **BROKEN (cross-unit); export would help** | `lemu.{mli,ml}` + `lemu_client.ml`: a `.ml` `[@@vox.lemma] lemma_len_nonneg` does **not** reach the client — `Goal: len l >= 0, Hypotheses: <none>`. Independently reconfirms predecessor Probe A. The obligation pattern is the *only* working cross-unit fact channel today. |
| 13 | Packaging: an installed unit is **`.cmi` + `.cmo` + `VoxSig_*.olean`**, no source | **works** | Predecessor Probe B (removing the olean flips a passing client to `unknown module prefix 'VoxSig_…'`). Not re-probed here. |

**Matrix bottom line.** The pattern is *fully usable today* for the shape
the stdlib actually needs: opaque or via-abstract interfaces whose
client-facing laws are attributed axioms, discharged by proved theorems in
the `.ml` with unlimited private scaffolding, sound on drift, composing
across opaque sorts, via images, and multiple implementations. Two real
holes: **statement duplication** with no prove-only form (row 7), and the
**non-composition with `total_`** (row 11) — which together mean the only
rideable measure algebra is the one written twice as block axiom + block
theorem. `[@@vox.lemma]` remains same-unit-only (row 12).

### Migration slice: `ptrie.mli` → obligation pattern

`ptrie.mli` is the anti-pattern the user flagged: **382 lines, 24 `public
theorem`s proved inline in the interface** (`isbit_pos`, `emod_unique`,
`emod_double`, `bbit_isbit/pos/diff/agree/lt`, `isbit_dvd`, `isbit_lt_dvd`,
`mask_tele`, `zbit_mask`, `not_mem_mismatch/zero/one`,
`allmatch/allzero/allone_insert`, `allmatch_weaken`,
`allzero/allone_of_allmatch`, `trie_join`, `trie_insert`, `mem_insert`).
Of these, exactly **two are client-facing** — `mem_insert` (the membership
law) and `trie_insert` (invariant preservation); the other 22 are
bit-arithmetic scaffolding that exists only to prove those two.

The migrated shape is precisely what `oset.mli`/`oset.ml` already ship, and
what probe `mlist` re-demonstrates from scratch. Mechanical friction the
slice exposes:

- **The two client laws must be restated verbatim** — as `public axiom` in
  the `.mli` and as `theorem` in the `.ml` (row 7; no prove-only form). All
  other 22 lemmas *move* (not rewrite): they already carry
  `@[grind]`/`grind_pattern`, so in the `.ml` they keep discharging the
  client-law proofs unchanged.
- **`t` must go abstract.** `ptrie` exposes its ADT (`type t = Empty | Leaf
  | Branch`), so its model `mem`/`insert` are transparent `def`s whose
  bodies pull in the entire bit theory (`mask`/`zbit`/`bbit`/`isbit`). To
  move scaffolding out cleanly the interface must model `mem`/`insert`/
  `trie` as **opaque model constants** (axioms, `oset`-style) so their
  bodies — and the bit theory they mention — stay in the `.ml`. Result:
  ~382 lines → a ~15-line interface (model constants + 2 attributed law
  axioms), 24 theorems → 2 obligations. This is a mechanical refactor with
  no new compiler support required; it is queued as the separate
  interface-hygiene quest (task #12).

---

## Part 2 — ranked improvement inventory

Ranking: **BLOCKING** (a stdlib feature cannot exist without it) ·
**MAJOR-ERGONOMIC** (feature works but authoring is painful enough to deter
a real stdlib) · **MINOR** · **COSMETIC**. Each item: evidence, a sketch of
the compiler/language change, a rough cost, and which milestone it unblocks
(v0/v1/v1.1/v2 per the predecessor roadmap).

### BLOCKING

**B1 — `[%%vox.lean]` blocks in functor bodies (the generic ordered
container).** *Unblocks v2.* The single true blocker, confirmed and now
sharper than the predecessor's "blocks nested in a functor body are
dropped": they are **explicitly rejected**. Probe `funct.ml` (a
`Make(X)(struct … [%%vox.lean …] … end)`) fails with `vox: [%%vox.lean]
blocks are unit-level; move to the file top level`. So `Vset.Make(Ord)` /
`Vmap.Make(Ord)` over an arbitrary ordered element cannot carry the model
theory their contents need. The compiler work is two distinct pieces:
  1. *Block collection from functor bodies.* Today block harvesting is
     unit-level (one `VoxSig_<Unit>`/prelude per compilation unit). A
     functor body needs its blocks collected and elaborated **relative to
     the functor's parameter models** — i.e. a per-functor Lean section
     parameterized by the argument unit's sort(s)/defs.
  2. *Per-instantiation order discharge.* An abstract element order (`lt`
     with `LinearOrder`-style laws) must be dischargeable at each
     instantiation (`Ord = Int`). Per the via-design doc, there is **no
     compiler mechanism** for this today — OCaml polymorphic compare has no
     Lean counterpart, and Lean typeclass constraints (`LinearOrder α`,
     `DecidableEq α`) *fail closed at the solver*. The realistic design is
     a comparator whose **model carries the order** (the functor argument
     supplies `cmp : elt -> elt -> int` plus a block of order axioms as
     obligations), instantiated by importing the concrete element unit's
     VoxSig (R7-style). This is exactly the obligation pattern (Part 1)
     applied to a functor parameter.

  *Cost:* large — new elaboration path for functor-scoped blocks + a
  parameter-obligation mechanism. Genuinely v2. **v1 ships int-keyed +
  order-free-generic containers around it** (the predecessor's scoping, still
  correct).

### MAJOR-ERGONOMIC

**M1 — no prove-only / statement-referencing form for obligations.**
*Improves v0 onward.* Every client-facing law is typed **twice**,
verbatim, once as `.mli` `axiom` and once as `.ml` `theorem` (probe
`mproveonly.ml`: `theorem name := by …` is a Lean parse error). At stdlib
scale (`oset` already duplicates 3 laws; a migrated `ptrie` duplicates 2; a
real `Vset` more) this is the dominant authoring tax of the hygiene
pattern, and every duplicated statement is a future drift site (caught
soundly — probe `mdrift.ml` — but still a manual re-sync). *Change:* a
seal-side "prove-only" form where the `.ml` names the obligation and
supplies only a tactic, and the seal fills the statement from the
re-elaborated interface block (it already has that statement — that is how
it checks drift). *Cost:* small-medium, localized to the seal elaborator.
High value: it is what makes the hygiene pattern pleasant rather than
merely possible.

**M2 — cross-unit lemma export (`[@@vox.lemma]` → `VoxSig`).** *Improves
v1.* `[@@vox.lemma]` facts are same-unit-only (probe `lemu*`, `Hypotheses:
<none>` at the client; reconfirms predecessor Probe A). The clean workaround
is R3 (client laws as `.mli` block theorems/axioms), so this is *ergonomic,
not blocking* — but it forces the duplication of M1 and prevents an
implementation from exporting an internally-proved lemma without restating
it in interface form. *Change:* have `[@@vox.lemma]` optionally emit a
`public theorem` into the unit's `VoxSig` (the olean already travels — row
13). Partly subsumes M1 for the lemma case: an exported lemma need not be
re-typed as an axiom. *Cost:* small-medium.

**M3 — attribution required for a law to be live, silently (dead-theorem
lint, extended to interface axioms).** *Improves v0 onward.* A block
`theorem`/`axiom` with neither `@[grind]` nor `grind_pattern` nor a cite is
**silently inert** — checked but never in the solver fact set. Probe
`mbare*` shows this now bites *interface axioms* too: the law rides to the
client and does nothing, surfacing only as a downstream `NOT PROVED`. The
demo-audit already proposed a lint for dead `.ml` block theorems
(`2026-07-06-demo-modernization-audit.md:84-93`); it must also flag
un-attributed **interface** axioms/theorems, which are strictly more
dangerous (the failure appears in a *different unit*). *Change:* a
compile-time warning at block elaboration. *Cost:* small. Already tracked as
task #5; this widens its scope.

### MINOR

**N1 — spec/model name collisions.** *Affects v0 onward.* Two independent
hazards, both fail-closed with mediocre messages:
  - A spec name equal to a Lean core identifier: probe `collide.ml` (a def
    named `repr`) → `a non-private declaration 'repr' has already been
    declared`. Naming discipline (R6) avoids it, but the error names Lean
    internals, not the user's `.ml`.
  - Two co-imported units each `public`-declaring the same model name
    (`ISet`) collide at olean import even byte-identically (sweep F2). The
    only sound sharing is "import the one defining unit" (R7).
  *Change (optional):* the compiler could **namespace per-unit block
  declarations automatically** (prefix non-`public` names with the unit, as
  it already does for the sort `Vox_<Unit>_t`), reserving the global
  namespace for deliberately-shared `public` models. That would kill the
  core-collision hazard entirely and make the shared-model rule explicit
  rather than a naming taboo. *Cost:* medium (touches name resolution across
  the VoxSig boundary); needs care not to break the intended shared-model
  imports. Worth scoping as a v1.1 nicety.

**N2 — element equality / derived-op story for containers.** *Affects v1
(any element-generic container).* vox never reflects OCaml structural `(=)`
as image equality; an unpacked `(=)` is an uninterpreted bool (sweep). A
container keyed on element equality (a set/map over a non-`int` element)
therefore has no automatic equality model — it must take the equality/order
as an explicit spec-carrying parameter. This is the *same* need as B1's
comparator-with-model and should be designed together: a small
`ORDERED`/`EQ`-style signature whose block states the (obligation) laws
`eq_refl`, `eq_sym`, `lt_trans`, `lt_irrefl`, imported by the element unit.
*Cost:* design item, no isolated compiler change beyond B1.

### COSMETIC

**C1 — named call-result injection into a `via` type.** *v1.* Predecessor
listed direct-ascribe of a *call result* into a via type as mis-sorting
(bind to a variable first). Probe `naminj.ml` shows direct ascribe of a
**constructor expression** (`(Node (Node …, x, Leaf) : t{ _ = insn x (insn
x s) })`) now works; the residual is the call-result case, which #31 /
sequence fact-threading is expected to absorb (probe `noletbind.ml` is the
same "callee fact needs a let" family). *Action:* re-verify after #31 lands;
likely closes with no separate work.

**C2 — 0-ary spec constant in a refinement: RESOLVED (contradicts
predecessor).** *v0.* The predecessor listed "0-ary spec constant not
referenceable in a refinement" as a COSMETIC gap and used `lisnil _` in the
Vlist PoC. Probe `zeroary.ml` shows a bare 0-ary `def lnil : Vox_Zeroary_t
:= .Nil` **is** referenceable: `let empty : t{ _ = lnil } = Nil` compiles.
Either the gap was context-specific (a via *image* constant, or an `.mli`
constant) or it has since closed. **Marked resolved for plain blocks;** the
via-image-constant case is an UNKNOWN I did not isolate (see below).

**C3 — VC placeholder readability (`*unknownN*`, `*vox-wild*`).** *any
milestone.* Every failed-goal message in these probes shows synthetic names
(`*unknown2* = llen a + llen b`, `l = Cons (*vox-wild*, t)`). Cosmetic but
it degrades every authoring failure. Already tracked as task #8.

---

## Part 3 — what v1 can ship on today's compiler

**Everything except the generic ordered functor (B1).** The obligation
pattern — the interface-hygiene mechanism the user endorsed — is fully
operational: opaque and via-abstract interfaces, client laws as attributed
axioms discharged by `.ml` theorems with private scaffolding, sound on
drift, composing across opaque sorts, via images, and multiple backends
(Part 1 rows 1–10, 13). A v1 stdlib of **int-keyed / order-free containers**
— `Vlist`, `Voption`, `Vresult`, `Vset`(int, via-abstract with a real BST
backend), `Vhashmap`, `Viarray`, `Vbits` — ships today, with `oset`/`mlist`
as the interface template and `ptrie`'s 382-line interface refactored down
to the pattern (mechanical, no compiler work).

The costs a v1 author pays on today's compiler, none blocking: every client
law typed twice (M1), measure algebra expressible only as block axioms not
`total_` (Part 1 row 11), cross-unit facts only via the interface block
(M2), and naming discipline to dodge collisions (N1). Fixing #31/#32 +
sequence-threading (in flight) removes the let-binding contortions in
container *implementations*; M1 (prove-only) is the highest-value *new*
ergonomic to make the hygiene pattern pleasant.

v1.1 adds RB-tree / trie backends behind `Vset`'s interface (the
representation-independence showcase — Part 1 row 10 is its foundation) and
mutable containers behind the borrow lib. **v2** is the generic ordered
functor and needs the two-piece B1 compiler work.

### Honest unknowns (not probed)

- Whether a **via-image 0-ary constant** (vs the plain-block one in
  `zeroary.ml`) or an `.mli`-declared constant is referenceable — I isolated
  only the plain-block case (C2).
- The **actual size** of the functor block-collection work (B1) — I
  established the hard rejection and the two required pieces, not an
  implementation estimate.
- Whether M1's prove-only form is easy to bolt onto the existing seal
  elaborator or needs a new surface syntax — asserted small-medium, unverified.
- `total_`-in-functor and `[@@vox.lemma]`-in-functor interactions (moot until
  B1).

---

## Appendix — probe index (`scratch_probe/inventory/`)

| Probe | Establishes |
|---|---|
| `mlist.{mli,ml}` + `mlist_client.ml` | baseline obligation + private scaffolding + ambient client ride (rows 1–4) |
| `mbare.{mli,ml}` + `mbare_client.ml` | un-attributed axiom is inert for clients (row 5, M3) |
| `mdrift.{mli,ml}` | statement-drift caught with a seal error (row 6) |
| `mproveonly.{mli,ml}` | no prove-only form; statement typed twice (row 7, M1) |
| `viaob.{mli,ml}` | via + obligation-law composition (row 8) |
| `mstep.{mli,ml}` + `mstep_client.ml` | one interface, two bodies, body-agnostic client (row 10) |
| `totob.mli` | `total_` does not compose with a block law (row 11) |
| `lemu.{mli,ml}` + `lemu_client.ml` | cross-unit `[@@vox.lemma]` fails (row 12, M2) |
| `funct.ml` | `[%%vox.lean]` blocks rejected in functor bodies (B1) |
| `collide.ml` | spec name `repr` collides with Lean core (N1) |
| `zeroary.ml` | bare 0-ary constant IS referenceable — resolved (C2) |
| `naminj.ml` | constructor-expression injection into via works (C1) |
| `noletbind.ml` | pre-#31 clone: callee fact needs a let-binding (baseline) |
