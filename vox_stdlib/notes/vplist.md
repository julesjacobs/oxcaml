# Vplist — LANGUAGE_NEEDS notes

One block per pain-site (blueprint §5). Sites that "just worked with the
documented workaround" still get a note — that is the evidence the workaround
is load-bearing.  Vplist is the element-polymorphic `'a` list; every note
below is either a poly-specific friction (S_param) or a Vlist finding that
generalizes unchanged.

### Vplist · unspecced empty (polymorphic nullary via constructor)
- **site:** vox_stdlib/Vplist.mli (`empty`), .ml:`let empty`
- **milestone/gap:** F-B2 (poly study)
- **what I tried:** the Vlist-shaped spec `empty : (u : unit) -> 'a t{ _ = pl_nil }`.
- **error:** (from the study's `pbl_empi.ml`, re-confirmed by design) the via
  injection of the nullary `Nil` leaves the Lean datatype's type parameter `a`
  an unsolved metavariable — `don't know how to synthesize implicit argument a`
  — even at a concrete element type (`int t`). A refinement on a nullary
  polymorphic via constructor cannot pin `a`.
- **workaround used:** ship `empty : (u : unit) -> 'a t` UNSPECCED. Emptiness
  stays OBSERVABLE two ways: at runtime via `is_empty` (whose `bool` arg pins
  `a`, spec `_ = pl_isnil l`), and statically as a client `{ _ = pl_nil }`
  PRECONDITION on a list known to be empty (this is what keeps `pl_isnil_nil`
  live — see the liveness note below).
- **removed by:** poly-study ask #2 — pin a nullary via-constructor's Lean type
  parameter from the expected result type at the injection site.
- **severity:** MINOR-ERGONOMIC (one op loses its postcondition; the fact is
  recoverable by query or precondition).

### Vplist · no decidable Bool `mem` (Prop membership only)
- **site:** vox_stdlib/Vplist.mli (membership shipped as `pl_mem` + laws, NOT a `val mem`)
- **milestone/gap:** F-B3 (poly study)
- **what I tried:** the Vlist Bool op `mem : (x:'a) -> (l:'a t) -> bool{ _ = pl_mem x l }`
  with the natural impl `... | Cons (y, r) -> if x = y then true else go r`.
- **error:** NOT PROVED at the then-branch — `Goal: true = pl_mem x (pl_repr u)`
  with `u = Cons (y, r)`, `grind failed`.  On THIS compiler the wall is not the
  study's predicted `synthInstanceFailed`/`DecidableEq VoxU`: it is that the
  polymorphic OCaml `x = y` on the ABSTRACT element sort yields no threadable
  Prop equality, so the branch cannot establish `x = y ∨ …`.  Same verdict
  (Bool `mem` unshippable generically), different mechanism than the study.
- **workaround used:** ship membership ONLY as the Prop-valued model predicate
  `pl_mem` (`@[grind, expose]`) plus its laws `pl_mem_cons` / `pl_mem_app`.  A
  client STATES membership in a spec (forced in smoke by pure-Prop
  `unit{ pl_mem … = … }` goals); it cannot QUERY it at runtime.
- **removed by:** either (a) a reflected/assumed element-equality (`Veq`-style
  companion, study C-tier-2) to license the decision, or (b) modelling
  polymorphic structural `=` so the branch guard threads a Prop equality.
- **severity:** MAJOR (a whole op family — decidable membership/lookup — is out
  for `'a` containers; it is the generic/non-generic line, study F-X1).

### Vplist · empty-non-membership law is DEAD (drop it)
- **site:** vox_stdlib/Vplist.mli (a `pl_nil_not_mem` law was authored, then REMOVED)
- **milestone/gap:** new (Amendment-A dead-law family; asymmetry with `pl_isnil_nil`)
- **what I tried:** ship `pl_nil_not_mem (x) : ¬ pl_mem x (@pl_nil a)` (the
  Vlist `ll_nil_not_mem` generalized), forced by a smoke goal
  `unit{ not (pl_mem x l) }` under an `l : t{ _ = pl_nil }` hypothesis.
- **error:** none — and that is the problem.  Removal test (§6.7): the goal
  STILL passes with the law deleted.  `pl_mem` is `@[grind, expose]` (recursive,
  Amendment-A-correct) and `pl_nil` is a nullary abbreviation grind reduces
  (`pl_nil → .PNil`, whether or not `@[grind]` is present — verified by making
  it opaque, still dead), so `¬ pl_mem x pl_nil` closes by reduction alone.
  DEAD.  Contrast `pl_isnil_nil`, which is LIVE: `pl_isnil` is a NON-exposed
  match, so grind has no equations for it and the law is the only route —
  exactly the asymmetry Amendment A predicts (exposed recursive def vs opaque
  non-recursive match).
- **caveat (Vlist differs):** the SAME-shaped `ll_nil_not_mem` in the shipped
  Vlist is LIVE on this base — because its forcing goal routes through the Bool
  `mem` OP's postcondition (`mem x (empty()) = false`), not a ground pure-Prop
  term; the op-mediated `r = ll_mem x ll_nil` (fresh bool `r`) does not eagerly
  reduce the way a ground `¬ pl_mem x pl_nil` does.  Vplist has no Bool `mem`,
  so only the reducing route exists → dead.  The deadness is a property of
  Vplist's op surface, not of the law in the abstract.
- **workaround used:** do not ship `pl_nil_not_mem` (7 shipped laws, all live).
- **removed by:** the dead-axiom lint (blueprint backlog): flag a shipped law
  whose grind_pattern is discharged by reduction under the current expose set.
- **severity:** MINOR (caught by the removal sweep; the general form is the
  MAJOR-SOUNDNESS lint backlog item).

### Vplist · S_param resolution fires at every concrete element (positive)
- **site:** vox_stdlib/clients/smoke_Vplist.ml (int + string halves)
- **milestone/gap:** none (works today) — the load-bearing poly positive (F-B1)
- **what I tried:** prove every shipped law at BOTH `int Vplist.t` and
  `string Vplist.t`.
- **error:** n/a — all 8 int goals + all 8 string goals PASS.  A law proved once
  at the abstract element sort (`{a : Type}`) fires at each instantiation: the
  parameter sort `S_param` resolves through the `Tconstr` head (`int`/`string`)
  to the concrete argument sort, no per-element proof needed.
- **workaround used:** none — this is the mechanism working as designed
  (parameterized ghost sort `'a plist [@@vox.sort lean "PList"]` over
  `inductive PList (a : Type)`, pset's discipline generalized from set to list).
- **removed by:** n/a.
- **severity:** n/a (recorded as the evidence the parameterized route is real).

### Vplist · recursive via-returning append still needs the #31 skeleton
- **site:** vox_stdlib/Vplist.ml (`append`)
- **milestone/gap:** #31
- **what I tried:** the natural shape — recurse over the via type `'a t`,
  `let rec go : (u : 'a t) -> 'a t{ _ = pl_app u q }`, re-injecting the
  recursive result.
- **error:** `NOT PROVED — Goal: 0 = 0 && pl_repr q = pl_app u q`, hyps
  `t0 = Nil`, `pl_repr t0 = u`; `Application type mismatch`.  The via value `q`
  is conflated as both its own model image (`pl_repr q`) and a model element in
  `pl_app u q`.  IMPORTANT: this is UNCHANGED from the pre-#31-fix behaviour,
  and I verified the SAME failure on the MONOMORPHIC Vlist natural append on
  this base — so the gap-#31 (bind-skel) fix does NOT cover the append shape
  (Vlist.ml still ships the skeleton workaround, un-de-contorted).
- **workaround used:** thread the SKELETON at the `'a tree` level with an
  explicit image spec `pl_repr _ = pl_app (pl_repr u) (pl_repr tq)`, inject to
  `'a t` once through a variable `res` (design §7.2; identical to Vlist).
- **removed by:** a #31-family fix that actually reaches the recursive
  via-returning append (the current fix does not).
- **severity:** MAJOR-ERGONOMIC.

### Vplist · call/coercion result cannot be passed into a dependent parameter
- **site:** vox_stdlib/clients/smoke_Vplist.ml (nested op calls, let-bound)
- **milestone/gap:** C1
- **what I tried:** pass an op-call result straight into a dependent parameter,
  e.g. `Vplist.length (Vplist.cons x l)`.
- **error:** (same as Vlist) `the argument for a dependent parameter must be a
  variable or a pure expression the logic can name (let-bind it first)`.
- **workaround used:** let-bind every nested call result first.  Confirms the
  study's F-X2: C1 bites poly clients exactly as it bites int clients (not
  polymorphism-specific).
- **removed by:** auto-ANF the argument (the v1 build's #1 ergonomic ask).
- **severity:** MINOR (mechanical, pervasive).

### Vplist · statement/model typed twice (M1 + model-dup)
- **site:** Vplist.mli vs Vplist.ml
- **milestone/gap:** M1 / model-dup
- **what I tried:** the mandated obligation pattern.
- **error:** none — works as designed; the cost is duplication.  M1 = 7 (seven
  `public axiom`s in .mli restated as same-named `theorem`s in .ml).  model-dup:
  the 6 model defs (`pl_cons`/`pl_isnil`/`pl_nil`/`pl_len`/`pl_mem`/`pl_app`) +
  the `PList` inductive are restated in the .ml (sans `public`) because
  `pl_repr` and the theorems reference them.  Both are the standard taxes,
  UNCHANGED by parameterization (the `{a : Type}` binders duplicate cleanly).
- **workaround used:** verbatim-duplicate the 7 law statements (axiom in .mli,
  same-named `theorem` in .ml, identical grind_pattern) and restate the 6 model
  defs + the `PList` inductive in the .ml block sans `public`; the seal matches
  the theorems to the axioms by name+type.
- **removed by:** an obligation form stating each law once; a way for the .ml
  block to import the .mli block's model defs.
- **severity:** MINOR (accepted hygiene tax).

## eq-param layer (WP-2, 2026-07-08)

bool `mem` + `remove` + `dedup` at an ABSTRACT element via a client-supplied
decider (probe3/4): a comparator `e : 'a -> 'a -> bool` passed as a decider
`eq : (x:'a) -> (y:'a) -> bool{ _ = eqHolds e x y }` (eqHolds from Vhof). This
ESCAPES the DecidableEq-at-abstract wall at zero new TCB — the documented
headline. `mem` -> `bool{ _ = pl_memr e x l }`; `remove` -> `pl_remove_ok`;
`dedup` -> `pl_dedup_sub` (the WP-6-C gate: dedup over abstract 'a, now WRITABLE).
Decider spec params are [@vox.total]. All verify; smoke green; negatives fail
closed.

### Vplist · the full ∀↔ removespec needs e to be an EQUIVALENCE
- **site:** vox_stdlib/Vplist.ml (`remove`, spec `pl_remove_ok`)
- **milestone/gap:** new (sharpens probe4)
- **what I tried:** probe4's spec `pl_removespec e x l r := ∀y, pl_memr e y r ↔
  (¬ eqHolds e x y ∧ pl_memr e y l)` — "the result is exactly the input minus x's
  e-class". probe4 said this "needs a membership helper lemma".
- **error:** NOT PROVED for an arbitrary decider (confirmed by build: goal
  `pl_removespec e x (pl_repr u) (pl_repr t')` un-dischargeable). Root cause is
  NOT a missing helper lemma: the spec is FALSE for a non-equivalence e. If z is
  e-equal to the removed y but not to x (¬ e x z ∧ e z y), the ∀↔ demands z stay,
  but removing y drops it — only reflexive+symmetric+transitive e makes the ∀↔
  hold.
- **workaround used:** ship the honest `pl_remove_ok e x l r := (¬ pl_memr e x r)
  ∧ (∀y, pl_memr e y r -> pl_memr e y l)` — "x is gone (up to e) AND result ⊆
  input". Both conjuncts hold for ANY decider and prove by `induction ... grind`
  (grind instantiates the IH's ∀). The equivalence-strength spec is a client-block
  one-liner for a client that supplies an equivalence e.
- **removed by:** n/a — the weaker spec is correct for the general decider; the
  strong spec is available under an equivalence hypothesis.
- **severity:** MINOR (refines probe4's verdict: the elaboration wall is gone,
  but the ∀↔ spec is equivalence-gated, not lemma-gated).

### Vplist · [@vox.total] decider cannot be forwarded -> instantiate at a concrete element
- **site:** vox_stdlib/clients/smoke_Vplist.ml (dedup_int; the dropped abstract-'a client)
- **milestone/gap:** new (total-no-forward × poly)
- **what I tried:** a client generic over 'a that FORWARDS its own
  `(e [@vox.total])` decider param into `Vplist.dedup`/`mem`.
- **error:** `the argument for this parameter must be a TOTAL spec function` — a
  total param VARIABLE is not a valid total argument (only a call-site lambda /
  [@vox.reflect] value is; the WP-1 total-no-forward finding). So a fully abstract
  client cannot thread a decider.
- **workaround used:** instantiate the poly op at a CONCRETE element (int) with a
  call-site lambda decider `(fun a b -> a = b)`; the op itself is still proven
  ONCE at the abstract element (Vplist.ml). This is normal usage (dedup an int
  list). A STRING decider additionally fails: bool `=` on string carries no model
  fact (no reflected string equality — ties to the no-string-theory wall), so the
  concrete comparator must be at a type whose `=` reflects (int).
- **removed by:** a total-forwarding rule accepting a total-typed param variable
  as a total argument; and a reflected string-equality primitive.
- **severity:** MINOR (the concrete-element instantiation is the realistic client
  shape; the abstract proof lives in the module).

## POLY TRANSPARENCY FLIP (2026-07-08) — the Vlist flip applied at S_param

Assessed and DONE: the transparency flip carries to the polymorphic list.
Vplist now exposes `type 'a t = PNil | PCons of 'a * 'a t`; its Lean
correspondent is the auto-derived PARAMETERIZED inductive `Vox_Vplist_t a`
(constructors .PNil/.PCons) — no `[@vox.via]`, no ghost `'a plist`/`PList`
sort, no `pl_repr`.  Verdict: the poly sort machinery DID NOT resist — a
scratch probe (scratch_probe/plflip) confirmed the derived parameterized
inductive builds, and the full module (cons/is_empty/length/append + the
eq-param mem/dedup/remove) re-verifies with native `let rec` over `'a t`
(refine_/skeleton threading GONE).  All consumers (smoke_Vplist, effort_dedup,
smoke_Vpset) stay green with zero change (no consumer referenced the `PList`
sort name — they use the pl_* def names, which are unchanged).

The ONE residual friction is PRE-EXISTING and unchanged by the flip: the
nullary `empty` still cannot carry its `_ = pl_nil` spec — the native `PNil`
at `empty`'s result leaves the Lean type parameter `a` an unsolved
metavariable ("don't know how to synthesize implicit argument `α`"), exactly
the old via-era F-B2 (unspecced empty).  So `empty` ships UNSPECCED as before;
emptiness stays observable via `is_empty`.  This is a poly-nullary-constructor
issue, orthogonal to transparency (it bit the via version identically).
