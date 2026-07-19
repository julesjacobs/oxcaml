# DT+LIA combination poison — diagnosis (task #47, bugreport 03 followup)

Branch `task/dtlia-combine` off trunk `40e8d7392f`. Repro confirmed on the trunk binary:
`dt_plus_int_order.smt2` → `(verdict unknown) (unknown-reason poison-solve:Invalid_argument)`
after 1 propagation; `dt_only` and `int_only` controls both unsat.

## Root cause (pinned, not inferred)

1. **Theory selection is all-or-nothing.** `Cdclt.ensure_theory` (cdclt.ml:502-526) installs
   EXACTLY ONE theory, chosen syntactically at the first intern:
   - arrays registry non-empty → `TArr` (standalone arrays)
   - **datatype registry non-empty → `TDt` (standalone DT)**  ← taken here
   - else → `TCombined` = `Combine(Uflia_router)(Euf_adapter)(Lia_adapter)` (Nelson-Oppen EUF+LIA)
   The datatype branch (cdclt.ml:520-521) fires for ANY datatype declaration, regardless of
   whether the query also uses arithmetic.

2. **The standalone DT theory has no arithmetic.** `Oxsmt_dt.Dt` (dt.mli:1-26) owns an `Euf.t`
   and layers the 4 datatype axioms (distinctness/injectivity/selector-eval/acyclicity). It has
   NO LIA. An order atom `(> k 0)` classifies as `K_foreign` (dt.ml:283); `register_atom` makes
   it a no-op (dt.ml:308); then at solve time `assert_lit` on it raises
   **`invalid_arg "Dt.assert_lit: a foreign (non-DT) atom must not be asserted"`** (dt.ml:339) →
   the engine's CONTRACT-POISON turns it into `unknown`. That is the exact `Invalid_argument`.

So the mixed query needs BOTH theories, but the session installs only DT.

## Correction to the charter's premise (verify-don't-trust)

The assignment states "the quantified UFDT corpus runs through combine." **That is not what the
code does.** `ensure_theory` is the sole theory selector and routes every datatype registry to
standalone `TDt` unconditionally — there is no quantifier/UFDT override. The quantifier matcher
for a datatype session reads DT's OWN congruence-query API, not `Combine`'s: `live_egraph_view`
(cdclt.ml:832-837) builds the e-graph view from `Dt.app_terms_by_symbol` / `Dt.find_class_opt` /
`Dt.class_members`, distinct from the `Combined.congruence_state` path used for EUF/LIA
(cdclt.ml:813). UFDT = standalone `TDt` + the `Manager` quantifier layer; DT never goes through
`Combine`. **Consequence:** there is no existing DT+LIA integration to reuse — the fix must
create real combination, and any UFDT query that also touches arithmetic poisons today too (same
`K_foreign` path).

## Fix shape (recommended) — DT as the congruence child of `Combine`

The `Combine` functor (combine.mli:248-253) is binary: a congruence child `A`
(`FABRIC_CONGRUENCE_CHILD`) + an arithmetic child `B` (`FABRIC_CHILD`), with a `ROUTER`. The DT
theory is precisely "EUF congruence + datatype axioms" — i.e. a specialized congruence child.
So the idiomatic, combinator-reusing fix is:

- Build a congruence child `Dt_congruence` implementing `FABRIC_CONGRUENCE_CHILD` = an inner
  `Euf_adapter` (delegate the whole fabric/merge/checkpoint seam to it) with the DT axioms run at
  `check`/merge time on that same Euf.
- `CombinedDt = Combine (Uflia_router) (Dt_congruence) (Lia_adapter)`.
- `ensure_theory`: for a non-empty datatype registry (Integer/None_seen), install `CombinedDt`
  instead of `TDt`. (`TDt` can stay for the array-free/arith-free case or be retired.)

Why this shape:
- **Reuses Nelson-Oppen wholesale.** The interface-sharing we need is exactly what `Combine`
  already does: the selector result `key(t)` is Int-sorted, DT derives `key(Node l k r) = k`
  (an Int equality) via selector-eval, and the combinator's interface walk shares that Int term
  between the congruence child and LIA — carrying `(> (key t) 0)` and `(> k 0)` into the same
  arithmetic child. DT constructor/selector/tester nodes are ordinary `App`s, which
  `Uflia_router` already routes to the congruence child `A`; Int-sorted equalities route `Both`.
  **The router likely needs no change** (to verify during build).
- **Sound by construction.** No no-op of arith atoms (the charter forbids that — it would
  wrong-SAT the ordering obligations). Every cross-theory equality is a real forwarded literal
  or a genuine `equality_split`, per `Combine`'s soundness backbone.

**Effort/risk:** this is a real multi-hour build, not a one-liner. The bulk is implementing
`FABRIC_CONGRUENCE_CHILD` for `Dt_congruence` (internalize_term, fabric merges, checkpoints,
explain) by delegating to the inner Euf and threading DT saturation through `check_fabric`. Risk
concentrated in: (a) DT axiom merges must flow through the fabric merge log so LIA sees them; (b)
DT conflict explanations must remain valid combined-currency premises; (c) not regressing the
DT-only / UFDT paths (keep them green — they can route through `CombinedDt` with an inert LIA, or
keep `TDt` for the arith-free case). check-frozen: `Combine`/`Theory` .mli are frozen — the plan
adds a new child module + a new `ensure_theory` arm, touching neither frozen signature.

**Rejected alternatives:** (i) no-op the foreign atom (poison47-style) — charter-forbidden,
wrong-SAT. (ii) DT owns its own LIA + hand-rolled NO — duplicates the combinator, higher
soundness surface. (iii) fold DT into `Euf_adapter` — pollutes the shared EUF adapter used by
every logic.

**De-risking finding (the substrate already exists).** `euf_adapter.mli:69-84` shows the Euf
engine already carries exactly what a DT client needs on the SHARED Euf: a multi-consumer
merge-notification log (`add_merge_consumer`/`drain_merges`) and a per-class tag slot
(`set_class_tag`/`class_tag`), and the doc explicitly states these are for "the combinator's
LIA-notify path AND A DATATYPES CLIENT each drain via their own cursor." So `Dt_congruence` need
NOT own a second Euf: it delegates the whole `FABRIC_CONGRUENCE_CHILD`/`THEORY` seam to an inner
`Euf_adapter` and runs the DT axiom fixpoint as a client of that same Euf — reacting to merges via
its own cursor, reading classes via the query API, tagging constructor classes via `class_tag`.
The open implementation question (being resolved in the build) is the injection channel for DT's
DERIVED equalities (selector-eval `key(Node l k r)=k`, injectivity field eqs): they must reach
LIA, so they are emitted either as `check`-time propagations/lemmas (SAT re-asserts to both
children — the classic Path-1) or via `assert_fabric_eq` (hub edge → Stage-2 `notify_eq` to LIA).
Path-1 is the lower-risk default (fabric can be off); that is the intended mechanism.

## Tests (to build, shape-independent)

- REDs: the 3 repro files (dt_plus_int_order → unsat; dt_only, int_only stay unsat).
- Mixed-SAT discrimination: a DT+arith query that IS sat, returns sat + a valid model
  (constructor tree for the datatype term + an Int witness satisfying the order atoms).
- Gates: make test, check-frozen 14/14, dt/array/lia suites, five-logic byte-id where
  untouched, UFDT quantified spot (no regression).
