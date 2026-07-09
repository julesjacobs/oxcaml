# HOF kit — reusable higher-order-spec recipe (WP-0)

The full recipe is `docs/plans/2026-07-08-vox-stdlib-hof-recipe.md`. This is the
one-screen index for WP-1..WP-4.

**What it is.** A copy-in snippet + recipe (NOT a shared module), because the
reusable part is ~6 trivial lines whose cross-unit import is a trap (the
`IntRel`-`abbrev` finding, notes/vrel.md), while the bulk — the per-container
relational lifts — is irreducibly per-container. `Vrel` is the reference for
EXPOSED containers; `Vlist` (this WP) for VIA-ABSTRACTED containers.

**Layers.**
1. *Substrate* — DO NOT declare it. `open Vhof` (the shared leaf
   vox_stdlib/Vhof.{mli,ml}) provides `IntRel`/`IntPred`/`IntRel3` +
   `rHolds`/`pHolds`/`r3Holds` once for everyone; redeclaring collides. Add
   `Vhof` to your manifest deps.
2. *Per-container lifts* over your model inductive: `m_listRel` (map),
   `m_allP`/`m_exP` (filter/for_all/exists), `m_relFold` + `m_sum` (fold_left).

**Laws.** map ⇒ `m_listRel_len` obligation. fold ⇒ `m_relFold_{sum,count}_exact`
as `.mli`-only public theorems (abstract-r + graph-premise; variable-r trigger).
for_all/exists/filter close directly off the exposed lift (no extra law).

**Client rules.** lambdas only in argument position (never in `{...}` — use a
reducible `abbrev` predicate for goals); name every lambda binder (a wildcard
in a reflected relation lambda fails reflection).

**Gates per op.** seal green · smoke green · negative fails closed · per-law
deletion-liveness sweep (removal must break the smoke).

**Boundaries.** (a) exact ELEMENT output does not survive a via face (opaque
model ctors block `m_listRel` reduction) — via containers ship map RELATIONAL +
length and fold RELATIONAL + exact sum/count (over abstract accessors); exact
element output stays a Vrel/exposed-container capability. (b) an op returning
another module's type pulls it into your VoxSig transitively (find_opt →
Voption); keep option/result→list conversions in the higher module to avoid a
cycle.

**`[@vox.total]`.** spec-position relation/predicate params (their lift is named
in a postcondition) get the total annotation once it lands; applied-under-
contract callbacks stay plain. WP-0 shipped plain-arrow; annotate at rebase.
