# WP-6 verification-efforts findings (2026-07-08)

Three consumer mini-projects stressing the polished stdlib as a real user would,
each verified with the real solver (effort_scope / effort_sorted / effort_dedup).
The friction list below is the primary product — it feeds the next campaign round.
Inline `F-*` findings live at the bottom of each effort file.

## What verified (the wins)
- **Scope table (Vmap+Vlist):** shadowing (inner binding wins), pass-through,
  definedness, empty-default, key enumeration — all fell out of the shipped
  add/find laws. The smoothest effort.
- **List aggregation (Vlist+Vint):** sum & count preservation via the exact-output
  fold laws; append/cons length laws; a Vint.iclamp-bounded aggregate.
- **Dedup (Vplist, eq-param):** distinct (subset spec), distinct-count, membership
  + remove on a deduped list, composition — the DecidableEq wall is gone in
  practice.

## Friction list (feeds the next round), by severity

### MAJOR
- **F-B1 — client-side structural recursion over a via-abstracted Vlist is
  blocked.** A hand-rolled `insert`/`sort` via head/tail/is_empty does not verify:
  from `is_empty l` a client cannot derive `length l = 0` (no is_empty↔length
  bridge law), and the cons-of-head-tail reconstruction does not fire inside a
  recursion. The head/tail eliminator surface is insufficient for consumer-side
  recursive proofs. FIX OPTIONS: (a) ship the bridge laws (`ll_isnil l ↔ ll_len l
  = 0`; a firing reconstruction trigger); (b) ship insert/sort as Vlist OPS
  (module-internal, over the repr) — the recommended route for sorted containers.

### MEDIUM
- **F-B2 — `ll_sum` has no cons law.** `ll_sum (cons x l) = x + ll_sum l` is not
  client-provable (opaque `ll_cons`); sum is reachable only via `fold_left`'s
  exact law. Ship `ll_sum_cons` (+ `ll_sum_app`) alongside the length laws.
- **F-B3 — sortedness invariants are not client-expressible** on concretely-built
  lists (opaque cons won't reduce a client `ll_sorted`). Needs module-internal
  invariant support (invariant-liveness pattern).
- **F-C1 — a `[@vox.total]` decider cannot be forwarded** (WP-1/WP-2 finding): a
  client generic over `'a` cannot thread a comparator; instantiate the poly op at
  a concrete element with a call-site lambda. A total-forwarding rule would remove
  this.
- **F-C3 — eq-param `dedup`/`remove` are subset-only**; superset/set-equality
  needs the decider to be an equivalence (negative controls confirm). A client
  supplies an equivalence + its own block law for exact set semantics.

### MINOR
- **F-A1 — Vmap has no `mopt` value-extractor** (`find_or`); a client defines its
  own spec fn over the eliminator. Ship `Vmap.find_or` with a spec.
- **F-C2 — a relational-result op cannot be re-referenced in a downstream
  refinement** (C1): pass the result as an argument to query fns.

## Cross-cutting theme
The via-abstraction that keeps the int-core interfaces clean (opaque model
constructors) is exactly what blocks CLIENT-side structural/recursive reasoning
(F-B1/B2/B3) — consumers must stay on the shipped algebra (laws + HOFs + exact
laws) and cannot hand-roll traversals. The stdlib should therefore ship the
traversal-shaped ops (insert/sort/fold-with-invariant) itself rather than expect
clients to build them. The eq-param and exact-law surfaces, by contrast, ARE
fully consumable.
