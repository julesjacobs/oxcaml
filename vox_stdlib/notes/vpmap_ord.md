# Vpmap_ord — the parameter-style poly-key map (the functor fallback)

The polymorphic-KEY fallback for `Vmap_make.Make(ORD)` when the key is a
GENUINELY ABSTRACT `'a` (no `[@@vox.sort int]`, so no `DecidableEq` at its Lean
sort). Instead of a functor argument, the comparator is threaded as a CALL-SITE
VALUE (WP-2's eq-param route, per `Vpset.mem`): `mem` takes a total key relation
`e` (`[@vox.total]`) and a runtime comparator `cmp` whose `= 0` case tracks `e`.
Keys abstract `'a`; values `int`. Repr: prepend assoc list (Vpset mechanism for
a map). Verified green: `.mli` + `.ml` seal + `clients/smoke_vpmap_ord.ml`
(int keys, comparator threaded inline). Depends on `Vhof` (eqHolds).

Ops shipped: `empty` (unspecced) / `singleton` / `add` (prepend) / `mem`.
Laws shipped (LIVE): `mp_haskeyr_cons`, `mp_haskeyr_nil` (each proven
load-bearing by deletion).

## The functor-vs-param delta — precisely where the fallback is WEAKER

This is the headline finding the task asked for: the parameter-style poly map
is strictly weaker than the functor, at three named points.

1. **`find` is ABSENT (the DecidableEq wall).** `find` must return a FUNCTIONAL
   VALUE (the binding for the matching key). At an abstract key the MODEL cannot
   CHOOSE that value without `DecidableEq 'a`, which does not exist at the
   element sort (poly study F-X1, the same wall that blocks Vpset's bool
   `mem`-as-run and `remove`-elaboration). `mem` escapes because presence is
   PROPOSITIONAL — a relational OR (`mp_haskeyr`) the threaded `cmp` decides at
   runtime; a functional lookup has no such relational escape. A relational
   find-spec + inductive discharge could express it (heavier; not shipped). A
   client that needs VALUES BY KEY uses the concrete-ordered route
   (`Vmap_make.Make` / `Vpmap`), where the key models at `Int` and lookup is a
   real function. THIS is the crisp reason the functor is the headline.

2. **No ordering is actually used (so no BST efficiency, no ordered fold, no
   canonical form).** An unordered assoc map needs key EQUALITY, not order; the
   comparator's FULL ordered contract (the `<` part) is unused — only its
   `= 0 <-> key-equality` case is needed. The comparator is threaded to match
   the SAME interface a client would hand the functor (reuse), but it degenerates
   to an equality decider here. Ordering is exactly what the functor buys: BST
   `O(log n)` ops + ordered iteration, both forgone by the fallback (linear
   scan, prepend, first-binding-wins).

3. **`empty` is unspecced (F-B2), so emptiness is only reachable through
   `singleton`.** A spec'd nullary via-producer over a PARAMETERIZED model
   leaves the Lean type parameter an unsolved metavariable at the via injection
   (same as Vpset/Vpmap). Consequence in the smoke: `mem` on a BARE `empty ()`
   cannot be proven false (the model does not know `empty () = mp_nil`);
   `mp_haskeyr_nil` is forced instead through `singleton`, whose spec names
   `mp_nil`.

## Ergonomics friction — inline comparator at the call site

- **site:** clients/smoke_vpmap_ord.ml (the `mem (fun a b -> a = b) (fun x y ->
  ...) k m'` calls)
- **milestone/gap:** C1-adjacent (dependent-param argument shape)
- **what I tried:** define the comparator STANDALONE
  (`let icmp : (x:int)->(y:int)->int{ (_=0) = eqHolds (fun a b -> a=b) x y }`).
- **error:** `Syntax error` — the refinement predicate grammar rejects the
  lambda argument `(fun a b -> a = b)` to `eqHolds` (a standalone comparator has
  no bound relation param `e` to name).
- **workaround used:** pass BOTH the relation and the comparator INLINE to `mem`
  so the comparator's contract `(_ = 0) = eqHolds e x y` is checked against
  mem's expected type with `e` instantiated to the passed relation. Works
  (inline pure lambdas to a dependent param are fine — unlike a named call
  result, C1).
- **removed by:** a way to write a standalone value whose refinement mentions a
  free relation (e.g. a relation-parameterized value type), or allowing the
  lambda in the refinement grammar.
- **severity:** MINOR (the inline form is idiomatic anyway — the client reuses
  the same comparator expression they'd give the functor).
