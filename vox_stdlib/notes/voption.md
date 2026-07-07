# Voption — language/compiler needs (build log)

Wave-1 exposed-ADT leaf module. Shipped in obligation form (blueprint
default), zero trust. Sealed green with the real solver; smoke client
verifies against `voption.cmi` + `VoxSig_Voption.olean` only (sources
deleted). No `#31`/`#32` sites: Voption has no recursive via-returning op and
no bind-then-branch on a spec'd bool, so neither gap can bite here (a note for
either would be miscalibrated).

### Voption · model theory authored in both .mli and .ml
- **site:** vox_stdlib/voption.mli:14,17,20  and  vox_stdlib/voption.ml:3,6,9
- **milestone/gap:** model-dup
- **what I tried:** author `vo_is_some` / `vo_get_or` / `vo_get` once. The
  `.mli` needs them (`@[grind] public def`) as the client vocabulary; the
  `.ml` needs them again (`@[grind] def`, no `public`) so the op-contract VCs
  can unfold them.
- **error:** none — but 3 defs are typed verbatim twice (6 authored defs for 3
  concepts). Drift between the two copies would be silently accepted for any
  def not exercised by a discharged obligation.
- **workaround used:** restate the three defs in the `.ml` block, dropping
  `public`/`expose` (the documented model-duplication tax).
- **removed by:** a `.ml`-side "import the `.mli` model block" form, or having
  the seal re-use the interface's defs instead of demanding local copies.
- **severity:** MAJOR-ERGONOMIC

### Voption · client law statement typed twice (obligation form)
- **site:** vox_stdlib/voption.mli:23,25,26,28  and  vox_stdlib/voption.ml:12,14,15,17
- **milestone/gap:** M1
- **what I tried:** state each of the 4 definitional laws once. Obligation form
  requires the `.mli` `public axiom <name> : <stmt>` and the `.ml`
  `theorem <name> : <stmt> := by grind` to be verbatim-identical in name, type,
  and attribution (`@[grind]` on `vo_not_some_none` in both).
- **error:** none — 4 laws × (axiom + theorem + matching grind_pattern) all
  hand-kept in sync.
- **workaround used:** duplicate each statement; keep the `@[grind]`/
  `grind_pattern` attribution identical on both sides.
- **removed by:** a prove-only obligation form (`.mli` names the law, `.ml`
  supplies only the proof term / tactic, no restated statement) — the M1 ask.
- **severity:** MAJOR-ERGONOMIC

### Voption · constructor application can't be passed to a dependent parameter
- **site:** vox_stdlib/clients/smoke_voption.ml:15,17,19,21
- **milestone/gap:** C1
- **what I tried:** `is_some (Vsome x)` / `get_or d (Vsome x)` / `get (Vsome x)`
  — pass a constructor application directly as the argument of a dependent
  parameter (`o` appears in the result refinement / precondition).
- **error:** `vox: the argument for a dependent parameter must be a variable or
  a pure expression the logic can name (let-bind it first)`
- **workaround used:** `let o = Vsome x in is_some o` — bind the constructor
  value to a variable first, then pass the variable. Applies at every call
  that feeds a constructor value to a dependent parameter (all 4 smoke goals).
- **removed by:** letting the logic name pure constructor applications inline
  (auto-let-bind a syntactically-pure argument), so clients need not hoist
  every `Ctor arg` to a `let`.
- **severity:** MAJOR-ERGONOMIC

### Voption · no higher-order ops (map / bind / iter)
- **site:** vox_stdlib/voption.mli (absent by design — the natural `map`/`bind`)
- **milestone/gap:** new
- **what I tried:** the obvious option API wants `map : (int -> int) -> t -> t`
  and `bind : t -> (int -> t) -> t`. vox does not model function-typed
  arguments in the refinement logic, so a spec like `{ _ = vo_map f o }` has no
  way to talk about `f`'s action.
- **error:** n/a (not attempted — would need a function-in-the-logic model).
- **workaround used:** ship only first-order ops (`is_some`/`is_none`/
  `get_or`/`get`); `map`/`bind` deferred, per blueprint §3.
- **removed by:** a story for reasoning about function-typed op arguments
  (e.g. reflected/uninterpreted function symbols with an application axiom, or
  spec-carrying arrows) — a genuine capability gap, not just ergonomics.
- **severity:** MAJOR-ERGONOMIC

### Voption · `@[grind, expose]` on model defs makes definitional laws DEAD (amendment A — third sighting; RESOLVED in-module)
- **site:** vox_stdlib/voption.mli:14,17,20 (the def attributes)
- **milestone/gap:** M3
- **what I tried:** ship the model defs as `@[grind, expose] public def` (the
  probe/vopt_b default) alongside `vo_is_some_some` / `vo_not_some_none` /
  `vo_get_or_some` as distinct client-facing laws. Phase-C review then
  probe-verified that all three laws were DEAD: with the defs exposed, a
  client's `grind` unfolds the def and discharges every goal *without matching
  the law's grind_pattern* — removing `vo_is_some_some` left the smoke client
  green. This is the same trap Vint/Vmap hit (amendment A), so Voption is a
  third sighting: it is not module-specific, it is inherent to shipping a
  definitional law over an `expose`d def.
- **error:** none at compile — the danger is silent: a dead law verifies, and
  the §6.7 dead-law check passes vacuously because `expose` gives grind a
  second route the law never needs.
- **workaround used:** de-exposed the three model defs (dropped `expose`, kept
  `public`) — clients still name them in specs but grind can no longer
  unfold-past, so the reduction facts must ride as the named laws. This forced adding
  `vo_get_some : vo_get (.Vsome x) = x` — without `expose`, `get`'s result
  refinement `{ _ = vo_get o }` is otherwise irreducible for a client, i.e. a
  partial accessor's reduction rule must be shipped as an explicit law once its
  def is opaque. Liveness re-verified: dropping ANY of the four laws now breaks
  smoke_voption.ml (exit 2), each individually confirmed.
- **removed by:** (a) a house default of `public` (not `expose`) for model
  defs that carry definitional laws, or (b) an M3 lint reporting which named
  block lemma actually fired in a VC, so a dead law is flagged rather than
  silently passing. The `expose`-makes-laws-dead interaction should be a
  documented conventions rule, not a per-module rediscovery.
- **severity:** MAJOR (soundness-of-evidence: silently ships unexercised laws
  and defeats the dead-law check; downgraded to MINOR only once a lint exists)
