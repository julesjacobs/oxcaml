# via: adversarial sweep + cost curve (2026-07-05)

Adversarial probing of the `via` feature (Lean-backed abstraction
functions) and a verification-cost measurement.  No soundness bug, no
crash, no silent-wrong was found; the two findings below are a confusing
diagnostic on a degenerate surface and a doc-accuracy gap, both
fail-closed.  Regression tests: `mechanics/via_adv_*.ml`.

## Adversarial sweep -- verdicts

| Probe | Theme | Verdict |
|-------|-------|---------|
| 1 | Cyclic via targets | CLEAN.  Self-via and mutually-recursive via are rejected by OCaml's own recursive-abbreviation checker BEFORE sort resolution.  A cycle through a DATATYPE target (which that checker permits) terminates via the `visited` guards in `dsort_of_type`/`register_type_specs` (degrade to the uninterpreted sort). |
| 2 | Cross-unit model collisions | FAIL-CLOSED.  Two units publicly defining `ISet` differently, co-imported by a client whose VC forces both blocks, fail at the Lean olean import (`environment already contains 'ISet' from VoxSig_...`).  See F2. |
| 3 | Rewrap smuggling | FAIL-CLOSED.  The injection VC always demands the invariant; a provably-bad tree is refuted, opaque bad trees (helper-laundered, ref-stashed) are unprovable; a genuinely-good rewrap passes. |
| 4 | Exotic skeletons | COHERENT.  `[@vox.via]` attaches to arrow / mutable-record / GADT / bare-tyvar skeletons as `Trefine(skel,[map],true)` -- never silently dropped; the map is trusted, never applied to a real value.  Nested-alias layering composes maps correctly and denotes at the final image.  See F1. |
| 5 | Equality reflection | SOUND.  vox NEVER reflects polymorphic/structural `(=)` as image equality.  A raw via binder cannot be compared (projection VC); after `refine_` unpack, structural `(=)` on the trees reflects as an UNINTERPRETED boolean, so an image-equality claim cannot discharge -- even with a trivial skeleton invariant.  (`elems` was made many-to-one to expose any image-collapse.) |
| 6 | Trusted-ghost edges | COHERENT.  `lean "Prop"` / `"Type"` / `"False"` are accepted (only `Vox_`/`v_` prefixes are reserved); a spec over a `Prop`-sorted ghost verifies.  Trusted-ghost contract covers asserting at these; nothing explodes. |

### F1 (minor): two `[@vox.via]` attributes on one type -> ill-typed Lean
`type bag = tree{..} [@vox.via (elems:iset)] [@vox.via (toBag:ibag)]`
composes the maps in the WRONG order (`[toBag; elems]`, because
`typetexp.vox_via_attr` `List.find_map`s the first attr and the outer
recursion appends last), so `toBag : ISet->IBag` is applied to a tree ->
ill-typed Lean.  The failure surfaces as a VC whose goal is textually
IDENTICAL to a hypothesis yet "fails" with a Lean "Application type
mismatch" -- very confusing.  The DOCUMENTED layering path (nested type
aliases) is unaffected.  Suggested fix (small, local): reject >1
`[@vox.via]` on one type at `typetexp.ml` with a message pointing at
nested aliases.  Reported, not fixed (compiler-source change; degenerate
undocumented surface).

### F2 (minor, doc gap): VoxSig block name collisions are shape-blind
Two units publicly defining `ISet` fail-closed when co-imported EVEN IF
the definitions are byte-identical (Lean `import` has no content dedup).
This is sound (fail-closed) but the design doc's rule "same solver name,
different shape -> fail" implies same-shape dedups; that holds only for
AUTO-EMITTED datatypes (`check_imported_datatype_clashes` compares
rendered text), NOT for user-written PUBLIC block declarations.  Doc
should state: public model/ghost names must be globally unique across
co-imported units; share a model by importing it, never by redefining.

### Diagnostic note (not a bug)
Fail-closed VCs involving an opaque value where a skeleton sort is
expected (rewrap of an opaque tree, unpacked-tree `(=)`) surface as a
Lean "Application type mismatch" rather than a grind counterexample or
the clean "unpack with refine_" projection error.  Correct outcome
(rejection), degraded message.

## Cost curve

`ocamlc -c` wall time, installed compiler + pinned Lean 4.31.0, warm.

- Via LAYER COUNT (nesting depth) is FREE: 1..6 nested via layers all
  ~0.5s (image-binder makes each layer a cheap def unfold; the top VC is
  one ground membership goal).
- Via MODEL SIZE (number of `@[grind]` defs the solver loads) is where
  via bends the ~1s/module baseline, ~LINEAR at ~15 ms/def:
  0 extra defs 0.46s | 5 -> 0.53s | 20 -> 0.74s | 50 -> 1.19s.
- Anchor: `lib/via_set.ml` (real BST-as-Set, `elems`+`tmem_elems`
  bridge, `add`/`member` proved) ~0.50s -- comfortably under baseline.

Takeaway: the fixed Lean startup (~0.4s) plus a term linear in the
grind-visible model dominates; the abstraction machinery itself (map
layers, image binder, `refine_` links) adds negligible cost.  Rich
models (many grind defs) are the only thing that pushes a via module
past 1s.
