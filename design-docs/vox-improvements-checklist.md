# Vox improvements checklist

Stack this work above total-recursion closures (#139), then restack the mutable
unifier (#138). Keep all changes covered by accepted and rejected regressions.

- [x] Audit and fix the negative-datatype totality boundary; use compile-only
  counterexamples and never execute a potentially diverging proof.
- [x] Fix total recursive definitions at the interactive top level and investigate
  recursive result-refinement annotations.
- [x] Adapt function refinements through checked pointwise obligations, including
  equal saved heap observations, without SMT quantifiers.
- [x] Elaborate non-variable dependent arguments while preserving evaluation
  order, single evaluation, ghost erasure and ownership.
- [x] Support direct ghost fields in unboxed records and improve total-function
  versus total-result diagnostics.
- [ ] Import frontend changes into Merlin, run focused and broader checks,
  exercise the new APIs in the unifier, and publish/restack the PRs.

The supported boundaries and elaboration rules are recorded in
[vox-proof-ergonomics.md](vox-proof-ergonomics.md). The totality audit found a
stale warning, not a remaining reproduced loophole. Dependent expression support
covers stable literals and immutable projections; effectful expressions retain
an explicit-let requirement. Function adaptation requires unlabelled parameters.
