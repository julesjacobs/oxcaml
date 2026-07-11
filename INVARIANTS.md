# Invariants

Numbered and citable. Specs and child reports reference these by number (e.g.
"I3"). Adding or changing one is a deliberate act — note it in the change and,
if load-bearing for a frozen interface, in an ADR.

- **I1 — Well-sorted, hash-consed.** Any `Term.t` in existence is well-sorted
  and hash-consed. Ill-formed terms are unconstructible, not merely detectable.
- **I2 — Single construction path.** Smart constructors are the only way to build
  a term (the node type is `private`); they sort-check, normalize, and hash-cons.
- **I3 — Dependency firewall.** Nothing under `smt/` depends on anything above
  the OCaml stdlib (no opam packages, no `compiler-libs`, no `Unix`). Test code
  under `tests/` is exempt.
- **I4 — Justified inferences.** Every derived fact — theory propagation, theory
  conflict, learned clause — carries a premise set plus a rule tag, so it is
  explainable on demand (DESIGN.md §7).
- **I5 — No wall-clock in goldens.** Committed golden files never contain
  wall-clock time or other nondeterministic values; exact timings go to an
  uncommitted stats sidecar.
- **I6 — Determinism.** Search uses fixed seeds and no wall-clock-dependent
  heuristics; a given input yields the same decisions, verdict, and counters
  every run.
