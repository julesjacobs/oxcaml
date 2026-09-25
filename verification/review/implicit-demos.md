# Implicit refinements in cost and structure demos

Remove 2068 source occurrences of `refine_` from the cost and historical
structure developments, their interfaces and clients. The inventory records
before/after counts and SHA256 for each changed source; every after count is zero.
Keep all semantic predicates, ghost scopes, ownership modes and proof calls.
Simplify redundant self-bindings, unit-result aliases and unused proof names.
The `detach` postcondition retains an ordinary local binding because deleting it
prevents verification on the pinned compiler; no obsolete keyword is needed.

Base: `5355e755bcdec9fe57805f521155586a8b2e8348` (PR #207).
Compiler/source evidence is in `implicit-library.md`; the same read-only
`worktrees/time-credits/_install` installation is used here. No full compiler
build or installation was performed.

Both commands passed with that absolute installation prefix:

```
python3 verification/review/check_structures_boundaries.py COMPILER_PREFIX
python3 verification/review/check_cost_boundaries.py COMPILER_PREFIX
```

These cover bytecode/native implementation checks, public-only semantic clients,
concrete ring/online-growth/time-credit execution, 61 expected rejections,
hidden-helper rejection and emitted-operation erasure. Normal ocamltest runs
also pass all seven changed structure/cost rejection fixtures in both backends.
Their expected output was regenerated while preserving every rejection class,
including the existing principal-mode alternatives. Clamp, folding and list-law
expect fixtures now explicitly compile and load their checked public modules;
their normal bytecode/native tests pass in both principal settings.

Public review surfaces remain those in `cost-boundaries.md` and
`structures-boundaries.md`. No specification claim or primitive trust boundary
is weakened. Parser/refinement-language compatibility fixtures and immutable
historical snapshots are outside this migration.
