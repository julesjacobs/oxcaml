# Vox SAT solver

`Vox_sat.solve fuel n formula` accepts an in-memory CNF. Variables are numbered
`0` through `n - 1`; `Positive v` and `Negative v` are its literals. The empty
formula is satisfiable and an empty clause makes a formula unsatisfiable.

The checked result contract has four outcomes:

- `Ok {answer = Sat assignment; ...}`: `assignment` has exactly `n` Boolean
  values and satisfies `formula`. `solve` also runs the executable `check`
  against the original accepted formula before returning `Sat`.
- `Ok {answer = Unsat; ...}`: `unsat_at n formula report assignment` proves
  that every assignment of length `n` fails to satisfy `formula`. The report
  from `solve` carries the formula-specific `refutes` premise needed to call
  this lemma.
- `Ok {answer = Unknown; ...}`: the search-node budget was exhausted; there is
  no satisfiability claim.
- `Error ...`: the variable count is outside `0..256`, the formula exceeds
  4,096 clauses or 65,536 literal occurrences, or a literal index is invalid.
  These checks happen in that order.

The solver simplifies clauses after assigning the next variable. It stops on
an empty clause or opposing unit clauses, returns a model when no clauses
remain, and follows a unit clause immediately when it concerns the next
variable. Otherwise it explores both values. `reduce_formula_correct` relates
each simplification to the
independent `eval_formula` semantics; `refutes_sound` proves that a refuted
search node excludes every satisfying assignment extending it. Both branches
must be refuted before search returns `Unsat`.

`fuel` counts recursive search calls. It is not a wall-clock or literal-visit
limit. This implementation uses immutable lists and allocates reduced clauses;
it has no watched literals or clause learning. The explicit limits and `Unknown`
result make hard instances well-defined, but the current performance target is
moderate CNFs. DIMACS parsing and any claim about bytes read from a file are
outside this module's theorem.

## Validation

```sh
./dev test vox/sat_solver.ml
make -s vox-library
_install/bin/ocamlopt -extension refinement_types \
  -I _install/lib/ocaml/vox _install/lib/ocaml/vox/vox_borrow.cmxa \
  verification/benchmarks/vox_sat_bench.ml -o _build/vox_sat_bench.exe
_build/vox_sat_bench.exe
```

The test checks bytecode, native, and `-principal` compilation, a formula with
no initial units requiring both UNSAT branches, opposing units on a later
variable, forced literals, budget and
input boundaries, and 729 three-clause formulas against an exhaustive
two-variable oracle; the separate rejection test checks that callers cannot
forge the formula-specific UNSAT premise. The benchmark uses the compiler from `make install`, as
required by the repository guide.

On the 2026-09-24 arm64 development machine, the benchmark measured 0.6 ms
for a 256-variable UNSAT implication chain; generated random 3-CNFs with 20
and 30 variables finished in 1.7 and 3.2 ms. The 50-variable instance returned
`Unsat` after 836,723 search calls in 2.2 CPU seconds. The 75- and 100-variable
instances returned `Unknown` after one million search calls in 4.8 and 7.4
CPU seconds. The fixed random seed and generator are in the benchmark source.
These results show the current scaling limit; they are not
SATLIB or competition results.

The trust boundary includes Vox's compiler, VC generation and ghost erasure,
Z3's reported `unsat` result, and the compiler/runtime execution stack. This
module introduces no SAT-specific external primitive or unchecked axiom.

[`Vox_cdcl`](vox_cdcl.md) uses this module's resolution kernel to prove
learned clauses and derive an empty clause for UNSAT.
