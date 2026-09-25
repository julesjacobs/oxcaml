# Rule-relative equality saturation

`Vox_egraph_rule_handle` is the checked public interface. It accepts a finite
list of typed first-order rules over integers, booleans, addition, integer
equality and conditionals. Rules need not preserve the built-in interpretation:
equality is relative to exactly the supplied rules. The separate
`Vox_egraph_interpret_wrapping.sound` theorem requires an interpretation premise
for every supplied rule instance before deriving equality of evaluations.

## Semantic review surface

Read these files in `verification/library`, in order:

1. `vox_egraph_language_spec.ml`: expressions, sorts and evaluation.
2. `vox_egraph_rule_spec.ml`: patterns, substitutions, rule validity and lookup.
3. `vox_egraph_derivation_spec.ml`: finite derivations, exact endpoints and validity.
4. `vox_egraph_match_spec.ml`: graph observation and class-based pattern matching.
5. `vox_egraph_snapshot_spec.ml`: input-origin reconstruction.
6. `vox_egraph_preservation_spec.ml`: preservation of previously admitted origins.
7. `vox_egraph_saturation_spec.ml`: finite typed instances and rule closure.
8. `vox_egraph_congruence_spec.ml`: independent enode congruence closure.
9. `vox_egraph_fixedpoint_spec.ml`: conjunction of rule and congruence closure.
10. `vox_egraph_interpret_wrapping.mli`: interpretation consequence and premise.
11. `vox_egraph_rule_handle.mli`: actual executable entrypoints and guarantees.

The accompanying declaration inventory selects the exact public declarations
and every definition needed to interpret them, excluding auxiliary proof bodies.
The immutable-array length/get contracts in `stdlib/iarray.mli`, machine-integer
operations in `stdlib/stdlib.mli`, and Vox's refinement/ghost semantics are trusted.
The checker, SMT encoding, compiler and runtime remain trusted. No new axiom or
trusted e-graph primitive is introduced.

The abstract model is connected to execution by empty creation, exact admitted
origins, preservation through every public operation, and total `same_class`.
The latter returns true exactly for equal model class labels and supplies an
erased valid derivation between their reconstructed origins. A successful
`query` supplies a derivation between the exact two caller expressions.

`Fixed_point` means every typed assignment of allocated IDs (with -1 for unused
variables) and every allocated root satisfies each supplied rule, and all
congruent enodes have equal classes. Repeated variables match by class equality.
No least-congruence, minimum-cost extraction or completeness of `Not_proved`
is claimed. Limit results do not establish closure. Admission and queries may
extend the graph, so a fixed-point result describes that returned state.

## Resources and termination

This implementation supports 64-bit OCaml integers and at most 512 nodes.
Saturation accepts nonnegative search fuel up to 4611686018427387903.
Fuel charges visits to the assignment generator and its append/prepend/choices
helpers, nonempty rule and assignment visits, and candidate-root attempts.
Matching and RHS admission within one attempt depend on pattern size and are
not separately charged. Fuel is not an instruction, time, byte or allocation
limit. The separate bounded-matcher diagnostic has its own exact cost model;
it is not the saturation fuel model.

One round performs rebuilding followed by a frozen exhaustive rule scan.
The scan restarts after its first graph change. `rounds <= 0` returns
`Round_limit`; nonpositive rebuild passes return `Rebuild_limit` when rebuilding
is reached. Rebuild passes are allowed separately in each round. Search fuel
is carried between rounds. `Search_limit` returns zero remaining fuel;
`Saturation_node_limit` establishes that the node count is 512. Exhaustion can
occur before all rules or assignments are inspected.

The public contracts for create/admit/query/saturate are normal-return claims.
Their hash-table interface dependency does not export totality. They do not
promise recovery from allocation failure, stack exhaustion or runtime exceptions.
`same_class` and the pure semantic/proof functions explicitly marked total have
checked totality. Runtime allocation includes enodes, arrays, rule syntax,
assignment lists and ordinary control data. Derivations, origin expressions,
proof substitutions and model snapshots erase; they are not accumulated as
runtime certificates or checked by a final correctness traversal.

## Reproducing the evidence

From a configured checkout with `./dev init` completed, run sequentially:

```
./dev test vox/egraph_rule_public.ml
./dev test vox/egraph_rule_saturate.ml
./dev test vox/egraph_rule_rejected.ml
./dev test vox/egraph_match_observation.ml
./dev test vox/egraph_ghost_arrays_erasure.ml
```

The public client imports only public semantic/interpretation modules and the
sealed handle. It derives exact equality, interpretation and input preservation,
checks saturation capacity, and checks native handle size after ghost erasure.
The saturation regression includes productive cyclic classes and every limit
status. Semantic rejection tests cover foreign-rule provenance and incompatible
transitivity endpoints. The matcher regression includes exact cost/exhaustion.

For a fresh dependency compile, a public-only CMI client, three additional
interface rejection fixtures, and emitted-Lambda inspection, run:

```
python3 verification/tests/check_egraph_boundary.py \
  --compiler _build/main/main.bc --stdlib _runtest/stdlib
```

The script writes its artifacts to a temporary directory and prints its path.
The emitted-code check excludes calls to proof modules; the handle retains a
static export of the ghost `preserved_origin` function, while `same_class`
executes the two union-root lookups in `model_evidence.query`.
