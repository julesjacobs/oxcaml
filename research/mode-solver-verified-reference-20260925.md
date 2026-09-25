# Verified finite regionality solver

## Final status (2026-09-25)

The identified solver/specification defects and installed compiler regressions
are repaired. Final formatting, bootstrap, and optimized installation pass.
With `OCAML_EXPERIMENTAL_SYMBOLIC_MODES=1`, the refreshed kind-bounds directory
passes 76/76 tests, and the complete installed suite passes **2567 tests, with
261 skips, zero failures, and zero unexpected errors** (2828 considered,
909.5 seconds). The source manifest was unchanged throughout the final suite.
Logs: `/tmp/mode-final3-{fmt,boot,install,kind-refresh,full}.log`.
The first `kind` run used an old `_runtest` snapshot via `test-one-no-rebuild`;
`test-one` refreshed it before the passing directory and full-suite gates.

The finite executable reference has 36 checked dependency files and 24 runtime
fixtures, verified in bytecode and native modes. Its public-only client checks
without access to proof interfaces. This proves the specified three-element
reference semantics; it does not prove the mutable production adapter, BDD
implementation, caches, level changes, copying, or rollback. Those are covered
by the compiler and differential regression gates. Symbolic fallback remains
opt-in, and resource exhaustion remains a separate operational outcome.

The coordinating task supplied the recorded standing authorization for
staging, committing, pushing, and opening PRs without merging. Production
changes are committed, and the required Merlin import script completed without
conflicts. Merlin tests and PR publication are in progress.

The sections below preserve the investigation history; earlier failing-gate
statements describe intermediate revisions.

The executable reference uses the chain `Global < Regional < Local`. Terms
contain constants, variables, joins, meets, the regionality morphism, and
arbitrary unary tables. A graph is an arbitrary list of inequalities between
terms. Formulas additionally contain Boolean connectives and nested `exists`
and `forall` quantifiers. Variables use de Bruijn indices.

`Mode_solver_public.eliminate` eliminates quantifiers by substituting all three
carrier values. Vox proves that elimination preserves truth under every outer
assignment and removes the bound variable from the result. Thus the residual
is the strongest exact consequence over the outer variables: it has precisely
the outer assignments for which the original formula holds.

`Mode_solver_public.project_graph` turns a graph into a formula and eliminates
any finite list of graph variables. `Mode_solver_public.decide_graph_checked` rejects graphs
with out-of-scope variable indices before calling the decision procedure. Vox
proves the checked result is exact. For well-scoped graphs, Vox also proves

```text
eval_qf outer (Mode_solver_public.project_graph hidden graph)
  = exists hidden assignment. models (hidden assignment, outer) graph
```

and proves that a well-scoped projection mentions only outer variables.
`assert_graph_subsumption` computes and conjoins the exact guarded
`forall`–`exists` residual with an outer environment. The assertion theorem
states its meaning for every outer assignment, including the quantifier
alternation. The regression demonstrates an assertion that propagates an upper
bound to an outer variable.

`mode_solver_conditional_cut.ml` proves the exact conditional consequence

```text
forall u. (x <= u -> v <= u)    iff    v <= x
```

on regionality. The bound on `v` depends on the outer value of `x`. A numeric
floor computed from the current lower bound of `x` cannot represent this
consequence in general. Both displayed equivalences follow from order and
join laws in any lattice; Vox checks their executable regionality instances.

The same file proves a two-bound version:

```text
forall u. (x <= u and y <= u -> v <= u)
  iff v <= join x y
```

The join appears on the right of the residual inequality. This is outside the
upstream solver's right-hand mode grammar, which permits a variable, constant,
or meet there. On the three-element chain it is equivalent to
`v <= x or v <= y`, making the required disjunction explicit.

`mode_solver_residual_gap.ml` proves a representation gap for the simpler
conjunctive edge language with variables and constants. Every such graph has a
solution set closed under pointwise meet. The two-bound residual accepts
`(x,y,v) = (Regional,Global,Regional)` and
`(Global,Regional,Regional)`, but rejects their pointwise meet
`(Global,Global,Regional)`. No graph in that edge language expresses the
residual exactly. This proof is scoped to that edge language; it does not
purport to classify every upstream morphism and mode form.

`mode_solver_symbolic_projection.ml` gives a symbolic existential step for
arbitrary outer terms on a finite chain. For lower bounds `lower_i <= z` and
upper bounds `z <= upper_j`, it constructs the formula
`join_i lower_i <= upper_j` for every `j`. Vox proves that the constructed
formula is equivalent to the existence of a value for `z` under every outer
assignment. The proof covers empty lower and upper lists. This is an exact
single-variable step for that normalized fragment; it does not yet cover
self-cycles, general morphism edges, or guarded universal blocks.

`mode_solver_level_cut.ml` applies that projector to one inner regionality
variable and one outer variable, with independent lower and upper bounds and
optional edges in both directions. Vox proves that the residual holds exactly
when the outer assignment extends to a model. It also proves that, whenever
an extension exists, taking the meet of the inner upper bound and its optional
outer cap gives a greatest inner extension. This is a single-variable level
cut theorem. It does not establish the proposed level-cut theorem for an
arbitrary upstream graph or validate `update_level`.

This is an executable, verified solver for this finite regionality language.
Its quantifier elimination enumerates assignments and may grow exponentially.
The upstream solver is a heterogeneous, mutable graph representation with
levels, adjoints, and backtracking. The reference theorem does not establish
that the upstream graph operations implement the same relation. A refinement
proof for those operations, especially level updates and rigid checks, remains
the step needed to verify the production solver.

For production, the next representation step is an outer residual formula at
each rigid boundary. A graph edge can be retained when elimination produces
one; otherwise the Boolean residual must remain as solver state. A rigid check
must preserve, for every admissible outer assignment, whether the higher
variables have an extension. Computing one numeric extremum of a rigid
variable is exact only when that extremum is independent of the outer
assignment. The reference solver supplies a finite oracle for testing this
condition and for validating an optimized residual implementation.

The compiled upstream reproducer
`typing-modes/rigid_right_conditional_regression.ml` confirms the first
conditional gap on current `origin/main` plus the rigid-both repair. It creates
older outer variables `x` and `v`, records `x <= u`, makes `u` rigid, and
asserts `v <= u`. The solver accepts the assertion but subsequently rejects
`Regional <= x` together with `Regional <= v`; its output is `lost` against
the expected `preserved`. The Vox conditional projection theorem proves this
outer assignment is valid. The reproducer is deliberately a failing regression
until the upstream representation preserves the conditional residual.

The symmetric compiled reproducer
`typing-modes/rigid_left_conditional_regression.ml` also fails. From
`u <= x`, asserting `u <= v` for every rigid `u` has exact residual `x <= v`.
The solver's global ceiling loses the valid outer assignment
`x = Regional, v = Regional`. Both regressions expect `preserved` and print
`lost` with the current production algorithm.

The production wrappers now roll back logged solver changes if an operation
raises, and the bound-update hints are computed from the old bound before the
new bound is stored. The exception rollback regression and the 324-case
regionality oracle pass. These repairs are independent of the conditional
projection defect. The `typing-modes` run before adding the constant tests
had 48 passes and two failures, both the mixed conditional regressions above.

Direct `S` tests now confirm the analogous constant cases. Given outer `x`
and rigid `u`, asserting `Regional <= u` under `x <= u` should propagate
`Regional <= x`; the compiled test prints `right lost`. Dually, asserting
`u <= Regional` under `u <= x` should propagate `x <= Regional`; the compiled
test prints `left lost`. Both outcomes follow by choosing `u = x` in the
universal condition. These paths currently reject a valid outer residual.

## Production integration contract

The next production state must retain a quantifier-free Boolean residual over
outer variable handles. A rigid assertion conjoins its exact projected
condition to that state. Its `Ok` result means that this state has been
updated, while `Error` means that the assertion has no admissible outer
assignment. A pure subsumption query instead checks whether the current state
implies the residual and leaves the state unchanged. Resource limits need a
separate outcome from either semantic result.

Every later observation must account for the residual: in particular,
`submode`, `get_floor`, `get_ceil`, zapping, and consistency checks. A copy must
rename the handles in the residual while preserving shared outer handles.
Generalization must project handles that leave scope. Level changes must
preserve the state relation. Undo logs must restore residual additions along
with graph mutations. Keeping a residual only at assertion time would not
meet this contract; later graph operations could forget or contradict it.

The one-variable level-cut proof supplies the first refinement case. The next
proof step is an arbitrary graph cut with boundary-fill invariants, followed
by a representation relation between the pure residual state and the mutable
production state. No such representation relation is currently proved.

`mode_solver_residual_state.ml` implements an immutable Boolean residual
state. Vox proves that assertion conjoins the clause, one-variable
generalization existentially projects the first handle, a closed query checks
every assignment, and restoration recovers the prior relation. Copying adds
a fresh equal handle and shifts the old references; Vox proves that projecting
the copy recovers the original relation. Vox also proves that adding an
unconstrained fresh handle leaves the old relation unchanged and that
projecting the second handle of a two-handle state has existential semantics.
The checked copy reports unsupported
indices separately. A further theorem shows that a stored disjunction of
edges has exactly the meaning `v <= join(x,y)` on regionality, the residual
that the production graph cannot currently store. Quantified subsumption
assertions and queries are defined on this state and have Vox exactness proofs.
This is an executable reference state, not the upstream mutable solver's state
representation.

The production worktree now also contains a standalone `Solver_exact` functor
for one finite lattice. It stores an immutable Boolean residual, eliminates
nested finite quantifiers, supports copy and projection of an arbitrary
indexed handle, computes exact per-handle envelopes, and separates
`Invalid_scope` from `Limit`. Its compiled test passes the four conditional
examples, quantifier-order examples, a regionality morphism example, and an
exhaustive two-variable check of copy and projection for simple inequalities.
It can add a fresh handle and merge independent states; the compiled test
checks both operations.
This standalone functor is not wired into `Solver_mono`. Before the separate
production handoff described below, the compiler mode paths still produced
the three failing regression files. The Vox results above are proofs about
the reference implementation, not a refinement proof for this OCaml functor.

The next compiler adapter needs a typed inventory of the variables in one
connected component, a translation of both stored edge orientations and
bounds into `Solver_exact` terms, and an ownership switch so every later
constraint and bound observation uses that exact state. The adapter must
receive an explicit rigid quantifier scope and distinguish its guard from the
outer environment; a variable's numeric level alone does not state the full
quantifier prefix. It must also rename exact-state handles during mode copying
and project them during generalization. Until that adapter exists, the new
functor is a candidate backend and cannot change the compiled mode solver's
answers.

## Production handoff status, 2026-09-25

The separate production worktree now has an experimental finite-relation
handoff in `Solver_mono`. It retains guard assignments and existential witness
assignments separately, evaluates both graph edge orientations, and uses the
resulting relation for `submode` and bound observations. The compiled
conditional regressions, sequential rigid/flexible assertion regression,
copied rigid/flexible assertion regression, regionality oracle, and the full
`typing-modes` directory pass (60 tests). Copying now gives a copied variable
a renamed relation while shared variables retain both original and copied
relations. This is test evidence, not a proof of refinement.

Exact-owned zapping now selects a feasible constant and narrows the relation;
the compiled conditional outer-variable zap regression passes. The
no-constant case proved below now has a separate failure outcome. The handoff
still has unhandled semantics: `update_level` now reclassifies a witness
entering the rigid guard,
but generalization and other level changes do not yet have full scope
semantics. Fresh variables created above or below an exact-owned mode now
inherit exact ownership, and the compiled `newvar_above` regression passes.
The graph-equal copy created by `generalize_structure` also inherits exact
ownership; its compiled regression passes. Other constructors and internal
graph operations require further validation. The current 10,000-assignment
limit raises `Exact_resource_limit`, separate from a semantic inequality
failure. A quantified mode with no valid constant zap raises
`Exact_zap_impossible`. Both exceptions have registered compiler diagnostics,
and compiled tests exercise the distinction. The public detailed solver APIs
return `Inequality`, `Inconsistent_relation`, or `Resource_limit` for
subsumption and `No_constant` or `Zap_resource_limit` for zapping. The old
interfaces remain for compiler callers and convert the two exact failures to
registered diagnostics. Full generalization semantics remain necessary.

The production solver should not be presented as complete or verified while
these paths remain open.

## Graph projector follow-up

The production worktree now has a compact graph projector for finite lattice
constraints whose binary slices are principal upsets induced by
join-preserving morphisms. It composes those morphisms while eliminating
existential variables. A separate bounded binary-matrix projector handles
cases outside that fragment. A small exhaustive oracle checks projection and
implication against enumeration. These are executable checks, not Vox
refinement proofs for the mutable solver.

The projector can convert a composed self-edge into a unary restriction; a
cycle case was added to the graph oracle. In combination with a direct
implication shortcut, the focused `typing-mode-polymorphism/alloc_mode.ml` test
passed, including projections of 102-, 47-, and 95-variable components. The
shortcut was then removed: a full `typing-modes` run found seven semantic
regressions because skipping exact ownership loses information required by
later zapping, copying, and rigid assertions. Enabling self-edge projection in
the production shortcut also exposed this ownership gap, so the compiler uses
the previous conservative behavior while the oracle exercises the exact
self-edge transformation explicitly. A subsequent ownership regression showed
that the graph-tautology handoff itself is unsound even without the self-edge
rule: the two assertions `r <= a` and `a <= r`, with universal `r` and
existential `a`, are separately tautological, but together require `a = r` and
have no constant zap. The handoff is now off by default and available only
under `OCAML_EXPERIMENTAL_GRAPH_MODES=1`. Capacity improvement awaits a symbolic
component that remains owned after each assertion.
With the handoff disabled by default, all 63 `typing-modes` tests pass,
including the ownership, conditional, copying, and graph-oracle regressions.

The full mode-polymorphism gate still fails. `basic.ml` passes, but
`curry_mode_subsumption.ml` reaches a 42-variable component for which the
projected assertion is not tautological. The finite-relation handoff then
exceeds its search limit. This is the nontrivial outer-residual case described
above: proving tautologies with a graph projector does not retain conditional
consequences over outer variables. The gate must remain open until a symbolic
outer residual is integrated into every relevant solver operation and verified
against the finite oracle.

## Production capacity finding

The full `typing-mode-polymorphism` suite is a required production gate. With
the finite-relation handoff enabled, 17 of its 19 tests fail, usually at the
first toplevel phrase with `Exact_resource_limit`. Even `let id x = x;;` fails
in the interactive toplevel under `-extension mode_polymorphism_alpha`, while
the same definition compiles under `ocamlc -i`.
The failure also occurs before any explicit ordered-scope wiring is added to
`Ctype.moregeneral`, so that wiring was rolled back. Filtering each variable's
enumerated values by its stored bounds does not fix the toplevel failure.
Graph-valid-row backtracking with partial edge pruning also fails the focused
subsumption test at its first toplevel phrase. Both unsuccessful enumeration
changes were rolled back. A capacity repair must pass the mode-polymorphism suite
without reverting to the unsound legacy handling of conditional assertions.
Temporary tracing of `let id x = x;;` showed the first exact subsumption has
two seed modes, but its graph-connected component already has 85 variables:
39 outer, 2 universal, and 44 existential. It has no stored exact component.
This is a graph projection problem at the initial handoff, not accumulation
of stored exact rows. The tracing code was removed after measurement.

The exact adapter also used the solver's persistent ID to find a variable in
rows and connected components. Persistent copies can reuse a negative ID after
`reset_persistent_id`, so the adapter now compares variable identity there.
A compiled regression with two copies bearing the same negative ID passes.
This correction does not resolve the toplevel search-limit failure.

`mode_solver_zap_obstruction.ml` proves a concrete API obstruction on the
three-element regionality chain. For every rigid value `r`, the existential
witness `a = r` satisfies `r <= a` and `a <= r`. No single constant `a` can
satisfy those inequalities for both `r = Global` and `r = Local`. Thus a
quantified satisfiable exact component may have no constant zap. A total zap
API returning only a lattice value cannot always preserve its relation; it
needs an explicit failure outcome or a scoped projection operation with a
different contract. The proof does not by itself establish when production
calls zapping on such a component.

The same obstruction distinguishes nested scopes. `forall r. forall s.
exists a. a = s` holds, while `forall r. exists a. forall s. a = s` fails.
The first formula lets `a` depend on `s`; the second requires a single `a`
before `s` is chosen. A state that records only which variables are rigid and
which are flexible identifies both prefixes. The production adapter therefore
needs explicit scope ordering before it can claim nested-quantifier
correctness. The Vox no-constant theorem proves the critical impossibility
for the second prefix.

The experimental production adapter now accepts explicit `Outer`,
`Universal order`, and `Existential order` tags. Its finite-relation check
groups variables by ordered quantifier blocks; a compiled regression
distinguishes `forall s. exists a. a = s` from `exists a. forall s. a = s`.
An additional compiled oracle compares two successive regionality assertions
against direct quantified evaluation for three quantifier orders and 300 pairs
of inequalities; it reports zero mismatches. The oracle tests explicit tags,
not the compiler's subsumption path.
The default constructor still derives one universal and one existential block
from the numeric level. The compiler's subsumption path uses
`subject_level = generic_level - 1`
in `Ctype.moregeneral`; `Ctype.update_level` passes type levels through to
mode variables. Nested subsumption can reuse this numeric subject level.
The subsumption path must supply ordered scope tags to obtain the nested
behavior automatically.

The old production error type, `error_raw`, contains one pair of lattice
values witnessing a failed inequality. Intersecting previously stored exact
components can instead remove every witness while the newly asserted
inequality holds on every guard row. The exact handoff now distinguishes this
as `Inconsistent_relation` in `submode_detailed`, separate from a local failed
inequality and resource exhaustion. The compatibility `submode` raises a
registered diagnostic for that case. A compiled regression checks the
resource and inequality result constructors; no compiled reproducer yet
reaches the inconsistent-relation constructor.

## Symbolic fallback experiment

An experimental multi-valued decision diagram backend is present behind
`OCAML_EXPERIMENTAL_SYMBOLIC_MODES=1`. It is not a production capacity repair.
The direct ordered-scope oracle still passes, but `let id x = x;;` under
`-extension mode_polymorphism_alpha` does not finish within 30 seconds with
the fallback enabled. Temporary tracing showed an eight-variable snapshot
with 9,075 MDD nodes, followed by a slow conjunction with the next inequality.
Separating a left join/right meet comparison into pairwise atoms and fixing
the MDD Boolean identities did not make that repro complete. The default
path remains the bounded row solver. All 62 `typing-modes` tests pass there;
17 of 19 `typing-mode-polymorphism` tests fail with the existing exact search
limit. The MDD experiment does not justify wiring ordered scopes into
`Ctype.moregeneral` or claiming production support for nested binders.

## Retained binary symbolic state

The symbolic backend now represents each finite lattice variable by binary
index bits. Extra encodings repeat the last slice of the odd factor of the
carrier cardinality, preserving the remaining product coordinates. Every bit
assignment denotes a carrier value, so complement and quantification need no
additional validity constraint. Mode axes provide ordering hints that align
corresponding coordinates across scalar and product objects. The ordering is
semantically irrelevant; an exhaustive heterogeneous-domain oracle checks
Boolean operations, quantification, fused guarded quantification, and import
under different bit orders.

The compiler retains both guard and witness diagrams. Each assertion conjoins
its atom with the existing witness before computing the guarded residual, then
restricts both stored relations by that residual. Copies rename the same
relation through member handles; subsequent constraints and zapping observe
that owned relation. Snapshot reuse requires the same member identities and
unchanged immutable bounds and edge maps. The component already entails that
stamped graph, so reconstruction would conjoin redundant graph constraints.

Forced symbolic testing exposed an error in the prior universal/existential
fold: after a universal step, an existential step could select a value outside
the guard through vacuous truth. Existential projection now computes
`exists vars. domain && winning`. Universal projection computes
`forall vars. domain => winning`. Both operations combine Boolean application
with abstraction to avoid materializing the full intermediate formula.

`mode_solver_retained_symbolic.ml` proves the corresponding guarded projection
recursion for arbitrary nested prefixes on regionality. Its specification is
a game in which existential choices must have an admissible continuation and
universal choices are required only when admissible. The projection theorem
relates both the remaining domain and winning relation to this game for every
outer assignment. A separate theorem states that successive assertions retain
the conjunction of their clauses with one witness relation.
`mode_solver_binary_encoding.ml` connects the regionality bit encoding to the
existing quantifier-elimination theorem, including complement, existential,
and universal operations. These proofs establish the reference algebra used
by the executable adapter. The mutable hash-cons tables, cache reuse, and
heterogeneous compiler object encoding are still checked by executable
oracles and regression tests, rather than a complete Vox refinement proof.

The installed compiler passes `symbolic_ownership.ml` (which forces symbolic
state) and `typing-mode-polymorphism/curry_mode_subsumption.ml` with
`OCAML_EXPERIMENTAL_SYMBOLIC_MODES=1`. Nineteen focused regressions also pass
when forced through symbolic state, including copying, nested scopes,
conditional consequences, rigidification, and exception rollback. The full
mode-polymorphism capacity gate is being run before enabling the symbolic
fallback by default. The graph-tautology handoff remains disabled.

## Scope exit and irreducible bounds: current gate

The production adapter now assigns ordered subsumption scopes and projects
local quantified handles when a successful `Ctype.moregeneral` returns. The
scope oracle checks all nine outer regionality assignments for the residual
`y <= x` of `forall r >= x. exists a. y <= a <= r`. Nested dynamic scopes,
shared witnesses, and correlated join/meet envelopes pass forced symbolic
regressions. `exact_scope_use.ml` also passes with the fresh compiler.

Symbolic assertion, envelope, and zap construction now use lattice
irreducibles instead of enumerating Cartesian products of mode supports.
`mode_solver_irreducible_bounds.ml` proves the regionality instances of the
order decomposition and the join-lower/meet-upper rewrites. This proof does
not establish the generic finite-lattice irreducible discovery algorithm.
Snapshot reconstruction skips immutable graph factors already entailed by
imported components. A one-entry snapshot cache checks component identity,
member identity, and every graph stamp before reuse.

The production gate still fails. `basic.ml` and `subsumption.ml` report
resource exhaustion and changed inferred modes. In particular, after a
successful `Good_client : module type of Producer` inclusion, a subsequent
invalid `Bad_client` with a local curry mode is accepted. The isolated
`curry_mode_subsumption.ml` continues to reject it. `alloc_mode.ml` still
exceeds a 300-second limit. These failures prohibit enabling symbolic
fallback by default or claiming that the compiler adapter refines the
reference semantics. They also demonstrate that algebraic projection proofs
and isolated solver oracles do not cover compiler scheme lifecycle and
successive module inclusions.

## Hidden witnesses and constant connectors

The successive-inclusion acceptance regression was caused by copying the
visible generic handles while retaining the same hidden witnesses above the
generic level. Those witnesses are now freshened with the copied residual,
retain existential status, and receive the new scope order. The compiler
regression `curry_mode_subsumption_after_success.ml` rejects the invalid
inclusion after a successful inclusion. A direct ownership test also checks
that a hidden witness can depend on the new universal handle.

Fixed graph variables no longer connect otherwise independent snapshots
through every incoming edge. Their constraints are substituted into retained
guard and witness relations before the handles are removed from ownership.
Outer handles proven constant by the guard are materialized in the graph,
with logged changes. `mode_solver_fixed_projection.ml` proves regionality
projection equals substitution under the explicit fixed-value hypothesis,
and that substitution preserves conjunction. Establishing that hypothesis
from mutable compiler state remains an adapter obligation.

The fresh bytecode compiler now passes `basic.ml`, `alloc_mode.ml`,
`mode_polymorphism_printing.ml`, currying, records, tuples, GADTs, modules,
labelled arguments, short paths, and the focused scope/instantiation tests.
The 20 focused symbolic oracle/ownership tests pass. At the latest completed
broader gate, `functions.ml` still had one resource-limit diagnostic in place
of its expected portability error, and `subsumption.ml` still exhausted the
limit on `M_restruct`; several quantified-failure diagnostics also differ.
Symbolic fallback remains opt-in. This is not a completed capacity gate.

Diagram work limits now apply per public operation. Explicit compaction
retains nodes reachable from the current guard and witness without reusing
node identities, so older rollback roots remain valid. An oracle checks old
roots after compaction. Bit ordering can keep the two bits of a three/four
value chain together while separating the independent bits of diamond
lattices. A bounded oracle constructs 20 independent four-value inequalities.
The unsafe graph-tautology handoff has been removed entirely.

## Toplevel self-check, rollback, and guarded blocks

The toplevel checks its inferred signature against its simplified signature.
That call now explicitly uses the existing self-check inclusion mode. Ordinary
subsumption at this call introduced inappropriate quantified constraints.
`exact_toplevel_self_check.ml` reproduces the resulting false portability
error; it and the full `functions.ml` now pass. The changed labelled-argument
signature is checked in both inclusion directions by
`exact_labelled_subsumption.ml`.

Mode copying and generalization now log retained ownership and level changes.
Copy-scope finalization is excluded, as explained in the installed-suite
regression below. Fresh-variable registration is logged at creation
time so nested rollback restores registry entries in mutation order.
`Ctype.is_moregeneral`, used as a predicate by value-printer selection, always
backtracks its speculative changes. These lifecycle changes are compiler
obligations; the finite-lattice theorems do not establish their correctness.

`mode_solver_quantifier_blocks.ml` has been checked with Vox. It proves that
two adjacent guarded existential blocks and two adjacent guarded universal
blocks on regionality equal the corresponding nine-assignment formulations.
The production adapter coalesces adjacent blocks of the same quantifier kind,
preserves their domain guards, and avoids computing unused final domains.

Direct diagram implication tests avoid constructing complemented intersections
for constant detection and mode envelopes. Fixed-variable substitution is
fused with import into a fresh manager. Support analysis removes handles absent
from both retained relations. Exhaustive heterogeneous-domain oracles cover
implication, support, fixed import, reordered import, and counterexamples.
Quantified-failure diagnostics examine unavoidable irreducible clauses; optional
refinement falls back to an ordinary counterexample on resource exhaustion.

Compaction now tracks newly accumulated entries rather than repeatedly scanning
a large unchanged live diagram after every graph edge. Residual construction
and assertion conjunction compact at operation boundaries as well. The node
budget has not been increased. The production capacity gate remains open until
the full subsumption test and installed compiler suites pass. Mutable diagram
internals, cache reuse, hidden-witness lifecycle, and the compiler adapter still
lack an end-to-end refinement proof.

The capacity investigation subsequently tried a 2,000,000-entry budget for
measurement. It still exhausted the budget while merging retained relations
in `M_restruct`; that experiment does not establish a production capacity
repair. Ordering now preserves retained component sequences, puts shared
handles first, and is being tested with adjacent identity-equality handles.
Graph reconstruction skips constraints already entailed by a retained guard
and can conjoin several relations without constructing prefix conjunctions.
The n-ary conjunction primitive has an exhaustive heterogeneous-domain oracle.
The complete 17 ordinary expect fixtures outside `subsumption.ml` and the
separately configured short-path fixture pass. The installed native compiler
compiled `performance.ml` in 3.0 seconds. The full installed directory and
whole-compiler suite gates have not yet completed.

## Checked public boundary (2026-09-25)

Read these files, in order, under `testsuite/tests/vox/`:

1. `mode_solver_semantics.ml` and `.mli`: the three-element carrier, order,
   morphisms, term/formula syntax, total evaluation, de Bruijn indices and scope,
   and the guarded subsumption formula.
2. `mode_solver_graph_semantics.ml` and `.mli`: graph satisfaction and scope,
   including existential projection.
3. `mode_solver_guarded_semantics.ml` and `.mli`: admissible assignments,
   quantifier order, guarded game, normalization, and scope under a prefix.
4. `mode_solver_public.mli`: actual executable operations and their exactness
   and scope contracts. This includes graph subsumption and scope composition.

The semantic interfaces expose checked definition lemmas. The public interface
exposes no proof-module types or implementation aliases. Implementations live
in `mode_solver_three_qe_proof.ml`, `mode_solver_graph_qe_proof.ml`,
`mode_solver_retained_symbolic.ml`, and `mode_solver_public.ml`.
`project_admissible` uses normalization after every quantifier step; an
inductive proof connects this algorithm to `admissible && game` and proves
that its result is scoped over the remaining outer variables.

`mode_solver_public_client.ml` derives exact executable answers using only
these public interfaces. It was also compiled in a directory containing only
the four public `.cmi` files, with no proof module interfaces. Its runtime
checks distinguish forall-exists equality from exists-forall equality and
reject an unbound variable. The generated Lambda contains evaluation and
solver calls but no calls to the ghost correctness lemmas.

The established auxiliary results (representation gaps, conditional cuts,
atomic state updates, binary encoding, scope obstructions, and quantifier
coalescing) remain in their existing `mode_solver_*.ml` fixtures. Their theorem
statements and local semantic definitions remain additional review surfaces;
the public core interface does not silently replace or subsume those claims.
All 36 source/interface files in the solver proof fixtures type-check after
the split. All 24 solver fixtures pass through the normal `./dev test`
runner, along with `ghost_erasure.ml`, `ghost_function_records_rejected.ml`,
and `negative_totality.ml`.

These claims concern an allocating, finite executable reference over three
regionality values. Exhaustive elimination can grow exponentially. They do
not prove the mutable production BDD/factor implementation, compiler state
rollback, caches, or the compiler adapter. The production resource limit is
an operational outcome, not a logical unsatisfiability result. Symbolic mode
solving remains opt-in while the production gates remain open.


### Production follow-up gates

The initial factored backend avoided the eager BDD node exhaustion but still
exceeded 300 seconds in the installed native compiler on the initial three
modules of `typing-mode-polymorphism/subsumption.ml`. Sampling identified
repeated satisfiability searches in fixed-variable materialization and mode
envelopes. The follow-up implementation reuses independent satisfying
assignments and a bounded, copied counterexample cache, and factors shared
conjuncts out of edge diagrams. The finite-domain oracle covers the new
factorization, both search preferences, and mutation of returned assignments.
The full compiler capacity and installed/native suite gates are still pending;
these changes are not a claim that production validation is complete.

The native `expectnat` runner in this checkout emits x86 assembly on this ARM
host, so its assembly failures are not useful solver evidence. Installed
`ocamlc.opt` typing checks and the ordinary bytecode expect runner are used
for the corresponding checks. No assembler failures or resource failures
were promoted into reference output.


### Sequential signature reconstruction

A reduced production counterexample defines `id` followed by `compose`, then
reconstructs the same module against `module type of` the first. Comparing
`compose` alone succeeds; comparing `id` first makes it fail. Backtracking the
`id` comparison restores success. Restricting cached generalization copies to
the current subsumption scope did not fix it, and that experiment was reverted.
Disabling scope-exit elimination also failed and was reverted.

The `id` comparison snapshots a graph containing variables of the other value.
It leaves a retained relation that changes subsequent copying even when that
relation is already implied by the ordinary graph. Scope exit now reconstructs
the remaining graph and checks that it implies **both the retained guard and
the retained witness relation** before removing the retained component. This
is stronger than checking a quantified assertion for validity: live witness
correlations are preserved, and the check occurs after local quantifiers have
been eliminated. Failure to establish implication, including resource limits,
keeps the retained component.

The reduced reconstruction now passes in 1.3 seconds in the rebuilt native
frontend (previously a mismatch after about 32 seconds). The original first
three subsumption modules pass in 6.9 seconds. All 21 focused forced-symbolic
fixtures pass. `exact_signature_reconstruction.ml` adds a compiler regression
including reordered declarations. Full subsumption and installed compiler
gates are still running; this is not yet completion evidence.


### Implicit refinement migration

The 24 solver-specific source files containing `refine_` now use implicit
refinement introduction/elimination: 267 obsolete tokens and 13 redundant
result/proof aliases were removed. The four checked public interfaces are
unchanged; no assumptions or replacement wrappers were introduced.

Both bytecode and native compilation check 36 dependency files, and all 24
solver runtime fixtures pass in both modes. A separately compiled client sees
only the four public `.cmi` files. Its lambda output retains calls to
`assert_subsumption`, `project_admissible`, `eliminate`, and `decide_checked`,
with proof calls erased. A false unit refinement is rejected.

These checks used the existing read-only `time-credits/_install` compiler
(HEAD `9d5d8fca7a3261a3b06c293fe7aa30a38b9a2ef0`, supplied by the cleanup
coordinator); they do not assert a new compiler change. Detailed artifacts are
in `/tmp/mode-implicit-check.log` and the directory recorded by
`/tmp/mode-implicit-stage`.


### Installed/native follow-up

The first full installed suite exposed a missing `Solver_mdd` entry in both
manually assembled dynlink archives. The archive lists now include that module;
unused `Solver_graph`/`Solver_graph_compact` copies were removed from dynlink's
private build.

It also exposed a missed inferred-signature self-check in compilation without
an `.mli`. Typing `fatal_application_modes.ml` with `-i` took 0.006 seconds of
typing work, while normal compilation entered a large retained-relation search.
`Includemod.compunit` now accepts the existing self-check distinction, and
`Typemod` passes it only for the inferred-signature comparison. Actual interface
and argument-interface checks retain ordinary subsumption. This preserves mode
identities in the self-check while retaining type compatibility checking.

The outdated full run was stopped to rebuild these fixes. Formatting,
boot-compiler, and install pass again. In the refreshed installed directory run,
the native performance fixture, fatal application modes, both curry rejection
fixtures, the new reconstruction regression, and short-path prerequisites pass.
The full subsumption fixture and remaining complete-suite checks are pending.


The refreshed installed gates now pass: 26/26 `typing-mode-polymorphism`
tests (211 seconds), 65/65 `typing-modes` tests, and the previously failing
`backtrace_dynlink.ml`. The full suite has restarted with the refreshed
installation and `OCAML_EXPERIMENTAL_SYMBOLIC_MODES=1`.
The implicit-refinement proof sources also pass the original verification
checkout's installed compiler: all 36 dependency files check, so the migration
does not require switching or rebasing that checkout.


### Copy finalization and the complete installed suite

The complete symbolic-enabled installed run finished with 2566 passes, 261
skips, and one failure among 2828 tests. The failure was the existing
`typing-jkind-bounds/universal.ml` acceptance case involving a polymorphic
record field and a GADT equality refining `Abs.t` to `unit`, under `-no-ikinds`.
A clean export of production HEAD `b51084e2da` accepts the reduced case, so this
was a regression introduced by the adapter changes.

Isolation identified rollback logging of copy-scope finalization. Copy
finalization establishes edge orientation and propagates bounds at the copy's
level. Restoring the pre-finalization graph on backtracking does not restore
the copy's original level. Types retained across backtracking can therefore
observe an invalid mode graph. Filtering the log to omit only fresh copies
was insufficient: finalization also modifies edges on shared variables.

The fix restores permanent copy finalization, while preserving logging of
ordinary solver assertions, generalization, and retained ownership. The
reduced batch case and the original expect fixture (both principal and
non-principal) pass. All 21 forced-symbolic regression fixtures pass. A new
format/boot/install and complete installed suite run is in progress; logs use
`/tmp/mode-final3-*.log`.


The final refreshed kind-bounds directory passes 76/76. The complete installed
suite then passes 2567 tests with 261 skips and no failures or unexpected errors.
The final symbolic suite took 909.5 seconds. All source hashes match the
pre-suite manifest. There are no remaining observed compiler-gate failures.
Production Merlin synchronization is proceeding under the standing
authorization supplied by the coordinating task.
