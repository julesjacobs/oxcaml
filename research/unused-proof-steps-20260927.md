# Unused proof steps from unsat cores (27 September 2026)

Readiness item "Unused proof steps from unsat cores". Branch
`jujacobs/vox/unsat-core-20260927`. This note records the design, the
cost on the vox test suite (which builds the verified library and every
catalogue demo), and the unused steps found. The steps are listed, not
removed: other branches are editing the demos.

## What it does

Warning 227 `unused-proof-step`, off by default (`-w +unused-proof-step`),
reports three kinds of proof step whose facts no refinement proof in their
function used:

- a lemma call: an application whose result is a refinement of `unit`
  (`size_def xs;`, `put_frame h p v x;`);
- an `assume_`;
- a refinement written on a parameter of a `let`-bound function
  (`(x : {x : int | x > 0})`, or a dependent arrow in its annotation).

```
Warning 227 [unused-proof-step]: No refinement proof in this function used the fact from this
  call to "Lemmas.double_def".
Warning 227 [unused-proof-step]: No refinement proof in this function used the fact from this "assume_".
Warning 227 [unused-proof-step]: No refinement proof in this function used the refinement of
  argument "x".
```

`-smt-unused-steps-precise` adds the precise mode. For test harnesses,
`VOX_UNUSED_STEPS=FILE` checks every step, whatever the warning settings,
and appends the unused ones to `FILE` (`file:line:col-line:col: unused
lemma call f`), like `VOX_SLOW_PROOFS`; `VOX_UNUSED_STEPS_PRECISE=1`
selects the precise mode there.

## Design

**Naming facts.** VC generation already records each assumption as an
`Assume` command and each named term as a `Define` command. While a step
is evaluated, `Vox_proof_steps.current` names it, and every command made
then is recorded against the step (a side table keyed by the command's
physical identity, so `vox_vc.ml` keeps its command type). The query
builder, asked for indicators, replaces each such assumption `p` by
`u => p` and each definition `v = t` by `u => (v = t)`, with a fresh Boolean
`u` per command. `Vox_smt.to_smtlib ~assumptions` then ends the query with
`(check-sat-assuming (u1 ... un))` and asks for `(get-unsat-core)`. A step
is used when one of its indicators is in a core.

**Normal proofs are unchanged.** Indicators only appear in the second
queries. Those run after all the proofs of a pass (a unit's generation or
a termination check), so they cannot change the proofs or use their
`-smt-budget`; an exhausted budget abandons the check and reports nothing.
Without the warning no step is created and nothing else runs. On the whole
vox suite the 13,336 normal queries have the same SMT-LIB text with and
without the check (compared through the query cache keys), and in the
final runs the same resource counts.

**Which query.** The check proves again exactly what was proved: the whole
batch when the batch was proved, else each refinement's group, else each
obligation, each with its own core (per-goal cores for batches that fell
back). Bitwise queries are first tried with bitwise operations abstracted,
as proofs are. Outcomes are cached in the query cache (attempt `unsat core`,
entry `core RESOURCES INDICES...` or `unproved RESOURCES`).

**Conservative reports.** Cores are not minimal. A step is reported only if
it is absent from every core covering it; since each query is proved from
its core alone, all reported steps can be removed together (up to the
solver's search, see below). A second query that is not proved counts every
step it mentions as used. The precise mode shrinks each core by proving the
query again without each unused-so-far step in it (deletion-based
minimization), which finds more unused steps.

**Mapping back to source.** A step is its source location and kind: the
lemma call expression (with the function's name), the `assume_`, or the
parameter pattern. The same code is evaluated again by termination checks
(before the unit's proofs) and inside nested functions; a step used by any
of these proofs is used.

**What else a step contributes.** Facts are not the only thing a step adds
to a query, and the first validation (below) found two cases where removing
a reported lemma call broke a proof. The check therefore also guards:

- the observations that expansion derives from a step's terms (heap and
  array observations requested by `H.at (H.put h p v) x` in a lemma's
  conclusion): expansion visits unguarded facts first, then each guarded
  fact under its indicators, which it propagates to what it derives,
  through heap and array equality edges as well;
- observation equations and symbols recorded in the context while a step was
  evaluated;
- the evaluation of a lemma call's arguments (its terms can request
  observations); a step nested in another's arguments marks its parent used;
- re-exposures of a checked parameter's value (at each use, and in
  predicates), which belong to the parameter as well as to any enclosing
  step.

A step that makes its path impossible (`assume_ u : {u : unit | false}`)
is used: no obligation is generated on that path, so no core can show it.

**Steps the typer needs.** A refined value is also accepted where its own
refined type is expected, with no obligation (the typer unifies the types,
or cancels an elimination against a re-introduction). A core cannot see
that use. So a step is only checked when its refined value is not used
that way: a lemma call or `assume_` whose refinement is converted where it
is computed (a statement, a result at another refinement), or which is
bound by a `let` without variables; a parameter that the function never
uses at a refined type without a conversion. Parameters of anonymous
functions passed as arguments (callbacks) are not reported: their
refinements come from the callee's type. `let check x : positive =
assume_ x` is therefore never reported. Refinements named by a type
abbreviation (`(x : small)`, `B.u32`) are part of that type, not a proof
step, and are not reported; nor are the parameters of generated definition
lemmas.

## Code

- `verification/vox_proof_steps.ml` (new): steps, the check of one query,
  the precise mode, passes and reporting.
- `verification/vox_vc.ml`: step creation (`expression_step`,
  `argument_step`, `in_step`), recording in `branch` and `name`,
  indicators in `query` and guard propagation in its `expand`, `?proved` on
  `verify_batch`, and `verify_steps`. The changes are in the obligation and
  query layer; the evaluator only gains the `in_step` wrappers.
- `verification/vox_smt.ml`, `vox_smt_solver.ml`: `?assumptions`,
  `check-sat-assuming`, `get-unsat-core`, `result.core`.
- `verification/runtime/vox_verify.enabled.ml`: the core callback and its
  cache, the flag, the harness report.
- `utils/warnings.ml`: warning 227, disabled by default.

## Tests

- `vox/unused_proof_steps.ml`: a needed lemma call (no warning); an unneeded
  one; a lemma call needed by the typer; an unused `assume_`, one needed by
  the typer and one needed by a proof; an `assume_` of a parameter; an
  impossible path; parameters (used, unused, passed at their own type, named
  by an abbreviation); a batched function with several goals; steps about
  arrays, whose terms request observations; a function without proofs; a
  `[@warning]` attribute; a non-minimal core (not reported).
- `vox/unused_proof_steps_precise.ml`: the precise mode reports the step the
  non-minimal core kept.
- `vox/unused_proof_steps_split.ml`: a batch that falls back to one query per
  refinement (`-smt-resource-warning 1`), with a core for each.

## Cost

Measured with `./dev test vox` (467 tests: 460 run, 7 skipped; the run
builds the verified library and every catalogue demo) on the AMD box
(Ryzen 9 7950X3D, 32 threads), which other agents were using at the same
time, so wall-clock times vary with their load. "Cold": empty verification
cache and a rebuilt test library. "Warm": the library rebuilt with the
cache of the previous run (every query cached), as in `dev test` after a
compiler rebuild. The check was enabled with `VOX_UNUSED_STEPS`, which
checks every step.

Solver work, in Z3 resource units (`rlimit`), does not depend on load:

| | queries | resource units |
|---|---|---|
| normal proofs | 13,339 | 866.6 million |
| core check: second queries | +10,137 (18 not proved) | +812.8 million (+94%) |
| precise mode: second queries and proofs without a step | +39,036 (28,372 not proved) | +5,480 million (+632%) |

Times (the test phase is dominated by the library build, whose critical
path is long; "load" is the 1-minute load average at the start):

| run | load | test phase wall | total CPU (user) |
|---|---|---|---|
| no check, cold (four runs) | 6–8 | 275–323 s | 3,542–3,814 s |
| no check, cold, busy machine | 21 | 630 s | 4,779 s |
| core check, cold (three runs) | 3–31 | 409–527 s | 5,183–5,633 s |
| precise mode, cold (two runs) | 8–11 | 575–629 s | 8,884–8,944 s |
| no check, warm | 9 | 230 s | 2,642 s |
| core check, warm | 25 | 235 s | 2,814 s |

(The core and precise CPU times include a rebuild of the compiler's test
artifacts, about a minute of wall time, in three of the five runs.)

So, cold, the core check roughly doubles the solver's work, which costs
about 35–60% more CPU time, and 27–49% more wall time in the quietest run
(409 s against 275–323 s), more under load. The precise mode costs about 2.4
times the CPU time of a run without the check; most of its extra work is
proofs without a step that fail, each limited to the warning threshold (one
million units). Warm, it costs almost nothing (+2% wall, +6% CPU): second
queries are cached like proofs, and a unit whose report is cached replays it
(the unit cache records the reported steps). The reports are identical cold
and warm, and between runs.

**For `dev test` by default:** the cost is not small on a cold cache
(about as much solver time again), but `dev test` keeps its cache, so in the
edit loop only changed units pay, about twice their solver time. That
seems acceptable for a report listed at the end like slow proofs
(`VOX_UNUSED_STEPS=$run_dir/unused-steps`, then print it), but it would
double the cost of the first run after a cache wipe or a solver upgrade. I
have not turned it on. The precise mode is too slow for a default.

**Normal proofs are unchanged.** In every pair of runs the normal queries
had the same SMT-LIB text (the same cache keys). In the final pair (with and
without the check) their outcomes and resource counts matched exactly; in
other pairs one to five queries differed by a few units (e.g. 116,412 vs
116,415), as they do between two runs without the check: a query's resources
depend slightly on the queries sent before it in the same Z3 session, and
the query cache makes that depend on test scheduling. Once this flipped a
batch attempt from "exhausted" to "proved" at the warning threshold, which
only changes whether the batch is split.

## Unused steps found

With every step checked (`VOX_UNUSED_STEPS`) on the vox suite, the core mode
reports 1,000 steps: 923 lemma calls, 10 `assume_`s and 67 parameters. Of
these, 982 are listed below (920 lemma calls, 10 `assume_`s, 52 parameters,
in 317 files). The precise mode reports 1,415 (1,305 lemma calls, 14
`assume_`s, 96 parameters); the 409 it adds are marked. They are listed below by
catalogue demo (a file belongs to a demo by the catalogue's `owned`
prefixes; test clients that match no prefix are listed last). Reports are
deterministic: the same in every run, cold or warm.

**Validation by removal.** To check the reports, I replaced every reported
lemma call by `()` in a copy of the tree (874 calls in the core mode's list,
1,299 in the precise mode's, from runs whose lists of lemma calls differ
from the final ones only in a test of this branch) and ran the vox suite
again, with warnings off. Everything still verified, except two proofs in
the RSA library that then exceed the 40-million-unit resource limit:

- `vox_rsa_number_theory.ml:83` (core mode) without the calls
  `remainder_unique multiple l quotient 0Z` (line 82) and
  `remainder_unique difference (p * q) (factor / p) 0Z` (line 110);
- `vox_rsa_arithmetic.ml:109` (precise mode) without
  `remainder_unique (n * k) n k 0Z` (line 108).

Z3 proves these goals without those facts (the second queries show it),
but in the normal encoding, without the facts, its nonlinear search does
not finish in budget: an unused fact can still steer the search. Such
steps should stay, and the list marks them. Removing everything else
produced only unused-variable warnings (in `dfa_equivalence_proof.ml` and
`int_lists.ml`, where the removed calls were the variables' only uses).
Reports without a file name (from some expect tests) were matched to
their file by line and text; 15 of the core mode's (33 with the precise
mode's) could not be, and are left out.

An earlier version, which guarded only the steps' facts, reported two calls
whose removal broke proofs (`copy_heap_proofs.ml:21`, `put_frame`;
`vox_lz4_roundtrip.ml:175`, `heap_put_at`): their conclusions mention
`H.at (H.put h p v) x`, which makes expansion add heap observations that the
proof needed. That is why derived observations are guarded too.

**What the reports are.** Reading a sample:

- definition lemmas (`*_def`) whose unfolding the proof did not need, the
  most common case, often called more than once (`U.member_def x0 (borrow_
  state)` at lines 286 and 288 of `union_find_online.ml`, both reported);
  generated proofs such as `hm_routed_infer.ml` call many;
- lemma calls in test clients that exercise a lemma API without proving
  anything after them (`avl_set_client.ml`, `int_lists.ml`): unused by
  design;
- `assume_`s whose fact is also derivable, e.g. in `rsa.ml` (precise mode)
  the loop bounds already give `e >= 0Z` and `n > 0Z`;
- lemma premises passed as unit arguments that no proof needs (`_factor`,
  `_fit`, `_premise` in the Hindley–Milner demos), and preconditions on
  parameters that could be weakened.

A step is unused for the proofs that exist; a lemma call can also document
why a proof holds, and an unused precondition can be part of an interface
on purpose. The list is input for the proof cleanup, not a set of edits to
apply blindly.

## Vox limitations met

- A refined value is accepted where its own refined type is expected without
  an obligation, so a check at the SMT level cannot see those uses; the
  check has to recognize them from the typed tree (conversions, `let`
  patterns, uses of parameters).
- Lemma calls also act as triggers: their terms make expansion add heap and
  array observations. Removing a call whose fact is unused can still lose
  those observations.
- A fact no proof needs can still decide whether Z3 finishes within its
  resource limit (nonlinear arithmetic in RSA).
- A query's resource count depends slightly on the queries sent before it in
  the same solver session, although each query starts with `(reset)`; with
  the query cache, this depends on test scheduling.
- The expect tool gives no file name and numbers lines from each phrase (its
  character offsets count from the start of the file), and some expect tests
  pass the compiler no `.ml` argument, so a harness cannot always name the
  file of a location.
- Generated definition lemmas have the location of the whole definition, not
  a ghost location.
- `assume_` needs a plain local variable: `let u = () in assume_ u`.

## List

Positions are `file:line:column` of the call, `assume_` or parameter.

### flat-hash-table (62 lemma calls, 9 arguments)

- `verification/library/vox_table_bindings.ml:90:10`: lemma call `Key.transitive`
- `verification/library/vox_table_bindings.ml:219:44`: lemma call `erase_absent` (precise mode only)
- `verification/library/vox_table_bindings.ml:281:6`: lemma call `count_def`
- `verification/library/vox_table_bindings.ml:286:21`: lemma call `Assoc.count_nonnegative`
- `verification/library/vox_table_bindings.ml:292:6`: lemma call `count_def`
- `verification/library/vox_table_bindings.ml:296:21`: lemma call `Assoc.count_nonnegative`
- `verification/library/vox_table_bits.ml:26:5`: argument `lane`
- `verification/library/vox_table_bits.ml:30:2`: lemma call `clear_def` (precise mode only)
- `verification/library/vox_table_coverage.ml:6:5`: argument `modulus` (precise mode only)
- `verification/library/vox_table_coverage.ml:18:5`: argument `modulus` (precise mode only)
- `verification/library/vox_table_coverage.ml:38:4`: lemma call `W.wrap_range` (precise mode only)
- `verification/library/vox_table_coverage.ml:41:6`: lemma call `W.scale16_def` (precise mode only)
- `verification/library/vox_table_coverage.ml:41:23`: lemma call `W.scale16_def` (precise mode only)
- `verification/library/vox_table_coverage.ml:46:6`: lemma call `W.scale16_def` (precise mode only)
- `verification/library/vox_table_coverage.ml:49:6`: lemma call `W.scale16_def`
- `verification/library/vox_table_coverage.ml:104:38`: lemma call `W.wrap_range` (precise mode only)
- `verification/library/vox_table_coverage.ml:108:4`: lemma call `block_bounds` (precise mode only)
- `verification/library/vox_table_implementation.ml:94:56`: argument `view`
- `verification/library/vox_table_implementation.ml:141:13`: lemma call `Proof.absent_after_rebuild`
- `verification/library/vox_table_implementation.ml:146:8`: lemma call `H.put_law` (precise mode only)
- `verification/library/vox_table_initial.ml:74:7`: argument `capacity` (precise mode only)
- `verification/library/vox_table_initial.ml:113:6`: lemma call `index_bounds` (precise mode only)
- `verification/library/vox_table_initial.ml:116:8`: lemma call `index_step` (precise mode only)
- `verification/library/vox_table_insert.ml:56:6`: lemma call `H.put_law` (precise mode only)
- `verification/library/vox_table_insert.ml:57:6`: lemma call `H.put_law` (precise mode only)
- `verification/library/vox_table_insert.ml:115:11`: lemma call `I.reserve_def` (precise mode only)
- `verification/library/vox_table_insert_proofs.ml:257:8`: lemma call `arithmetic` (precise mode only)
- `verification/library/vox_table_map.ml:182:9`: lemma call `Key.transitive`
- `verification/library/vox_table_map.ml:185:9`: lemma call `lookup_congruent`
- `verification/library/vox_table_map.ml:186:9`: lemma call `absent_lookup`
- `verification/library/vox_table_map.ml:191:12`: lemma call `Key.symmetric`
- `verification/library/vox_table_map.ml:224:9`: lemma call `lookup_congruent`
- `verification/library/vox_table_map.ml:231:12`: lemma call `Key.symmetric`
- `verification/library/vox_table_map.ml:310:11`: lemma call `Key.transitive`
- `verification/library/vox_table_mask.ml:44:19`: lemma call `prefix_def`
- `verification/library/vox_table_migrate.ml:79:8`: lemma call `H.put_law`
- `verification/library/vox_table_migrate.ml:88:8`: lemma call `next_index` (precise mode only)
- `verification/library/vox_table_migrate.ml:108:17`: lemma call `H.put_law` (precise mode only)
- `verification/library/vox_table_migrate.ml:140:13`: lemma call `H.put_law` (precise mode only)
- `verification/library/vox_table_model_proofs.ml:80:2`: lemma call `M.slot_def` (precise mode only)
- `verification/library/vox_table_model_proofs.ml:95:2`: lemma call `M.control_def` (precise mode only)
- `verification/library/vox_table_model_proofs.ml:168:44`: lemma call `length_nonnegative`
- `verification/library/vox_table_mutation.ml:110:15`: lemma call `H.put_law` (precise mode only)
- `verification/library/vox_table_mutation.ml:145:6`: lemma call `H.put_law` (precise mode only)
- `verification/library/vox_table_mutation.ml:146:6`: lemma call `H.put_law` (precise mode only)
- `verification/library/vox_table_mutation.ml:165:8`: lemma call `H.put_law` (precise mode only)
- `verification/library/vox_table_mutation.ml:179:8`: lemma call `M.slot_def`
- `verification/library/vox_table_occupancy.ml:36:6`: lemma call `L.length_nonnegative` (precise mode only)
- `verification/library/vox_table_occupancy.ml:40:10`: lemma call `index_step` (precise mode only)
- `verification/library/vox_table_probe.ml:14:5`: argument `modulus` (precise mode only)
- `verification/library/vox_table_probe.ml:40:5`: argument `left` (precise mode only)
- `verification/library/vox_table_probe.ml:41:5`: argument `right` (precise mode only)
- `verification/library/vox_table_probe.ml:75:5`: argument `value` (precise mode only)
- `verification/library/vox_table_probe.ml:166:35`: lemma call `groups_range`
- `verification/library/vox_table_probe.ml:189:35`: lemma call `groups_range` (precise mode only)
- `verification/library/vox_table_read_proofs.ml:96:23`: lemma call `I.power_of_two_def`
- `verification/library/vox_table_read_proofs.ml:99:6`: lemma call `Vox_table_model_proofs.index_bounds`
- `verification/library/vox_table_read_proofs.ml:137:6`: lemma call `W.wrap_range` (precise mode only)
- `verification/library/vox_table_read_proofs.ml:142:6`: lemma call `W.wrap_range` (precise mode only)
- `verification/library/vox_table_resize.ml:57:13`: lemma call `H.put_law` (precise mode only)
- `verification/library/vox_table_resize.ml:65:14`: lemma call `double_capacity` (precise mode only)
- `verification/library/vox_table_resize.ml:90:14`: lemma call `double_capacity` (precise mode only)
- `verification/library/vox_table_search.ml:126:6`: lemma call `I.valid_def` (precise mode only)
- `verification/library/vox_table_search.ml:126:24`: lemma call `R.capacity_bounds` (precise mode only)
- `verification/library/vox_table_search.ml:130:43`: lemma call `R.group_in_shape` (precise mode only)
- `verification/library/vox_table_stop_proof.ml:50:8`: lemma call `R.route_equal_key` (precise mode only)
- `verification/library/vox_table_stop_proof.ml:90:10`: lemma call `R.route_equal_key`
- `verification/library/vox_table_update_proofs.ml:671:4`: lemma call `remove_arithmetic` (precise mode only)
- `verification/library/vox_table_vacancy.ml:120:6`: lemma call `Read.I.valid_def` (precise mode only)
- `verification/library/vox_table_vacancy.ml:120:29`: lemma call `Read.capacity_bounds` (precise mode only)
- `verification/library/vox_table_vacancy_progress.ml:76:8`: lemma call `Count.not_full`

### hindley-milner (144 lemma calls, 13 arguments)

- `testsuite/tests/vox/copy_algorithm.ml:33:15`: lemma call `Copy_heap_proofs.put_frame` (precise mode only)
- `testsuite/tests/vox/copy_cleanup_demo.ml:54:10`: lemma call `next_bounds`
- `testsuite/tests/vox/copy_cleanup_demo.ml:54:30`: lemma call `next_bounds`
- `testsuite/tests/vox/copy_cleanup_demo.ml:54:52`: lemma call `next_order`
- `testsuite/tests/vox/copy_cleanup_demo.ml:54:71`: lemma call `next_order`
- `testsuite/tests/vox/copy_cleanup_demo.ml:55:24`: lemma call `Copy_cleanup_proofs.touched_distinct`
- `testsuite/tests/vox/copy_cleanup_demo.ml:81:4`: lemma call `same_model`
- `testsuite/tests/vox/copy_cleanup_proofs.ml:27:8`: lemma call `members`
- `testsuite/tests/vox/copy_cleanup_proofs.ml:158:93`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/copy_cleanup_proofs.ml:162:86`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/copy_complete_proofs.ml:38:93`: argument `_fit`
- `testsuite/tests/vox/copy_demo.ml:70:77`: lemma call `rho_def` (precise mode only)
- `testsuite/tests/vox/copy_demo.ml:80:57`: lemma call `equal`
- `testsuite/tests/vox/copy_demo.ml:80:73`: lemma call `next`
- `testsuite/tests/vox/copy_heap_proofs.ml:27:46`: lemma call `Copy_spec.cell_def`
- `testsuite/tests/vox/copy_heap_proofs.ml:52:71`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/copy_heap_proofs.ml:58:86`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/copy_heap_proofs.ml:134:137`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/copy_heap_proofs.ml:136:86`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/copy_heap_proofs.ml:165:73`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/copy_model_proofs.ml:31:135`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/copy_model_proofs.ml:33:84`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/copy_order_demo.ml:52:10`: lemma call `next_bounds`
- `testsuite/tests/vox/copy_order_demo.ml:52:30`: lemma call `next_bounds`
- `testsuite/tests/vox/copy_order_demo.ml:52:52`: lemma call `next_order`
- `testsuite/tests/vox/copy_order_demo.ml:52:71`: lemma call `next_order`
- `testsuite/tests/vox/copy_sound_proofs.ml:34:76`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/forest_transport.ml:51:31`: lemma call `Copy_spec.cell_def`
- `testsuite/tests/vox/forest_transport.ml:82:52`: lemma call `unfolding_root` (precise mode only)
- `testsuite/tests/vox/generalize_datatype_demo.ml:37:4`: lemma call `Copy_spec.cell_def`
- `testsuite/tests/vox/generalize_datatype_demo.ml:37:29`: lemma call `Copy_spec.cell_def`
- `testsuite/tests/vox/generalize_demo.ml:43:49`: lemma call `Copy_spec.cell_def`
- `testsuite/tests/vox/generalize_demo.ml:43:63`: lemma call `Copy_spec.cell_def`
- `testsuite/tests/vox/generalize_demo.ml:54:58`: lemma call `Generalize_scheme_proofs.scheme_valid` (precise mode only)
- `testsuite/tests/vox/generalize_demo.ml:78:28`: lemma call `rho_def`
- `testsuite/tests/vox/generalize_demo.ml:87:57`: lemma call `equal`
- `testsuite/tests/vox/generalize_demo.ml:87:73`: lemma call `next`
- `testsuite/tests/vox/generalize_scheme_proofs.ml:18:2`: lemma call `scheme_root` (precise mode only)
- `testsuite/tests/vox/hm_complete_demo.ml:48:48`: argument `_fit`
- `testsuite/tests/vox/hm_complete_proofs.ml:271:28`: lemma call `Copy_heap_proofs.put_frame`
- `testsuite/tests/vox/hm_complete_proofs.ml:312:49`: lemma call `Copy_heap_proofs.put_frame`
- `testsuite/tests/vox/hm_complete_proofs.ml:324:50`: lemma call `Copy_heap_proofs.put_frame`
- `testsuite/tests/vox/hm_effective_agreement.ml:58:82`: lemma call `E.level_def`
- `testsuite/tests/vox/hm_effective_bound.ml:67:41`: lemma call `Hm_effective_execution_spec.copy_heap_def`
- `testsuite/tests/vox/hm_effective_complete.ml:125:140`: argument `fit1` (precise mode only)
- `testsuite/tests/vox/hm_effective_complete.ml:150:64`: lemma call `Forest_transport.unfolding_root`
- `testsuite/tests/vox/hm_effective_complete.ml:366:6`: lemma call `Copy_spec.cell_def`
- `testsuite/tests/vox/hm_effective_complete.ml:461:140`: argument `fit1`
- `testsuite/tests/vox/hm_effective_complete.ml:498:140`: argument `fit1`
- `testsuite/tests/vox/hm_effective_complete.ml:690:6`: lemma call `Hm_effective_execution_spec.allocated_def`
- `testsuite/tests/vox/hm_effective_complete.ml:690:69`: lemma call `Hm_effective_execution_spec.allocated_def`
- `testsuite/tests/vox/hm_effective_complete.ml:691:6`: lemma call `Copy_spec.cell_def`
- `testsuite/tests/vox/hm_effective_complete.ml:691:26`: lemma call `Copy_spec.cell_def`
- `testsuite/tests/vox/hm_effective_forest.ml:18:29`: lemma call `Copy_spec.cell_def` (precise mode only)
- `testsuite/tests/vox/hm_effective_generalization.ml:38:49`: lemma call `E.level_def`
- `testsuite/tests/vox/hm_effective_generalization.ml:101:10`: lemma call `E.level_def`
- `testsuite/tests/vox/hm_effective_infer.ml:136:6`: lemma call `Hm_effective_execution_spec.allocated_def`
- `testsuite/tests/vox/hm_effective_infer.ml:176:36`: lemma call `Copy_heap_proofs.put_frame`
- `testsuite/tests/vox/hm_effective_infer.ml:182:9`: lemma call `Hm_effective_execution_spec.allocated_def`
- `testsuite/tests/vox/hm_effective_infer.ml:277:6`: lemma call `Hm_effective_execution_spec.allocated_def`
- `testsuite/tests/vox/hm_effective_infer.ml:305:6`: lemma call `Hm_effective_execution_spec.allocated_def`
- `testsuite/tests/vox/hm_effective_infer.ml:353:6`: lemma call `Hm_effective_execution_spec.allocated_def`
- `testsuite/tests/vox/hm_effective_infer.ml:375:6`: lemma call `Hm_effective_execution_spec.allocated_def`
- `testsuite/tests/vox/hm_effective_infer.ml:405:6`: lemma call `Hm_effective_execution_spec.allocated_def`
- `testsuite/tests/vox/hm_effective_infer.ml:458:10`: lemma call `this function`
- `testsuite/tests/vox/hm_effective_infer_demo.ml:208:58`: argument `_factor`
- `testsuite/tests/vox/hm_effective_origin.ml:88:4`: lemma call `Hm_effective_execution_spec.copy_heap_def`
- `testsuite/tests/vox/hm_effective_origin.ml:114:4`: lemma call `Hm_effective_execution_spec.allocated_def`
- `testsuite/tests/vox/hm_effective_registration.ml:27:46`: lemma call `Copy_spec.cell_def`
- `testsuite/tests/vox/hm_effective_registration.ml:77:137`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/hm_effective_registration.ml:81:84`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/hm_effective_registration.ml:90:43`: lemma call `Hm_effective_execution_spec.copy_heap_def`
- `testsuite/tests/vox/hm_effective_registration.ml:211:113`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/hm_effective_registration.ml:214:113`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/hm_effective_sound.ml:73:62`: lemma call `Forest_transport.unfolding_root`
- `testsuite/tests/vox/hm_elaboration_demo.ml:34:56`: lemma call `Hm_elaboration.interpret_empty`
- `testsuite/tests/vox/hm_elaboration_test_support.ml:131:10`: argument `p`
- `testsuite/tests/vox/hm_execution_proofs.ml:102:6`: lemma call `Copy_heap_proofs.put_frame` (precise mode only)
- `testsuite/tests/vox/hm_execution_proofs.ml:106:8`: lemma call `Copy_heap_proofs.put_frame` (precise mode only)
- `testsuite/tests/vox/hm_let_runtime_proofs.ml:234:6`: lemma call `Hm_runtime_spec.runtime_at_def`
- `testsuite/tests/vox/hm_one_let_demo.ml:33:48`: argument `_factor`
- `testsuite/tests/vox/hm_origin_proofs.ml:59:37`: lemma call `Hm_execution_spec.allocated_def`
- `testsuite/tests/vox/hm_origin_proofs.ml:88:17`: lemma call `facts`
- `testsuite/tests/vox/hm_polymorphic_demo.ml:36:48`: argument `_factor`
- `testsuite/tests/vox/hm_polymorphic_proofs.ml:564:6`: lemma call `Hm_execution_spec.allocated_def`
- `testsuite/tests/vox/hm_principal_demo.ml:38:58`: argument `_factor`
- `testsuite/tests/vox/hm_readback_runtime_demo.ml:45:9`: lemma call `Copy_spec.cell_def` (precise mode only)
- `testsuite/tests/vox/hm_reconstruction_run.ml:134:54`: lemma call `Ty.cell_def`
- `testsuite/tests/vox/hm_reconstruction_run.ml:410:8`: lemma call `Ty.cell_def`
- `testsuite/tests/vox/hm_reconstruction_run.ml:410:34`: lemma call `Ty.cell_def`
- `testsuite/tests/vox/hm_reconstruction_run.ml:561:64`: lemma call `Forest_transport.unfolding_root`
- `testsuite/tests/vox/hm_reconstruction_run.ml:714:120`: argument `_premise`
- `testsuite/tests/vox/hm_registration_proofs.ml:19:137`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/hm_registration_proofs.ml:23:84`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/hm_routed_infer.ml:215:6`: lemma call `Hm_effective_execution_spec.allocated_def`
- `testsuite/tests/vox/hm_routed_infer.ml:257:36`: lemma call `Copy_heap_proofs.put_frame`
- `testsuite/tests/vox/hm_routed_infer.ml:269:9`: lemma call `Hm_effective_execution_spec.allocated_def`
- `testsuite/tests/vox/hm_routed_infer.ml:666:9`: lemma call `Hm_effective_execution_spec.allocated_def`
- `testsuite/tests/vox/hm_routed_infer.ml:683:6`: lemma call `Hm_effective_execution_spec.allocated_def`
- `testsuite/tests/vox/hm_routed_infer.ml:794:9`: lemma call `Hm_effective_execution_spec.allocated_def`
- `testsuite/tests/vox/hm_routed_infer.ml:811:6`: lemma call `Hm_effective_execution_spec.allocated_def`
- `testsuite/tests/vox/hm_routed_infer.ml:827:9`: lemma call `Hm_effective_execution_spec.allocated_def`
- `testsuite/tests/vox/hm_routed_infer.ml:844:6`: lemma call `Hm_effective_execution_spec.allocated_def`
- `testsuite/tests/vox/hm_routed_infer.ml:904:6`: lemma call `Hm_effective_execution_spec.allocated_def`
- `testsuite/tests/vox/hm_routed_infer.ml:931:6`: lemma call `Hm_effective_execution_spec.allocated_def`
- `testsuite/tests/vox/hm_routed_infer.ml:965:6`: lemma call `Hm_effective_execution_spec.allocated_def`
- `testsuite/tests/vox/hm_routed_infer.ml:1018:10`: lemma call `this function`
- `testsuite/tests/vox/hm_routed_infer_demo.ml:213:58`: argument `_factor`
- `testsuite/tests/vox/hm_template_generalization.ml:224:23`: lemma call `Effective_template.generic_def`
- `testsuite/tests/vox/hm_template_instance_demo.ml:25:4`: lemma call `P.scheme_wf`
- `testsuite/tests/vox/hm_template_instance_demo.ml:36:4`: lemma call `P.scheme_wf`
- `testsuite/tests/vox/hm_total_elaboration_demo.ml:22:67`: argument `q`
- `testsuite/tests/vox/level_copy_proofs.ml:51:4`: lemma call `scope` (precise mode only)
- `testsuite/tests/vox/level_copy_proofs.ml:53:6`: lemma call `scope`
- `testsuite/tests/vox/level_copy_proofs.ml:67:97`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/level_copy_proofs.ml:67:132`: lemma call `Copy_spec.mark_def`
- `testsuite/tests/vox/level_copy_proofs.ml:77:76`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/level_copy_proofs.ml:78:8`: lemma call `Copy_heap_proofs.put_frame` (precise mode only)
- `testsuite/tests/vox/level_copy_proofs.ml:78:55`: lemma call `Copy_heap_proofs.put_frame`
- `testsuite/tests/vox/level_copy_proofs.ml:82:73`: lemma call `Copy_spec.mark_def`
- `testsuite/tests/vox/level_copy_proofs.ml:82:95`: lemma call `Copy_spec.cell_def`
- `testsuite/tests/vox/level_copy_proofs.ml:88:47`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/level_copy_proofs.ml:88:82`: lemma call `Copy_spec.mark_def`
- `testsuite/tests/vox/level_finite_proofs.ml:17:30`: lemma call `Level_unifier_proofs.redirect_desc`
- `testsuite/tests/vox/level_lower.ml:23:4`: lemma call `Copy_heap_proofs.put_frame` (precise mode only)
- `testsuite/tests/vox/level_lower.ml:71:8`: lemma call `Level_proofs.lowering_at`
- `testsuite/tests/vox/level_lower.ml:109:8`: lemma call `Level_proofs.lowering_at`
- `testsuite/tests/vox/level_pool_execution.ml:77:47`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/level_pool_execution.ml:82:47`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/level_pool_execution.ml:109:6`: lemma call `E.copy_heap_def`
- `testsuite/tests/vox/level_pool_execution.ml:184:8`: lemma call `E.copy_heap_def`
- `testsuite/tests/vox/level_pool_routing.ml:43:15`: lemma call `Level_pool_routing_spec.insert_length`
- `testsuite/tests/vox/level_pool_routing_spec.ml:124:8`: lemma call `Representative_pool_spec.retained_rep_def`
- `testsuite/tests/vox/level_proofs.ml:121:2`: lemma call `frame` (precise mode only)
- `testsuite/tests/vox/level_unifier.ml:271:14`: lemma call `scope`
- `testsuite/tests/vox/level_unifier_datatype_demo.ml:44:61`: lemma call `active_all`
- `testsuite/tests/vox/level_unifier_datatype_demo.ml:44:76`: lemma call `active_all`
- `testsuite/tests/vox/level_unifier_proofs.ml:331:14`: lemma call `weight_positive`
- `testsuite/tests/vox/level_unifier_proofs.ml:339:14`: lemma call `weight_positive`
- `testsuite/tests/vox/level_unifier_proofs.ml:390:10`: lemma call `model`
- `testsuite/tests/vox/marked_occurs.ml:239:53`: lemma call `Copy_heap_proofs.put_frame`
- `testsuite/tests/vox/marked_occurs_proofs.ml:76:8`: lemma call `members` (precise mode only)
- `testsuite/tests/vox/pool_closing_equivalence.ml:18:35`: lemma call `Generalize_spec.close_cell_def`
- `testsuite/tests/vox/pool_closing_equivalence.ml:24:35`: lemma call `Generalize_spec.close_cell_def`
- `testsuite/tests/vox/pooled_allocator.ml:25:4`: lemma call `Copy_spec.cell_def` (precise mode only)
- `testsuite/tests/vox/pooled_allocator.ml:25:93`: lemma call `Copy_heap_proofs.put_frame`
- `testsuite/tests/vox/pooled_copy.ml:38:15`: lemma call `Copy_heap_proofs.put_frame` (precise mode only)
- `testsuite/tests/vox/pooled_demo.ml:129:20`: lemma call `solution`
- `testsuite/tests/vox/pooled_demo.ml:129:35`: lemma call `solution`
- `testsuite/tests/vox/pooled_demo.ml:177:38`: lemma call `Forest_transport.unfolding_root`
- `testsuite/tests/vox/pooled_demo.ml:216:54`: lemma call `model`
- `testsuite/tests/vox/pooled_demo.ml:217:4`: lemma call `Level_finite_proofs.with_finite_model`
- `testsuite/tests/vox/pooled_demo.ml:229:9`: lemma call `coverage6`
- `testsuite/tests/vox/pooled_proofs.ml:31:74`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/pooled_proofs.ml:37:96`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/pooled_proofs.ml:48:113`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/pooled_proofs.ml:51:113`: lemma call `Copy_spec.session_mark_def`

### online-union-find (75 lemma calls)

- `verification/library/vox_ackermann.ml:109:2`: lemma call `bounds` (precise mode only)
- `verification/library/vox_ackermann.ml:115:4`: lemma call `minimum_def`
- `verification/library/vox_ackermann.ml:137:4`: lemma call `minimum_def` (precise mode only)
- `verification/library/vox_ackermann.ml:223:6`: lemma call `iter_def` (precise mode only)
- `verification/library/vox_ackermann.ml:227:6`: lemma call `minimum_def` (precise mode only)
- `verification/library/vox_ackermann.ml:265:4`: lemma call `minimum_def` (precise mode only)
- `verification/library/vox_connectivity.ml:61:6`: lemma call `contains_def`
- `verification/library/vox_connectivity.ml:63:6`: lemma call `root_def`
- `verification/library/vox_connectivity.ml:78:6`: lemma call `contains_def`
- `verification/library/vox_connectivity.ml:80:6`: lemma call `contains_def`
- `verification/library/vox_connectivity.ml:82:6`: lemma call `root_def`
- `verification/library/vox_connectivity.ml:84:6`: lemma call `root_def`
- `verification/library/vox_connectivity.ml:101:6`: lemma call `contains_def`
- `verification/library/vox_connectivity.ml:102:6`: lemma call `contains_def`
- `verification/library/vox_connectivity.ml:103:6`: lemma call `contains_def`
- `verification/library/vox_connectivity.ml:105:6`: lemma call `contains_def`
- `verification/library/vox_connectivity.ml:106:6`: lemma call `contains_def`
- `verification/library/vox_connectivity.ml:107:6`: lemma call `contains_def`
- `verification/library/vox_connectivity.ml:111:6`: lemma call `root_def`
- `verification/library/vox_connectivity.ml:113:6`: lemma call `root_def`
- `verification/library/vox_connectivity.ml:114:6`: lemma call `root_def`
- `verification/library/vox_connectivity.ml:115:6`: lemma call `root_def`
- `verification/library/vox_union_find.ml:138:4`: lemma call `F.lookup_valid`
- `verification/library/vox_union_find.ml:139:4`: lemma call `M.terminal`
- `verification/library/vox_union_find.ml:142:4`: lemma call `S.find_paths_def`
- `verification/library/vox_union_find.ml:173:12`: lemma call `W.heap_def`
- `verification/library/vox_union_find.ml:268:49`: lemma call `C.nonnegative`
- `verification/library/vox_union_find.ml:457:49`: lemma call `C.nonnegative`
- `verification/library/vox_union_find.ml:518:37`: lemma call `F.lookup_closed`
- `verification/library/vox_union_find.ml:531:39`: lemma call `state_observations`
- `verification/library/vox_union_find.ml:544:12`: lemma call `valid_def` (precise mode only)
- `verification/library/vox_union_find.ml:544:39`: lemma call `state_observations`
- `verification/library/vox_union_find_bank.ml:136:6`: lemma call `F.refresh_valid`
- `verification/library/vox_union_find_bank.ml:143:6`: lemma call `refresh_potential`
- `verification/library/vox_union_find_mass.ml:227:2`: lemma call `F.join_valid`
- `verification/library/vox_union_find_online.ml:215:4`: lemma call `F.representative_def`
- `verification/library/vox_union_find_online.ml:236:42`: lemma call `U.alpha_def`
- `verification/library/vox_union_find_online.ml:283:6`: lemma call `U.alpha_def`
- `verification/library/vox_union_find_online.ml:294:6`: lemma call `C.nonnegative` (precise mode only)
- `verification/library/vox_union_find_online.ml:311:12`: lemma call `C.nonnegative` (precise mode only)
- `verification/library/vox_union_find_online.ml:311:45`: lemma call `C.nonnegative` (precise mode only)
- `verification/library/vox_union_find_online.ml:319:12`: lemma call `C.nonnegative` (precise mode only)
- `verification/library/vox_union_find_online.ml:319:45`: lemma call `C.nonnegative`
- `verification/library/vox_union_find_online.ml:324:39`: lemma call `contents_def`
- `verification/library/vox_union_find_online.ml:325:32`: lemma call `size_def`
- `verification/library/vox_union_find_online.ml:369:12`: lemma call `C.nonnegative` (precise mode only)
- `verification/library/vox_union_find_online.ml:369:45`: lemma call `C.nonnegative`
- `verification/library/vox_union_find_online.ml:409:37`: lemma call `representative_def`
- `verification/library/vox_union_find_online.ml:410:6`: lemma call `U.representative_def`
- `verification/library/vox_union_find_online.ml:421:12`: lemma call `C.nonnegative` (precise mode only)
- `verification/library/vox_union_find_online.ml:421:45`: lemma call `C.nonnegative` (precise mode only)
- `verification/library/vox_union_find_online.ml:427:6`: lemma call `heap_def`
- `verification/library/vox_union_find_online.ml:444:31`: lemma call `size_def`
- `verification/library/vox_union_find_online.ml:444:57`: lemma call `contents_def`
- `verification/library/vox_union_find_online.ml:445:42`: lemma call `U.size_bounds` (precise mode only)
- `verification/library/vox_union_find_online.ml:446:4`: lemma call `U.alpha_def`
- `verification/library/vox_union_find_online.ml:446:39`: lemma call `U.capacity_def` (precise mode only)
- `verification/library/vox_union_find_online.ml:447:4`: lemma call `U.contents_def`
- `verification/library/vox_union_find_potential.ml:206:2`: lemma call `node_level_def`
- `verification/library/vox_union_find_potential.ml:208:2`: lemma call `node_index_def`
- `verification/library/vox_union_find_rank.ml:90:6`: lemma call `weight_def`
- `verification/library/vox_union_find_rank.ml:90:22`: lemma call `weight_def` (precise mode only)
- `verification/library/vox_union_find_simple.ml:44:42`: lemma call `U.alpha_def`
- `verification/library/vox_union_find_simple.ml:79:6`: lemma call `U.alpha_def`
- `verification/library/vox_union_find_simple.ml:86:45`: lemma call `C.nonnegative`
- `verification/library/vox_union_find_simple.ml:91:69`: lemma call `contents_def`
- `verification/library/vox_union_find_simple.ml:92:32`: lemma call `size_def`
- `verification/library/vox_union_find_simple.ml:111:6`: lemma call `heap_def`
- `verification/library/vox_union_find_simple.ml:124:45`: lemma call `C.nonnegative` (precise mode only)
- `verification/library/vox_union_find_simple.ml:130:6`: lemma call `heap_def`
- `verification/library/vox_union_find_simple.ml:150:37`: lemma call `representative_def`
- `verification/library/vox_union_find_simple.ml:151:6`: lemma call `U.representative_def`
- `verification/library/vox_union_find_simple.ml:160:45`: lemma call `C.nonnegative` (precise mode only)
- `verification/library/vox_union_find_simple.ml:166:6`: lemma call `heap_def`
- `verification/library/vox_union_find_spec.ml:66:33`: lemma call `F.lookup_closed`

### quicksort (5 lemma calls)

- `testsuite/tests/vox/quicksort_frame_client.ml:74:4`: lemma call `Borrow.Slice.finish`
- `testsuite/tests/vox/quicksort_iarray.ml:83:15`: lemma call `Spec.permutation_refl`
- `testsuite/tests/vox/quicksort_iarray_model.ml:70:2`: lemma call `Vox_iarray.slice_length`
- `testsuite/tests/vox/quicksort_iarray_model.ml:71:2`: lemma call `Vox_iarray.slice_length` (precise mode only)
- `testsuite/tests/vox/quicksort_model.ml:148:2`: lemma call `Vox_int_sequence.permutation_def`

### regex-automata (29 lemma calls)

- `testsuite/tests/vox/dfa_equivalence_proof.ml:1459:13`: lemma call `run_from_def`
- `testsuite/tests/vox/dfa_equivalence_proof.ml:1460:13`: lemma call `run_from_def`
- `testsuite/tests/vox/dfa_equivalence_proof.ml:1514:13`: lemma call `run_from_def`
- `testsuite/tests/vox/dfa_equivalence_proof.ml:1515:13`: lemma call `run_from_def`
- `testsuite/tests/vox/dfa_equivalence_proof.ml:2056:11`: lemma call `same_pair_correct`
- `testsuite/tests/vox/dfa_equivalence_proof.ml:2063:13`: lemma call `same_pair_correct`
- `testsuite/tests/vox/dfa_equivalence_proof.ml:2538:11`: lemma call `pending_singleton`
- `testsuite/tests/vox/dfa_equivalence_proof.ml:2543:11`: lemma call `pending_singleton`
- `testsuite/tests/vox/dfa_equivalence_proof.ml:4641:11`: lemma call `reached_valid` (precise mode only)
- `testsuite/tests/vox/dfa_equivalence_proof.ml:4777:15`: lemma call `state_search_view_def`
- `testsuite/tests/vox/dfa_equivalence_proof.ml:4903:23`: lemma call `state_search_valid_def`
- `testsuite/tests/vox/dfa_equivalence_proof.ml:4960:11`: lemma call `reached_valid`
- `testsuite/tests/vox/dfa_equivalence_proof.ml:5224:11`: lemma call `same_partition_signature_trans`
- `testsuite/tests/vox/dfa_equivalence_proof.ml:7557:11`: lemma call `same_class_pairs_nonnegative`
- `testsuite/tests/vox/dfa_equivalence_proof.ml:7728:15`: lemma call `initial_accepting_partition`
- `testsuite/tests/vox/dfa_equivalence_proof.ml:7905:15`: lemma call `state_search_valid_def`
- `testsuite/tests/vox/dfa_equivalence_proof.ml:7906:15`: lemma call `state_search_view_def`
- `testsuite/tests/vox/dfa_equivalence_proof.ml:7927:15`: lemma call `of_raw_def`
- `testsuite/tests/vox/dfa_equivalence_proof.ml:8131:15`: lemma call `pair_search_view_def`
- `testsuite/tests/vox/dfa_equivalence_proof.ml:8300:23`: lemma call `pair_search_valid_def`
- `testsuite/tests/vox/dfa_equivalence_proof.ml:8381:15`: lemma call `pair_search_valid_def`
- `testsuite/tests/vox/regex_core.ml:1337:11`: lemma call `equal_correct` (precise mode only)
- `testsuite/tests/vox/regex_core.ml:1338:11`: lemma call `equal_correct` (precise mode only)
- `testsuite/tests/vox/regex_dfa_bridge_core.ml:142:4`: lemma call `Regex.Dfa.same_state_correct`
- `testsuite/tests/vox/regex_dfa_bridge_core.ml:142:4`: lemma call `Regex_core.Regex.Dfa.same_state_correct`
- `testsuite/tests/vox/regex_dfa_bridge_core.ml:747:6`: lemma call `member_state_def`
- `testsuite/tests/vox/regex_dfa_bridge_core.ml:805:4`: lemma call `index_def`
- `testsuite/tests/vox/regex_dfa_bridge_core.ml:869:9`: lemma call `index_def`
- `testsuite/tests/vox/regex_dfa_bridge_core.ml:896:4`: lemma call `member_prefix`

### merge-sort (2 lemma calls)

- `verification/library/vox_sort_cost.ml:53:4`: lemma call `height_bound` (precise mode only)
- `verification/library/vox_sort_cost.ml:57:26`: lemma call `height_def`

### one-shot-channels (1 lemma call)

- `testsuite/tests/vox/channel_buffer_demo.ml:42:8`: lemma call `Channel_buffer.M.covers_get` (precise mode only)

### avl-sets (16 lemma calls)

- `testsuite/tests/vox/avl_sets.ml:890:12`: lemma call `height_nonnegative`
- `testsuite/tests/vox/avl_sets.ml:922:8`: lemma call `make_node_height` (precise mode only)
- `testsuite/tests/vox/avl_sets.ml:1165:8`: lemma call `height_nonnegative`
- `testsuite/tests/vox/avl_sets.ml:1166:8`: lemma call `height_nonnegative`
- `testsuite/tests/vox/avl_sets.ml:1167:8`: lemma call `height_nonnegative`
- `testsuite/tests/vox/avl_sets.ml:1168:8`: lemma call `height_nonnegative`
- `testsuite/tests/vox/avl_sets.ml:1237:8`: lemma call `height_nonnegative`
- `testsuite/tests/vox/avl_sets.ml:1238:8`: lemma call `height_nonnegative`
- `testsuite/tests/vox/avl_sets.ml:1239:8`: lemma call `height_nonnegative`
- `testsuite/tests/vox/avl_sets.ml:1240:8`: lemma call `height_nonnegative`
- `testsuite/tests/vox/avl_sets.ml:1244:8`: lemma call `all_greater_weaken`
- `testsuite/tests/vox/avl_sets.ml:1368:8`: lemma call `height_nonnegative`
- `testsuite/tests/vox/avl_sets.ml:1452:8`: lemma call `height_nonnegative`
- `testsuite/tests/vox/avl_sets.ml:2316:13`: lemma call `Element_proofs.elements_all_less`
- `testsuite/tests/vox/avl_sets.ml:2525:8`: lemma call `List_proofs.same_repr_equal`
- `testsuite/tests/vox/avl_sets.ml:2555:8`: lemma call `List_proofs.same_repr_equal`

### binary-search (4 lemma calls, 3 arguments)

- `testsuite/tests/vox/sorted_array_proofs.ml:17:10`: argument `index`
- `testsuite/tests/vox/sorted_array_proofs.ml:76:28`: argument `size`
- `testsuite/tests/vox/sorted_array_proofs.ml:199:4`: lemma call `range_spec_def` (precise mode only)
- `testsuite/tests/vox/sorted_array_proofs.ml:228:6`: lemma call `above_def` (precise mode only)
- `testsuite/tests/vox/sorted_array_proofs.ml:229:6`: lemma call `above_def` (precise mode only)
- `testsuite/tests/vox/sorted_array_proofs.ml:370:6`: lemma call `range_spec_def`
- `testsuite/tests/vox/sorted_arrays.ml:395:10`: argument `index`

### functional-queue (2 lemma calls)

- `testsuite/tests/vox/functional_queue.ml:44:10`: lemma call `reverse_append_correct`
- `testsuite/tests/vox/functional_queue.ml:44:45`: lemma call `Vox_sequence.append_nil`

### lists-trees (29 lemma calls)

- `testsuite/tests/vox/pref_list.ml:79:2`: lemma call `Vox_pref_semantics.put` (precise mode only)
- `testsuite/tests/vox/pref_list.ml:95:2`: lemma call `H.put_law` (precise mode only)
- `testsuite/tests/vox/pref_list.ml:219:6`: lemma call `H.union_law`
- `testsuite/tests/vox/pref_list.ml:221:6`: lemma call `Vox_pref_semantics.put` (precise mode only)
- `testsuite/tests/vox/pref_list.ml:222:6`: lemma call `Vox_pref_semantics.union`
- `testsuite/tests/vox/pref_list.ml:223:6`: lemma call `Vox_pref_semantics.union` (precise mode only)
- `testsuite/tests/vox/pref_list.ml:234:9`: lemma call `H.union_law` (precise mode only)
- `testsuite/tests/vox/pref_list.ml:307:11`: lemma call `H.union_law` (precise mode only)
- `testsuite/tests/vox/pref_list.ml:310:11`: lemma call `H.union_law` (precise mode only)
- `testsuite/tests/vox/pref_list_client.ml:51:22`: lemma call `Pref_list.H.partition_law` (precise mode only)
- `testsuite/tests/vox/pref_list_client.ml:77:22`: lemma call `Pref_list.H.partition_law` (precise mode only)
- `testsuite/tests/vox/pref_tree.ml:74:2`: lemma call `H.commute_law` (precise mode only)
- `testsuite/tests/vox/pref_tree.ml:75:2`: lemma call `H.put_law` (precise mode only)
- `testsuite/tests/vox/pref_tree.ml:76:2`: lemma call `H.put_law` (precise mode only)
- `testsuite/tests/vox/pref_tree.ml:112:2`: lemma call `Vox_pref_semantics.put`
- `testsuite/tests/vox/pref_tree.ml:113:2`: lemma call `Vox_pref_semantics.put` (precise mode only)
- `testsuite/tests/vox/pref_tree.ml:114:2`: lemma call `Vox_pref_semantics.put` (precise mode only)
- `testsuite/tests/vox/pref_tree.ml:368:4`: lemma call `H.union_law`
- `testsuite/tests/vox/pref_tree.ml:383:2`: lemma call `Vox_pref_semantics.put`
- `testsuite/tests/vox/pref_tree.ml:384:2`: lemma call `Vox_pref_semantics.put` (precise mode only)
- `testsuite/tests/vox/pref_tree.ml:386:2`: lemma call `Vox_pref_semantics.put` (precise mode only)
- `testsuite/tests/vox/pref_tree.ml:388:2`: lemma call `Vox_pref_semantics.union` (precise mode only)
- `testsuite/tests/vox/pref_tree.ml:389:2`: lemma call `Vox_pref_semantics.union` (precise mode only)
- `testsuite/tests/vox/pref_tree.ml:390:2`: lemma call `Vox_pref_semantics.union`
- `testsuite/tests/vox/pref_tree.ml:391:2`: lemma call `Vox_pref_semantics.union` (precise mode only)
- `testsuite/tests/vox/pref_tree.ml:427:9`: lemma call `H.union_law` (precise mode only)
- `testsuite/tests/vox/pref_tree.ml:480:11`: lemma call `H.union_law` (precise mode only)
- `testsuite/tests/vox/pref_tree.ml:483:33`: lemma call `H.union_law` (precise mode only)
- `testsuite/tests/vox/pref_tree_client.ml:63:13`: lemma call `Pref_tree.H.partition_law` (precise mode only)

### rings (137 lemma calls)

- `testsuite/tests/vox/pref_ring.ml:314:4`: lemma call `mem_put`
- `testsuite/tests/vox/pref_ring.ml:315:4`: lemma call `mem_put`
- `testsuite/tests/vox/pref_ring.ml:346:4`: lemma call `connected_mem` (precise mode only)
- `testsuite/tests/vox/pref_ring.ml:347:4`: lemma call `connected_mem` (precise mode only)
- `testsuite/tests/vox/pref_ring_general.ml:281:6`: lemma call `Pref_ring.present_def` (precise mode only)
- `testsuite/tests/vox/pref_ring_general_demo.ml:61:4`: lemma call `Pref_ring.apart_def`
- `testsuite/tests/vox/pref_ring_general_demo.ml:61:19`: lemma call `Pref_ring.apart_def`
- `testsuite/tests/vox/pref_ring_general_demo.ml:61:34`: lemma call `Pref_ring.apart_def`
- `testsuite/tests/vox/pref_ring_insert_remove.ml:67:19`: lemma call `Pref_ring.present_def`
- `testsuite/tests/vox/pref_ring_insert_remove.ml:69:19`: lemma call `Pref_ring.present_def` (precise mode only)
- `testsuite/tests/vox/pref_ring_insert_remove.ml:151:19`: lemma call `Pref_ring.present_def` (precise mode only)
- `testsuite/tests/vox/pref_ring_insert_remove.ml:237:19`: lemma call `Pref_ring.present_def` (precise mode only)
- `testsuite/tests/vox/pref_ring_public_client.ml:53:4`: lemma call `at_put` (precise mode only)
- `testsuite/tests/vox/pref_ring_public_client.ml:68:4`: lemma call `Vox_pref_semantics.put`
- `testsuite/tests/vox/pref_ring_public_client.ml:69:4`: lemma call `Vox_pref_semantics.put` (precise mode only)
- `testsuite/tests/vox/pref_ring_reverse_model.ml:71:20`: lemma call `Pref_ring.flipped_def`
- `testsuite/tests/vox/pref_ring_reverse_model.ml:95:20`: lemma call `Pref_ring.flipped_def`
- `testsuite/tests/vox/pref_ring_reverse_model.ml:107:20`: lemma call `Pref_ring.flipped_def`
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:96:25`: lemma call `Pref_ring_proofs.put_observations`
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:97:25`: lemma call `Pref_ring_proofs.put_observations`
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:98:25`: lemma call `Pref_ring_proofs.put_observations`
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:99:25`: lemma call `Pref_ring_proofs.put_observations`
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:103:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:104:25`: lemma call `Pref_ring_proofs.put_observations`
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:105:25`: lemma call `Pref_ring_proofs.put_observations`
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:106:25`: lemma call `Pref_ring_proofs.put_observations`
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:110:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:111:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:112:25`: lemma call `Pref_ring_proofs.put_observations`
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:113:25`: lemma call `Pref_ring_proofs.put_observations`
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:117:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:118:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:119:25`: lemma call `Pref_ring_proofs.put_observations`
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:120:25`: lemma call `Pref_ring_proofs.put_observations`
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:124:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:125:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:126:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:127:25`: lemma call `Pref_ring_proofs.put_observations`
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:131:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:132:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:133:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:134:25`: lemma call `Pref_ring_proofs.put_observations`
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:138:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:139:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:140:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:141:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:145:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:146:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:147:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_reverse_setup.ml:148:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice.ml:61:16`: lemma call `Pref_ring_splice_model.contents` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_fixture.ml:48:2`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_fixture.ml:49:2`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_general.ml:196:8`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_general.ml:254:2`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_general.ml:256:2`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_general.ml:258:2`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_general.ml:260:2`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_general.ml:396:2`: lemma call `apart_lists_member`
- `testsuite/tests/vox/pref_ring_splice_general.ml:397:2`: lemma call `safe_external`
- `testsuite/tests/vox/pref_ring_splice_general.ml:422:2`: lemma call `apart_lists_member`
- `testsuite/tests/vox/pref_ring_splice_general.ml:423:2`: lemma call `safe_external`
- `testsuite/tests/vox/pref_ring_splice_general.ml:624:4`: lemma call `different_chunks`
- `testsuite/tests/vox/pref_ring_splice_general.ml:625:4`: lemma call `different_chunks`
- `testsuite/tests/vox/pref_ring_splice_general.ml:627:4`: lemma call `different_chunks` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_general.ml:628:4`: lemma call `different_chunks`
- `testsuite/tests/vox/pref_ring_splice_general.ml:629:4`: lemma call `different_chunks`
- `testsuite/tests/vox/pref_ring_splice_general.ml:632:4`: lemma call `different_chunks`
- `testsuite/tests/vox/pref_ring_splice_general.ml:633:4`: lemma call `different_chunks`
- `testsuite/tests/vox/pref_ring_splice_general.ml:636:4`: lemma call `different_chunks` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_general.ml:637:4`: lemma call `different_chunks`
- `testsuite/tests/vox/pref_ring_splice_general.ml:638:4`: lemma call `different_chunks`
- `testsuite/tests/vox/pref_ring_splice_general.ml:641:4`: lemma call `different_chunks`
- `testsuite/tests/vox/pref_ring_splice_general.ml:642:4`: lemma call `different_chunks`
- `testsuite/tests/vox/pref_ring_splice_general.ml:644:4`: lemma call `different_chunks` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_general.ml:645:4`: lemma call `different_chunks`
- `testsuite/tests/vox/pref_ring_splice_general.ml:646:4`: lemma call `different_chunks`
- `testsuite/tests/vox/pref_ring_splice_general.ml:650:4`: lemma call `different_chunks` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_general.ml:651:4`: lemma call `different_chunks`
- `testsuite/tests/vox/pref_ring_splice_general.ml:652:4`: lemma call `different_chunks`
- `testsuite/tests/vox/pref_ring_splice_general.ml:655:4`: lemma call `different_chunks`
- `testsuite/tests/vox/pref_ring_splice_general.ml:656:4`: lemma call `different_chunks`
- `testsuite/tests/vox/pref_ring_splice_general.ml:658:4`: lemma call `different_chunks` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_general.ml:659:4`: lemma call `different_chunks`
- `testsuite/tests/vox/pref_ring_splice_general.ml:660:4`: lemma call `different_chunks`
- `testsuite/tests/vox/pref_ring_splice_general.ml:663:4`: lemma call `different_chunks`
- `testsuite/tests/vox/pref_ring_splice_general.ml:664:4`: lemma call `different_chunks` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_general.ml:665:4`: lemma call `different_chunks` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_general.ml:666:4`: lemma call `different_chunks`
- `testsuite/tests/vox/pref_ring_splice_model.ml:80:20`: lemma call `Pref_ring_proofs.put_observations`
- `testsuite/tests/vox/pref_ring_splice_model.ml:81:20`: lemma call `Pref_ring_proofs.put_observations`
- `testsuite/tests/vox/pref_ring_splice_model.ml:82:20`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_model.ml:83:20`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_model.ml:87:20`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_model.ml:88:20`: lemma call `Pref_ring_proofs.put_observations`
- `testsuite/tests/vox/pref_ring_splice_model.ml:89:20`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_model.ml:90:20`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_model.ml:94:20`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_model.ml:95:20`: lemma call `Pref_ring_proofs.put_observations`
- `testsuite/tests/vox/pref_ring_splice_model.ml:96:20`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_model.ml:97:20`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_model.ml:101:20`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_model.ml:102:20`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_model.ml:103:20`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_model.ml:104:20`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_model.ml:108:20`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_model.ml:109:20`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_model.ml:110:20`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_model.ml:111:20`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_model.ml:115:20`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_model.ml:116:20`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_model.ml:117:20`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_model.ml:118:20`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_setup.ml:92:25`: lemma call `Pref_ring_proofs.put_observations`
- `testsuite/tests/vox/pref_ring_splice_setup.ml:93:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_setup.ml:94:25`: lemma call `Pref_ring_proofs.put_observations`
- `testsuite/tests/vox/pref_ring_splice_setup.ml:95:25`: lemma call `Pref_ring_proofs.put_observations`
- `testsuite/tests/vox/pref_ring_splice_setup.ml:99:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_setup.ml:100:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_setup.ml:101:25`: lemma call `Pref_ring_proofs.put_observations`
- `testsuite/tests/vox/pref_ring_splice_setup.ml:102:25`: lemma call `Pref_ring_proofs.put_observations`
- `testsuite/tests/vox/pref_ring_splice_setup.ml:106:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_setup.ml:107:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_setup.ml:108:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_setup.ml:109:25`: lemma call `Pref_ring_proofs.put_observations`
- `testsuite/tests/vox/pref_ring_splice_setup.ml:113:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_setup.ml:114:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_setup.ml:115:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_setup.ml:116:25`: lemma call `Pref_ring_proofs.put_observations`
- `testsuite/tests/vox/pref_ring_splice_setup.ml:120:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_setup.ml:121:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_setup.ml:122:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_setup.ml:123:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_setup.ml:127:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_setup.ml:128:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_setup.ml:129:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)
- `testsuite/tests/vox/pref_ring_splice_setup.ml:130:25`: lemma call `Pref_ring_proofs.put_observations` (precise mode only)

### sparse-arrays (3 lemma calls)

- `testsuite/tests/vox/sparse_overlay_client.ml:94:11`: lemma call `Int_client.last_write_wins`
- `testsuite/tests/vox/sparse_overlay_client.ml:95:11`: lemma call `Int_client.clear_restores_base`
- `testsuite/tests/vox/sparse_overlay_client.ml:104:9`: lemma call `Item_client.independent_updates`

### constant-folding (7 lemma calls)

- `testsuite/tests/vox/clamp.ml:26:11`: lemma call `Clamp.identity`
- `testsuite/tests/vox/clamp.ml:27:11`: lemma call `Clamp.idempotent`
- `testsuite/tests/vox/int_lists.ml:24:9`: lemma call `Laws.append_nil_left`
- `testsuite/tests/vox/int_lists.ml:25:9`: lemma call `Laws.append_nil_right`
- `testsuite/tests/vox/int_lists.ml:26:9`: lemma call `Laws.append_associative`
- `testsuite/tests/vox/int_lists.ml:27:9`: lemma call `Laws.length_append`
- `testsuite/tests/vox/int_lists.ml:28:9`: lemma call `Laws.sum_append`

### reference-locks (3 lemma calls)

- `testsuite/tests/vox/unique_lock_buffer_client.ml:35:11`: lemma call `H.put_law` (precise mode only)
- `testsuite/tests/vox/unique_lock_buffer_client.ml:48:9`: lemma call `H.put_law`
- `verification/library/reference_lock.ml:130:12`: lemma call `location_def`

### sat-solver (17 lemma calls, 6 arguments)

- `verification/library/vox_cdcl_total_proof.ml:122:4`: lemma call `unassigned_bounds` (precise mode only)
- `verification/library/vox_cdcl_total_proof.ml:593:44`: lemma call `sequence_length_nonnegative`
- `verification/library/vox_cdcl_total_proof.ml:874:4`: lemma call `at_def`
- `verification/library/vox_cdcl_total_proof.ml:927:4`: lemma call `at_def`
- `verification/library/vox_cdcl_total_proof.ml:982:4`: lemma call `at_def`
- `verification/library/vox_cdcl_total_proof.ml:1220:4`: lemma call `trail_rank_bounds`
- `verification/library/vox_cdcl_total_proof.ml:1447:4`: lemma call `at_def`
- `verification/library/vox_cdcl_total_proof.ml:2163:6`: lemma call `ordered_weaken` (precise mode only)
- `verification/library/vox_cdcl_total_proof.ml:2404:4`: lemma call `trail_levels_bound`
- `verification/library/vox_cdcl_total_proof.ml:2904:8`: lemma call `ordered_weaken` (precise mode only)
- `verification/library/vox_cdcl_total_proof.ml:3098:6`: argument `n`
- `verification/library/vox_cdcl_total_proof.ml:3098:16`: argument `database`
- `verification/library/vox_cdcl_total_proof.ml:3149:6`: argument `n`
- `verification/library/vox_cdcl_total_proof.ml:3152:2`: lemma call `current_variables_covered`
- `verification/library/vox_cdcl_total_proof.ml:3274:10`: lemma call `clause_rank_nonnegative`
- `verification/library/vox_cdcl_total_proof.ml:3641:9`: lemma call `unassigned_bounds` (precise mode only)
- `verification/library/vox_cdcl_total_proof.ml:3928:6`: argument `n`
- `verification/library/vox_cdcl_total_proof.ml:4036:8`: argument `depth` (precise mode only)
- `verification/library/vox_cdcl_total_proof.ml:4095:4`: lemma call `Vox_sat_proof.same_clause_reflexive` (precise mode only)
- `verification/library/vox_cdcl_total_proof.ml:4201:6`: argument `n`
- `verification/library/vox_cdcl_total_proof.ml:4350:9`: lemma call `progress_nonnegative`
- `verification/library/vox_cdcl_total_proof.ml:4380:19`: lemma call `ordered_weaken`
- `verification/library/vox_cdcl_total_proof.ml:4415:10`: lemma call `Vox_sat_proof.result_clause_valid`

### egraphs (33 lemma calls, 2 arguments)

- `verification/library/vox_egraph_arena_spec.ml:36:2`: lemma call `I.updated_length`
- `verification/library/vox_egraph_index_spec.ml:180:6`: lemma call `K.symmetric`
- `verification/library/vox_egraph_key.ml:30:2`: lemma call `equal_def`
- `verification/library/vox_egraph_key.ml:30:32`: lemma call `equal_def`
- `verification/library/vox_egraph_key.ml:35:2`: lemma call `hash_def`
- `verification/library/vox_egraph_key.ml:36:2`: lemma call `hash_def`
- `verification/library/vox_egraph_match_bounded.ml:46:14`: lemma call `positive` (precise mode only)
- `verification/library/vox_egraph_match_bounded.ml:58:14`: lemma call `positive` (precise mode only)
- `verification/library/vox_egraph_match_scan.ml:21:8`: lemma call `O.labels_length`
- `verification/library/vox_egraph_match_scan.ml:144:13`: lemma call `O.observe_def`
- `verification/library/vox_egraph_match_scan.ml:145:13`: lemma call `O.labels_length`
- `verification/library/vox_egraph_match_scan.ml:152:14`: lemma call `O.observe_def`
- `verification/library/vox_egraph_match_scan.ml:160:14`: lemma call `O.observe_def` (precise mode only)
- `verification/library/vox_egraph_owner.ml:151:8`: lemma call `Memo.Spec.Map.same_get`
- `verification/library/vox_egraph_owner.ml:153:8`: lemma call `Memo.Spec.Map.put_get`
- `verification/library/vox_egraph_owner.ml:154:8`: lemma call `K.reflexive`
- `verification/library/vox_egraph_quantifier.ml:12:37`: lemma call `Measure.nonnegative`
- `verification/library/vox_egraph_rule_handle.ml:164:39`: lemma call `rules_def`
- `verification/library/vox_egraph_rule_handle.ml:173:12`: lemma call `model_def` (precise mode only)
- `verification/library/vox_egraph_rule_hashcons.ml:205:10`: lemma call `O.valid_def` (precise mode only)
- `verification/library/vox_egraph_rule_hashcons.ml:575:10`: lemma call `N.signature_def`
- `verification/library/vox_egraph_rule_hashcons.ml:576:10`: lemma call `N.signature_def`
- `verification/library/vox_egraph_rule_hashcons.ml:661:8`: lemma call `V.pair_closed_def`
- `verification/library/vox_egraph_rule_rewrite.ml:170:6`: lemma call `H.O.valid_def`
- `verification/library/vox_egraph_rule_scan.ml:49:6`: lemma call `H.O.valid_def`
- `verification/library/vox_egraph_rule_scan.ml:74:17`: lemma call `C.closed_roots_def`
- `verification/library/vox_egraph_rule_scan.ml:121:44`: lemma call `H.O.valid_def` (precise mode only)
- `verification/library/vox_egraph_rule_scan.ml:170:6`: lemma call `H.O.valid_def`
- `verification/library/vox_egraph_rule_union.ml:176:6`: lemma call `valid_def`
- `verification/library/vox_egraph_rule_union.ml:177:6`: lemma call `U.valid_def`
- `verification/library/vox_egraph_rule_union.ml:188:22`: argument `state`
- `verification/library/vox_egraph_rules_scan.ml:34:18`: argument `index` (precise mode only)
- `verification/library/vox_egraph_rules_scan.ml:36:6`: lemma call `Vox_egraph_rule_scan.H.O.valid_def`
- `verification/library/vox_egraph_union.ml:87:6`: lemma call `I.updated_read`
- `verification/library/vox_egraph_union_spec.ml:120:4`: lemma call `root_def` (precise mode only)

### dfa-equivalence (2 lemma calls)

- `testsuite/tests/vox/dfa_equivalence.ml:1906:9`: lemma call `Dfa_proof.reduction_preserves`
- `testsuite/tests/vox/dfa_equivalence.ml:1916:9`: lemma call `Dfa_proof.minimum_count`

### lz4 (24 lemma calls, 16 arguments)

- `verification/library/vox_lz4_buffer.ml:30:6`: lemma call `M.location_law` (precise mode only)
- `verification/library/vox_lz4_buffer.ml:83:11`: lemma call `H.union_law` (precise mode only)
- `verification/library/vox_lz4_buffer.ml:105:11`: lemma call `M.location_law`
- `verification/library/vox_lz4_decode_bytes_proof.ml:100:17`: argument `used` (precise mode only)
- `verification/library/vox_lz4_decode_bytes_roundtrip.ml:97:20`: argument `first` (precise mode only)
- `verification/library/vox_lz4_decode_bytes_roundtrip.ml:97:33`: argument `anchor` (precise mode only)
- `verification/library/vox_lz4_decode_bytes_roundtrip.ml:335:17`: argument `count`
- `verification/library/vox_lz4_encode_buffer.ml:30:6`: lemma call `M.location_law` (precise mode only)
- `verification/library/vox_lz4_encode_buffer.ml:83:11`: lemma call `H.union_law` (precise mode only)
- `verification/library/vox_lz4_encode_buffer.ml:105:11`: lemma call `M.location_law`
- `verification/library/vox_lz4_fast_hints_reference.ml:6:5`: argument `source` (precise mode only)
- `verification/library/vox_lz4_general_bridge.ml:208:6`: lemma call `C.encode_model_size`
- `verification/library/vox_lz4_general_bridge.ml:273:8`: lemma call `Vox_lz4_spec_wire.extra_count_def` (precise mode only)
- `verification/library/vox_lz4_general_bridge.ml:291:8`: lemma call `Vox_lz4_spec_wire.extra_count_def` (precise mode only)
- `verification/library/vox_lz4_general_bridge.ml:292:8`: lemma call `Vox_lz4_spec_wire.extra_count_def`
- `verification/library/vox_lz4_general_cost.ml:211:4`: lemma call `encoded_size_loose_bound` (precise mode only)
- `verification/library/vox_lz4_general_cost.ml:254:9`: lemma call `encoded_size_loose_bound` (precise mode only)
- `verification/library/vox_lz4_general_match.ml:32:24`: argument `count` (precise mode only)
- `verification/library/vox_lz4_general_match.ml:94:6`: lemma call `Vox_lz4_spec_bytes.source_at_def`
- `verification/library/vox_lz4_general_match.ml:117:24`: argument `used` (precise mode only)
- `verification/library/vox_lz4_general_plan.ml:29:19`: argument `endpoint`
- `verification/library/vox_lz4_general_wire.ml:149:6`: lemma call `Vox_lz4_spec_bytes.source_at_def`
- `verification/library/vox_lz4_model.ml:16:6`: argument `written` (precise mode only)
- `verification/library/vox_lz4_mutable_scan.ml:20:20`: argument `count` (precise mode only)
- `verification/library/vox_lz4_packed_encode.ml:145:6`: argument `match_code`
- `verification/library/vox_lz4_roundtrip.ml:21:13`: argument `index`
- `verification/library/vox_lz4_roundtrip.ml:36:4`: lemma call `iarray_get_same`
- `verification/library/vox_lz4_roundtrip.ml:65:22`: argument `count` (precise mode only)
- `verification/library/vox_lz4_roundtrip.ml:123:6`: lemma call `heap_put_at`
- `verification/library/vox_lz4_roundtrip.ml:175:8`: lemma call `heap_put_at`
- `verification/library/vox_lz4_roundtrip.ml:320:27`: argument `cursor` (precise mode only)
- `verification/library/vox_lz4_roundtrip.ml:449:4`: lemma call `literal_token_of_source_def`
- `verification/library/vox_lz4_roundtrip.ml:521:4`: lemma call `literal_token_fields` (precise mode only)
- `verification/library/vox_lz4_roundtrip.ml:563:4`: lemma call `literal_token_fields` (precise mode only)
- `verification/library/vox_lz4_roundtrip.ml:582:4`: lemma call `match_token_fields` (precise mode only)
- `verification/library/vox_lz4_roundtrip.ml:645:8`: lemma call `heap_put_at` (precise mode only)
- `verification/library/vox_lz4_roundtrip.ml:659:11`: argument `position` (precise mode only)
- `verification/library/vox_lz4_spec_parse.ml:45:20`: argument `initial`
- `verification/library/vox_lz4_streaming.ml:48:4`: lemma call `Vox_lz4_spec_wire.extra_count_def` (precise mode only)
- `verification/library/vox_lz4_streaming.ml:49:4`: lemma call `Vox_lz4_spec_wire.extra_count_def` (precise mode only)

### register-allocation (15 lemma calls)

- `testsuite/tests/vox/register_allocation.ml:1659:4`: lemma call `Register_allocation_spec.valid_reg_def`
- `testsuite/tests/vox/register_allocation.ml:1660:4`: lemma call `Register_allocation_spec.valid_reg_def`
- `testsuite/tests/vox/register_allocation.ml:1665:4`: lemma call `Register_allocation_spec.valid_reg_def`
- `testsuite/tests/vox/register_allocation.ml:1666:4`: lemma call `Register_allocation_spec.valid_reg_def`
- `testsuite/tests/vox/register_allocation.ml:1671:4`: lemma call `Register_allocation_spec.valid_reg_def`
- `testsuite/tests/vox/register_allocation.ml:1672:4`: lemma call `Register_allocation_spec.valid_reg_def`
- `testsuite/tests/vox/register_allocation.ml:1678:4`: lemma call `Register_allocation_spec.valid_reg_def`
- `testsuite/tests/vox/register_allocation.ml:1679:4`: lemma call `Register_allocation_spec.valid_reg_def`
- `testsuite/tests/vox/register_allocation.ml:1680:4`: lemma call `Register_allocation_spec.valid_reg_def`
- `testsuite/tests/vox/register_allocation.ml:2054:6`: lemma call `subset_member`
- `testsuite/tests/vox/register_allocation.ml:2111:4`: lemma call `subset_member`
- `testsuite/tests/vox/register_allocation.ml:2167:6`: lemma call `subset_member`
- `testsuite/tests/vox/register_allocation.ml:2682:4`: lemma call `Register_allocation_spec.valid_reg_def` (precise mode only)
- `testsuite/tests/vox/register_allocation.ml:2687:7`: lemma call `all_valid_reg_lookup`
- `testsuite/tests/vox/register_allocation.ml:2688:7`: lemma call `Register_allocation_spec.valid_reg_def`

### mode-solver (78 lemma calls, 15 arguments)

- `testsuite/tests/vox/mode_solver_atomic.ml:36:5`: argument `s`
- `testsuite/tests/vox/mode_solver_atomic.ml:36:7`: argument `c`
- `testsuite/tests/vox/mode_solver_atomic.ml:36:9`: argument `x`
- `testsuite/tests/vox/mode_solver_atomic.ml:81:9`: argument `x` (precise mode only)
- `testsuite/tests/vox/mode_solver_atomic.ml:126:7`: argument `x` (precise mode only)
- `testsuite/tests/vox/mode_solver_atomic.ml:132:9`: lemma call `forward_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_atomic.ml:142:5`: argument `s` (precise mode only)
- `testsuite/tests/vox/mode_solver_atomic.ml:143:9`: lemma call `valid_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_atomic.ml:185:5`: argument `s`
- `testsuite/tests/vox/mode_solver_atomic.ml:185:7`: argument `x`
- `testsuite/tests/vox/mode_solver_atomic.ml:200:9`: lemma call `raised_lower_def`
- `testsuite/tests/vox/mode_solver_atomic.ml:215:9`: lemma call `lowered_upper_def`
- `testsuite/tests/vox/mode_solver_atomic.ml:236:12`: lemma call `closure_greatest`
- `testsuite/tests/vox/mode_solver_atomic_store.ml:44:19`: lemma call `H.put_law` (precise mode only)
- `testsuite/tests/vox/mode_solver_atomic_store.ml:45:24`: lemma call `H.put_law` (precise mode only)
- `testsuite/tests/vox/mode_solver_binary_encoding.ml:35:9`: lemma call `decode_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_binary_encoding.ml:51:9`: lemma call `decode_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_conditional_cut.ml:61:9`: lemma call `Mode_solver_semantics.le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_conditional_cut.ml:62:9`: lemma call `Mode_solver_semantics.le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_conditional_cut.ml:63:9`: lemma call `Mode_solver_semantics.le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_conditional_cut.ml:140:9`: lemma call `Mode_solver_semantics.le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_conditional_cut.ml:145:9`: lemma call `Mode_solver_semantics.le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_conditional_cut.ml:146:9`: lemma call `Mode_solver_semantics.le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_conditional_cut.ml:147:9`: lemma call `Mode_solver_semantics.le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_cycle.ml:32:7`: argument `x`
- `testsuite/tests/vox/mode_solver_cycle.ml:32:9`: argument `y` (precise mode only)
- `testsuite/tests/vox/mode_solver_cycle.ml:67:9`: lemma call `cycle_model_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_cycle.ml:69:9`: lemma call `Mode_solver_incoming.graph_model_def`
- `testsuite/tests/vox/mode_solver_cycle.ml:70:9`: lemma call `Mode_solver_incoming.graph_model_def`
- `testsuite/tests/vox/mode_solver_cycle.ml:71:9`: lemma call `Mode_solver_atomic.forward_def`
- `testsuite/tests/vox/mode_solver_cycle.ml:72:9`: lemma call `Mode_solver_atomic.backward_def`
- `testsuite/tests/vox/mode_solver_cycle.ml:73:9`: lemma call `Mode_solver_atomic.backward_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_cycle.ml:74:9`: lemma call `Mode_solver_atomic.backward_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_cycle.ml:112:9`: lemma call `Mode_solver_atomic.backward_def`
- `testsuite/tests/vox/mode_solver_cycle.ml:130:9`: lemma call `cycle_closed_def`
- `testsuite/tests/vox/mode_solver_direct.ml:78:9`: lemma call `le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_direct.ml:79:9`: lemma call `le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_direct.ml:80:9`: lemma call `le_def`
- `testsuite/tests/vox/mode_solver_direct.ml:81:9`: lemma call `le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_direct.ml:98:9`: lemma call `le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_direct.ml:102:12`: lemma call `le_def`
- `testsuite/tests/vox/mode_solver_direct.ml:105:12`: lemma call `le_def`
- `testsuite/tests/vox/mode_solver_direct.ml:108:12`: lemma call `le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_direct.ml:111:12`: lemma call `le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_direct.ml:123:9`: lemma call `le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_direct.ml:125:9`: lemma call `le_def`
- `testsuite/tests/vox/mode_solver_direct.ml:126:9`: lemma call `le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_direct.ml:184:9`: lemma call `le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_direct.ml:185:9`: lemma call `le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_direct.ml:197:9`: lemma call `le_def`
- `testsuite/tests/vox/mode_solver_direct.ml:198:9`: lemma call `le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_direct.ml:199:9`: lemma call `le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_direct.ml:200:9`: lemma call `le_def`
- `testsuite/tests/vox/mode_solver_direct.ml:310:9`: lemma call `le_def`
- `testsuite/tests/vox/mode_solver_direct.ml:311:9`: lemma call `le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_direct.ml:312:9`: lemma call `le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_direct.ml:313:9`: lemma call `le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_direct.ml:314:9`: lemma call `le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_incoming.ml:38:7`: argument `x`
- `testsuite/tests/vox/mode_solver_incoming.ml:38:9`: argument `y`
- `testsuite/tests/vox/mode_solver_incoming.ml:83:5`: argument `g`
- `testsuite/tests/vox/mode_solver_incoming.ml:83:7`: argument `x`
- `testsuite/tests/vox/mode_solver_incoming.ml:83:9`: argument `y`
- `testsuite/tests/vox/mode_solver_incoming.ml:97:9`: lemma call `with_incoming_def`
- `testsuite/tests/vox/mode_solver_irreducible_bounds.ml:19:9`: lemma call `Mode_solver_semantics.le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_irreducible_bounds.ml:21:9`: lemma call `Mode_solver_semantics.le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_irreducible_bounds.ml:37:9`: lemma call `Mode_solver_semantics.le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_irreducible_bounds.ml:39:9`: lemma call `Mode_solver_semantics.le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_level_cut.ml:106:9`: lemma call `Mode_solver_semantics.le_def`
- `testsuite/tests/vox/mode_solver_level_cut.ml:155:9`: lemma call `greatest_extension_def`
- `testsuite/tests/vox/mode_solver_level_cut.ml:161:9`: lemma call `Mode_solver_semantics.le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_level_cut.ml:162:9`: lemma call `Mode_solver_semantics.le_def`
- `testsuite/tests/vox/mode_solver_level_cut.ml:163:9`: lemma call `Mode_solver_semantics.le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_level_cut.ml:164:9`: lemma call `Mode_solver_semantics.le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_level_cut.ml:165:9`: lemma call `Mode_solver_semantics.le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_level_cut.ml:166:9`: lemma call `Mode_solver_semantics.le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_level_cut.ml:167:9`: lemma call `Mode_solver_semantics.meet_def`
- `testsuite/tests/vox/mode_solver_qe.ml:86:9`: lemma call `for_rigid_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_residual_gap.ml:37:9`: lemma call `Mode_solver_semantics.le_def`
- `testsuite/tests/vox/mode_solver_residual_gap.ml:59:11`: lemma call `Mode_solver_semantics.le_def`
- `testsuite/tests/vox/mode_solver_residual_state.ml:465:9`: lemma call `models_def`
- `testsuite/tests/vox/mode_solver_residual_state.ml:467:9`: lemma call `assert_residual_def`
- `testsuite/tests/vox/mode_solver_residual_state.ml:490:9`: lemma call `Mode_solver_semantics.le_def`
- `testsuite/tests/vox/mode_solver_rigid_slice.ml:60:9`: lemma call `Mode_solver_semantics.le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_rigid_slice.ml:63:9`: lemma call `Mode_solver_semantics.le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_symbolic_projection.ml:47:9`: lemma call `Mode_solver_semantics.le_def`
- `testsuite/tests/vox/mode_solver_symbolic_projection.ml:73:9`: lemma call `Mode_solver_semantics.le_def`
- `testsuite/tests/vox/mode_solver_symbolic_projection.ml:74:9`: lemma call `Mode_solver_semantics.le_def`
- `testsuite/tests/vox/mode_solver_symbolic_projection.ml:75:9`: lemma call `Mode_solver_semantics.le_def`
- `testsuite/tests/vox/mode_solver_symbolic_projection.ml:76:9`: lemma call `Mode_solver_semantics.le_def`
- `testsuite/tests/vox/mode_solver_toy.ml:52:9`: lemma call `Mode_solver_qe.le_def` (precise mode only)
- `testsuite/tests/vox/mode_solver_zap_obstruction.ml:29:9`: lemma call `Mode_solver_semantics.le_def`
- `testsuite/tests/vox/mode_solver_zap_obstruction.ml:32:9`: lemma call `Mode_solver_semantics.le_def`

### rsa (10 lemma calls)

- `verification/library/vox_rsa.ml:36:15`: lemma call `Spec.prime_def` (precise mode only)
- `verification/library/vox_rsa.ml:36:28`: lemma call `Spec.lambda_def`
- `verification/library/vox_rsa.ml:58:2`: lemma call `Spec.prime_def`
- `verification/library/vox_rsa.ml:58:15`: lemma call `Spec.prime_def`
- `verification/library/vox_rsa.ml:71:9`: lemma call `Spec.prime_def`
- `verification/library/vox_rsa.ml:72:9`: lemma call `Spec.prime_def`
- `verification/library/vox_rsa.ml:123:15`: lemma call `Spec.prime_def` (precise mode only)
- `verification/library/vox_rsa_arithmetic.ml:108:2`: lemma call `remainder_unique` (precise mode only) (keep: without it, line 109 exceeds the resource limit)
- `verification/library/vox_rsa_number_theory.ml:82:6`: lemma call `Vox_rsa_arithmetic.remainder_unique` (keep: without it and the call at line 110, line 83 exceeds the resource limit)
- `verification/library/vox_rsa_number_theory.ml:110:4`: lemma call `Vox_rsa_arithmetic.remainder_unique` (keep: without it and the call at line 82, line 83 exceeds the resource limit)

### http (14 lemma calls)

- `verification/library/vox_http.ml:184:8`: lemma call `S.append_associative`
- `verification/library/vox_http.ml:185:8`: lemma call `S.append_nil`
- `verification/library/vox_http.ml:816:17`: lemma call `length_nonnegative`
- `verification/library/vox_http.ml:1360:26`: lemma call `Driver.model_def`
- `verification/library/vox_http.ml:1442:23`: lemma call `Driver.model_def`
- `verification/library/vox_http.ml:1443:24`: lemma call `Driver.model_def`
- `verification/library/vox_http.ml:1481:28`: lemma call `machine_of_def` (precise mode only)
- `verification/library/vox_http.ml:1482:2`: lemma call `Driver.model_def` (precise mode only)
- `verification/library/vox_http.ml:1492:2`: lemma call `machine_of_def`
- `verification/library/vox_http.ml:1492:24`: lemma call `Driver.model_def`
- `verification/library/vox_http.ml:1512:29`: lemma call `machine_of_def` (precise mode only)
- `verification/library/vox_http.ml:1513:2`: lemma call `Driver.model_def` (precise mode only)
- `verification/library/vox_http.ml:1531:29`: lemma call `machine_of_def` (precise mode only)
- `verification/library/vox_http.ml:1532:2`: lemma call `Driver.model_def` (precise mode only)

### myers-diff (10 lemma calls)

- `verification/library/vox_diff.ml:95:4`: lemma call `suffix_bounds`
- `verification/library/vox_diff.ml:224:4`: lemma call `Proof.size_nonnegative`
- `verification/library/vox_diff.ml:225:4`: lemma call `Proof.size_nonnegative`
- `verification/library/vox_diff.ml:250:9`: lemma call `Proof.size_nonnegative`
- `verification/library/vox_diff.ml:251:9`: lemma call `Proof.size_nonnegative` (precise mode only)
- `verification/library/vox_diff.ml:420:9`: lemma call `valid_def`
- `verification/library/vox_diff.ml:423:9`: lemma call `Proof.size_nonnegative` (precise mode only)
- `verification/library/vox_diff.ml:466:13`: lemma call `valid_def`
- `verification/library/vox_diff.ml:492:13`: lemma call `valid_def`
- `verification/library/vox_diff.ml:813:9`: lemma call `Proof.size_nonnegative`

### hm-wasm-compiler (211 lemma calls, 4 arguments)

- `testsuite/tests/vox/hmc_cfg_evaluate.ml:26:54`: lemma call `Height.reflexive`
- `testsuite/tests/vox/hmc_cfg_evaluate.ml:53:40`: lemma call `Height.grow`
- `testsuite/tests/vox/hmc_cfg_evaluate.ml:54:8`: lemma call `Height.reflexive`
- `testsuite/tests/vox/hmc_cfg_evaluate.ml:66:40`: lemma call `Height.grow`
- `testsuite/tests/vox/hmc_cfg_evaluate.ml:67:8`: lemma call `Height.reflexive`
- `testsuite/tests/vox/hmc_cfg_evaluate.ml:79:40`: lemma call `Height.grow`
- `testsuite/tests/vox/hmc_cfg_evaluate.ml:80:8`: lemma call `Height.reflexive`
- `testsuite/tests/vox/hmc_cfg_evaluate.ml:92:40`: lemma call `Height.grow`
- `testsuite/tests/vox/hmc_cfg_evaluate.ml:93:8`: lemma call `Height.reflexive`
- `testsuite/tests/vox/hmc_cfg_evaluate.ml:105:40`: lemma call `Height.grow`
- `testsuite/tests/vox/hmc_cfg_evaluate.ml:106:8`: lemma call `Height.reflexive`
- `testsuite/tests/vox/hmc_cfg_evaluate.ml:118:40`: lemma call `Height.grow`
- `testsuite/tests/vox/hmc_cfg_evaluate.ml:119:8`: lemma call `Height.reflexive`
- `testsuite/tests/vox/hmc_cfg_return.ml:24:39`: lemma call `Height.grow`
- `testsuite/tests/vox/hmc_cfg_return.ml:25:6`: lemma call `Height.reflexive`
- `testsuite/tests/vox/hmc_cfg_return.ml:46:40`: lemma call `Height.grow`
- `testsuite/tests/vox/hmc_cfg_return.ml:47:8`: lemma call `Height.reflexive`
- `testsuite/tests/vox/hmc_cfg_return.ml:58:40`: lemma call `Height.grow`
- `testsuite/tests/vox/hmc_cfg_return.ml:71:8`: lemma call `Height.reflexive`
- `testsuite/tests/vox/hmc_cfg_return.ml:83:8`: lemma call `Height.reflexive`
- `testsuite/tests/vox/hmc_cfg_return.ml:98:8`: lemma call `Height.reflexive`
- `testsuite/tests/vox/hmc_cfg_return.ml:111:8`: lemma call `Height.reflexive`
- `testsuite/tests/vox/hmc_cfg_return.ml:138:46`: lemma call `Height.grow`
- `testsuite/tests/vox/hmc_cfg_return.ml:139:8`: lemma call `Height.reflexive`
- `testsuite/tests/vox/hmc_cfg_return.ml:157:8`: lemma call `Height.reflexive`
- `testsuite/tests/vox/hmc_cfg_return.ml:169:8`: lemma call `Height.reflexive`
- `testsuite/tests/vox/hmc_cfg_return.ml:187:8`: lemma call `Height.reflexive`
- `testsuite/tests/vox/hmc_cfg_return.ml:198:42`: lemma call `Height.grow`
- `testsuite/tests/vox/hmc_cfg_return.ml:199:8`: lemma call `Height.reflexive`
- `testsuite/tests/vox/hmc_closure_demo.ml:45:9`: lemma call `B.safe`
- `testsuite/tests/vox/hmc_compilation_examples.ml:90:12`: lemma call `Hmc_linear_bounds.covers_def`
- `testsuite/tests/vox/hmc_failed_guard_blocks.ml:133:4`: lemma call `Model.status_local_def`
- `testsuite/tests/vox/hmc_failed_guard_calls.ml:129:26`: lemma call `Dispatch.pc_offset_def`
- `testsuite/tests/vox/hmc_frame_call_slices.ml:29:6`: lemma call `Seg.temporaries`
- `testsuite/tests/vox/hmc_frame_demo.ml:103:50`: lemma call `Hmc_tail_stack.constant_stack`
- `testsuite/tests/vox/hmc_grounding.ml:137:8`: lemma call `entry_def`
- `testsuite/tests/vox/hmc_heap_frame_demo.ml:34:4`: lemma call `M.decode_def`
- `testsuite/tests/vox/hmc_manifest_demo.ml:65:49`: lemma call `Hmc_monomorphic_typing.program_typed`
- `testsuite/tests/vox/hmc_memory_active_machine_demo.ml:116:24`: lemma call `Capacity.ordered` (precise mode only)
- `testsuite/tests/vox/hmc_memory_active_machine_demo.ml:121:18`: lemma call `Capacity.ordered`
- `testsuite/tests/vox/hmc_memory_layout.ml:40:39`: lemma call `Capacity.ordered`
- `testsuite/tests/vox/hmc_memory_stack_capacity.ml:77:16`: lemma call `ordered`
- `testsuite/tests/vox/hmc_memory_stack_machine_demo.ml:109:16`: lemma call `Capacity.ordered`
- `testsuite/tests/vox/hmc_memory_suffix.ml:12:2`: lemma call `Bounds.covers_def`
- `testsuite/tests/vox/hmc_monomorphic_links.ml:68:49`: lemma call `M.lookup_present` (precise mode only)
- `testsuite/tests/vox/hmc_monomorphic_step.ml:81:8`: lemma call `H.environment_valid_def`
- `testsuite/tests/vox/hmc_monomorphic_step.ml:81:43`: lemma call `H.source_environment_def`
- `testsuite/tests/vox/hmc_monomorphic_step.ml:98:8`: lemma call `H.environment_valid_def`
- `testsuite/tests/vox/hmc_monomorphic_step.ml:98:43`: lemma call `H.source_environment_def`
- `testsuite/tests/vox/hmc_monomorphic_step.ml:129:9`: lemma call `H.C.ready_def`
- `testsuite/tests/vox/hmc_monomorphic_step.ml:151:16`: lemma call `H.environment_valid_def`
- `testsuite/tests/vox/hmc_monomorphic_step.ml:152:10`: lemma call `H.source_environment_def`
- `testsuite/tests/vox/hmc_monomorphic_step.ml:159:10`: lemma call `H.environment_valid_def`
- `testsuite/tests/vox/hmc_monomorphic_step.ml:160:10`: lemma call `H.source_environment_def`
- `testsuite/tests/vox/hmc_monomorphic_step.ml:175:16`: lemma call `H.environment_valid_def`
- `testsuite/tests/vox/hmc_monomorphic_step.ml:176:10`: lemma call `H.source_environment_def`
- `testsuite/tests/vox/hmc_monomorphic_step.ml:217:8`: lemma call `H.environment_valid_def`
- `testsuite/tests/vox/hmc_monomorphic_step.ml:217:43`: lemma call `H.source_environment_def`
- `testsuite/tests/vox/hmc_simulation_demo.ml:47:9`: lemma call `Hmc_monomorphic_safety.safe`
- `testsuite/tests/vox/hmc_simulation_demo.ml:51:11`: lemma call `P.normal_return_at_offsets`
- `testsuite/tests/vox/hmc_specialization.ml:27:11`: lemma call `Hmc_monomorphic_typing.program_typed`
- `testsuite/tests/vox/hmc_specialized_body_demo.ml:78:6`: lemma call `W.typing_action`
- `testsuite/tests/vox/hmc_tail_demo.ml:58:50`: lemma call `Hmc_tail_stack.constant_stack`
- `testsuite/tests/vox/hmc_wasm_allocation_advance.ml:20:43`: lemma call `S.add32_def`
- `testsuite/tests/vox/hmc_wasm_allocation_exit_fixture.ml:29:4`: lemma call `Continue.labels_def`
- `testsuite/tests/vox/hmc_wasm_allocation_exit_fixture.ml:31:104`: lemma call `T.stack_def`
- `testsuite/tests/vox/hmc_wasm_allocation_guard.ml:26:4`: lemma call `S.boolean_def`
- `testsuite/tests/vox/hmc_wasm_branch_invariant.ml:61:8`: lemma call `Lower.emit_def`
- `testsuite/tests/vox/hmc_wasm_call_capture_fixture.ml:88:16`: lemma call `Hmc_wasm_relayout_geometry.size_represents` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_call_captures.ml:30:14`: lemma call `Geometry.size_represents`
- `testsuite/tests/vox/hmc_wasm_call_continue_fixture.ml:202:52`: lemma call `Copy.position_def`
- `testsuite/tests/vox/hmc_wasm_call_dispatch_fixture.ml:136:52`: lemma call `Copy.position_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_call_dispatch_fixture.ml:137:16`: lemma call `Hmc_wasm_relayout_geometry.size_represents` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_call_dispatch_fixture.ml:149:21`: lemma call `Hmc_frame_call_decode.correct`
- `testsuite/tests/vox/hmc_wasm_call_entry.ml:74:6`: lemma call `Layout.width_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_call_frame_fixture.ml:99:52`: lemma call `Copy.position_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_call_frame_fixture.ml:100:16`: lemma call `Hmc_wasm_relayout_geometry.size_represents` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_call_frame_fixture.ml:108:21`: lemma call `Hmc_frame_call_decode.correct`
- `testsuite/tests/vox/hmc_wasm_call_operands.ml:55:12`: lemma call `Capture.separate_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_call_plan_fixture.ml:112:52`: lemma call `Copy.position_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_call_plan_fixture.ml:113:16`: lemma call `Hmc_wasm_relayout_geometry.size_represents` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_call_plan_fixture.ml:121:21`: lemma call `Hmc_frame_call_decode.correct`
- `testsuite/tests/vox/hmc_wasm_call_save_guard.ml:138:14`: lemma call `S.add32_def`
- `testsuite/tests/vox/hmc_wasm_call_save_memory.ml:54:6`: lemma call `Range.tag_def`
- `testsuite/tests/vox/hmc_wasm_call_target.ml:62:12`: lemma call `Capture.separate_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_closure_code_fixture.ml:73:21`: lemma call `Hmc_u32_index.unique`
- `testsuite/tests/vox/hmc_wasm_closure_result_memory.ml:47:34`: lemma call `U.zero_def`
- `testsuite/tests/vox/hmc_wasm_closure_result_memory.ml:47:49`: lemma call `Payload.offset_def`
- `testsuite/tests/vox/hmc_wasm_closure_write.ml:42:14`: lemma call `Geometry.size_represents` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_cons_allocate.ml:68:27`: lemma call `S.add32_def`
- `testsuite/tests/vox/hmc_wasm_cons_allocation_prefix.ml:73:6`: lemma call `S.add32_def`
- `testsuite/tests/vox/hmc_wasm_cons_locals.ml:33:31`: argument `premise`
- `testsuite/tests/vox/hmc_wasm_cons_locals.ml:41:29`: argument `premise`
- `testsuite/tests/vox/hmc_wasm_cons_locals.ml:58:22`: argument `premise`
- `testsuite/tests/vox/hmc_wasm_cons_memory.ml:60:6`: lemma call `Four.width_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_cons_result_memory.ml:47:34`: lemma call `U.zero_def`
- `testsuite/tests/vox/hmc_wasm_cons_result_memory.ml:47:49`: lemma call `Payload.offset_def`
- `testsuite/tests/vox/hmc_wasm_cross_range_words.ml:29:34`: lemma call `Range.payload_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_descriptor_address.ml:24:11`: lemma call `S.add32_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_descriptor_address.ml:29:11`: lemma call `S.add32_def`
- `testsuite/tests/vox/hmc_wasm_descriptor_address.ml:35:11`: lemma call `S.add32_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_descriptor_address.ml:41:11`: lemma call `S.add32_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_descriptor_address.ml:47:11`: lemma call `S.add32_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_descriptor_address.ml:51:12`: lemma call `S.add32_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_dispatch_fixture.ml:47:11`: lemma call `Wasm_control_codec.roundtrip`
- `testsuite/tests/vox/hmc_wasm_dispatch_loop_fixture.ml:65:11`: lemma call `Wasm_control_codec.roundtrip`
- `testsuite/tests/vox/hmc_wasm_dynamic_call_entry.ml:76:6`: lemma call `Layout.width_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_dynamic_call_frame_fixture.ml:102:52`: lemma call `Copy.position_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_dynamic_call_frame_fixture.ml:103:16`: lemma call `Hmc_wasm_relayout_geometry.size_represents` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_dynamic_call_frame_fixture.ml:111:21`: lemma call `Hmc_frame_call_decode.correct`
- `testsuite/tests/vox/hmc_wasm_frame_pad_finish.ml:37:44`: lemma call `Pad_memory.offset_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_header_update.ml:41:48`: lemma call `U.zero_def`
- `testsuite/tests/vox/hmc_wasm_jump_invariant.ml:57:8`: lemma call `Lower.emit_def`
- `testsuite/tests/vox/hmc_wasm_list_read_fixture.ml:46:14`: lemma call `Hmc_wasm_list_memory.zero_def`
- `testsuite/tests/vox/hmc_wasm_list_read_fixture.ml:46:48`: lemma call `Hmc_wasm_list_memory.eight_def`
- `testsuite/tests/vox/hmc_wasm_list_relayout.ml:43:14`: lemma call `Geometry.size_represents`
- `testsuite/tests/vox/hmc_wasm_list_relayout.ml:44:8`: lemma call `Geometry.size_represents`
- `testsuite/tests/vox/hmc_wasm_literal_invariant.ml:58:8`: lemma call `Lower.emit_def`
- `testsuite/tests/vox/hmc_wasm_loaded_call_fixture.ml:171:52`: lemma call `Copy.position_def`
- `testsuite/tests/vox/hmc_wasm_loaded_call_fixture.ml:172:16`: lemma call `Hmc_wasm_relayout_geometry.size_represents`
- `testsuite/tests/vox/hmc_wasm_loaded_call_fixture.ml:191:21`: lemma call `Hmc_frame_call_decode.correct`
- `testsuite/tests/vox/hmc_wasm_ordinary_call_fixture.ml:202:52`: lemma call `Copy.position_def`
- `testsuite/tests/vox/hmc_wasm_primitive_step_fixture.ml:63:13`: lemma call `Hmc_frame_decode_unique.frame`
- `testsuite/tests/vox/hmc_wasm_primitive_update.ml:49:34`: lemma call `U.zero_def`
- `testsuite/tests/vox/hmc_wasm_program_call.ml:274:44`: lemma call `Status.zero_def`
- `testsuite/tests/vox/hmc_wasm_program_call_fixture.ml:320:32`: lemma call `Program.valid_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_program_call_fixture.ml:342:60`: lemma call `Hmc_wasm_loaded_call.separate_def`
- `testsuite/tests/vox/hmc_wasm_program_call_fixture.ml:343:60`: lemma call `Hmc_wasm_loaded_call.separate_def`
- `testsuite/tests/vox/hmc_wasm_program_call_fixture.ml:344:60`: lemma call `Hmc_wasm_loaded_call.separate_def`
- `testsuite/tests/vox/hmc_wasm_program_caller_fixture.ml:163:26`: lemma call `Hmc_memory_saved_frame.slots_def`
- `testsuite/tests/vox/hmc_wasm_program_closure.ml:120:36`: lemma call `Status.zero_def`
- `testsuite/tests/vox/hmc_wasm_program_closure_fixture.ml:80:13`: lemma call `Pad.length`
- `testsuite/tests/vox/hmc_wasm_program_cons.ml:152:36`: lemma call `Status.zero_def`
- `testsuite/tests/vox/hmc_wasm_program_cons_fixture.ml:75:13`: lemma call `Pad.length`
- `testsuite/tests/vox/hmc_wasm_program_cons_fixture.ml:122:16`: lemma call `Wasm_control_branch_continue.labels_def`
- `testsuite/tests/vox/hmc_wasm_program_descriptors.ml:58:6`: lemma call `Bounds.covers_def`
- `testsuite/tests/vox/hmc_wasm_program_dispatch.ml:58:4`: lemma call `point_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_program_dispatch.ml:120:4`: lemma call `point_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_program_dispatch.ml:162:49`: lemma call `Runtime.prologue_code_def`
- `testsuite/tests/vox/hmc_wasm_program_dispatch.ml:249:27`: lemma call `frame_global_def`
- `testsuite/tests/vox/hmc_wasm_program_dispatch_fixture.ml:101:14`: lemma call `Dispatch.enter`
- `testsuite/tests/vox/hmc_wasm_program_global_fixture.ml:96:26`: lemma call `New.failure_def`
- `testsuite/tests/vox/hmc_wasm_program_initialize.ml:98:18`: lemma call `Capacity.ordered`
- `testsuite/tests/vox/hmc_wasm_program_input.ml:224:4`: lemma call `Invariant.valid_def`
- `testsuite/tests/vox/hmc_wasm_program_list_fixture.ml:85:13`: lemma call `Pad.length`
- `testsuite/tests/vox/hmc_wasm_program_list_fixture.ml:136:50`: lemma call `Capture.separate_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_program_register_fixture.ml:30:9`: lemma call `Registers.local_values`
- `testsuite/tests/vox/hmc_wasm_program_run.ml:41:14`: lemma call `Entry.start`
- `testsuite/tests/vox/hmc_wasm_program_run.ml:64:14`: lemma call `Entry.start`
- `testsuite/tests/vox/hmc_wasm_program_source_call.ml:480:6`: lemma call `Descriptors.valid_def`
- `testsuite/tests/vox/hmc_wasm_program_source_closure.ml:144:99`: lemma call `New.failure_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_program_source_cons.ml:181:99`: lemma call `New.failure_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_program_source_cons.ml:283:6`: lemma call `Hmc_wasm_program_frame.valid_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_program_source_cons.ml:286:8`: lemma call `Hmc_wasm_program_resources.valid_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_program_state_entry.ml:30:44`: argument `premise`
- `testsuite/tests/vox/hmc_wasm_program_state_fixture.ml:72:25`: lemma call `Hmc_memory_stack_capacity.ordered` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_program_state_fixture.ml:94:14`: lemma call `Bounds.covers_def`
- `testsuite/tests/vox/hmc_wasm_program_state_fixture.ml:102:50`: lemma call `Bounds.covers_def`
- `testsuite/tests/vox/hmc_wasm_program_state_fixture.ml:391:20`: lemma call `State.configuration_def`
- `testsuite/tests/vox/hmc_wasm_program_state_fixture.ml:411:20`: lemma call `State.configuration_def`
- `testsuite/tests/vox/hmc_wasm_program_state_fixture.ml:502:6`: lemma call `Wasm_scalar.add32_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_program_state_fixture.ml:596:19`: lemma call `State.configuration_def`
- `testsuite/tests/vox/hmc_wasm_program_state_fixture.ml:607:19`: lemma call `State.configuration_def`
- `testsuite/tests/vox/hmc_wasm_program_state_fixture.ml:688:13`: lemma call `Hmc_compiler.correct_def`
- `testsuite/tests/vox/hmc_wasm_program_state_fixture.ml:702:21`: lemma call `Hmc_wasm_program_static.dispatcher_typed`
- `testsuite/tests/vox/hmc_wasm_program_state_fixture.ml:713:23`: lemma call `Hmc_compiler.static_validity` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_program_status.ml:122:46`: lemma call `zero_def`
- `testsuite/tests/vox/hmc_wasm_program_status_fixture.ml:39:61`: lemma call `T.stack_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_program_step_call.ml:186:98`: lemma call `State.loop_def`
- `testsuite/tests/vox/hmc_wasm_program_step_closure.ml:81:6`: lemma call `Lower.corresponds_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_program_step_closure.ml:82:12`: lemma call `Hmc_heap_invariant.valid_def`
- `testsuite/tests/vox/hmc_wasm_program_step_closure.ml:96:100`: lemma call `State.loop_def`
- `testsuite/tests/vox/hmc_wasm_program_step_cons.ml:86:6`: lemma call `Lower.corresponds_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_program_step_cons.ml:87:12`: lemma call `Hmc_heap_invariant.valid_def`
- `testsuite/tests/vox/hmc_wasm_program_step_cons.ml:117:104`: lemma call `State.loop_def`
- `testsuite/tests/vox/hmc_wasm_program_step_cons.ml:126:20`: lemma call `Frame.valid_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_program_step_list.ml:89:14`: lemma call `Frame.valid_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_program_step_primitive.ml:82:14`: lemma call `Frame.valid_def`
- `testsuite/tests/vox/hmc_wasm_program_step_save_environment.ml:63:12`: lemma call `Hmc_heap_invariant.valid_def`
- `testsuite/tests/vox/hmc_wasm_program_step_save_environment.ml:82:12`: lemma call `Frame.valid_def`
- `testsuite/tests/vox/hmc_wasm_program_step_save_environment.ml:85:6`: lemma call `Frame.valid_def`
- `testsuite/tests/vox/hmc_wasm_program_step_saved_environment.ml:64:12`: lemma call `Hmc_heap_invariant.valid_def`
- `testsuite/tests/vox/hmc_wasm_program_step_saved_environment.ml:84:12`: lemma call `Frame.valid_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_program_tail_fixture.ml:103:13`: lemma call `Hmc_wasm_frame_padding.length` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_program_tail_fixture.ml:204:72`: lemma call `Copy.position_def`
- `testsuite/tests/vox/hmc_wasm_program_tail_fixture.ml:205:16`: lemma call `Hmc_wasm_relayout_geometry.size_represents` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_program_tail_fixture.ml:244:21`: lemma call `Hmc_frame_call_decode.correct`
- `testsuite/tests/vox/hmc_wasm_reached_cons_fixture.ml:89:16`: lemma call `Hmc_wasm_relayout_geometry.size_represents` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_relayout_fixture.ml:55:4`: lemma call `Hmc_frame_decode_unique.frame`
- `testsuite/tests/vox/hmc_wasm_relayout_fixture.ml:69:40`: lemma call `Wasm_sequence_update.zero_def`
- `testsuite/tests/vox/hmc_wasm_relayout_fixture.ml:69:74`: lemma call `Hmc_wasm_pc_update.offset_def`
- `testsuite/tests/vox/hmc_wasm_return_frame.ml:64:12`: lemma call `S.sub32_def`
- `testsuite/tests/vox/hmc_wasm_stack_guard_fixture.ml:49:4`: lemma call `Continue.labels_def`
- `testsuite/tests/vox/hmc_wasm_stack_guard_fixture.ml:51:104`: lemma call `T.stack_def`
- `testsuite/tests/vox/hmc_wasm_stack_push.ml:30:12`: lemma call `S.add32_def`
- `testsuite/tests/vox/hmc_wasm_stack_restore_fixture.ml:109:24`: lemma call `S.sub32_def` (precise mode only)
- `testsuite/tests/vox/hmc_wasm_stack_retreat.ml:20:43`: lemma call `S.sub32_def`
- `testsuite/tests/vox/hmc_wasm_static_block.ml:20:47`: lemma call `Step.take_push`
- `testsuite/tests/vox/hmc_wasm_value_pop.ml:56:14`: lemma call `Geometry.size_represents`
- `testsuite/tests/vox/hmc_wasm_value_pop.ml:57:8`: lemma call `Geometry.size_represents`
- `testsuite/tests/vox/hmc_wasm_value_pop.ml:58:8`: lemma call `Geometry.size_represents`
- `testsuite/tests/vox/wasm_calls_demo.ml:39:11`: lemma call `Budget.run`
- `testsuite/tests/vox/wasm_cell_suffix.ml:12:22`: lemma call `sixteen_def`
- `testsuite/tests/vox/wasm_control_demo.ml:23:12`: lemma call `N.flatten_correct`
- `testsuite/tests/vox/wasm_control_demo.ml:23:55`: lemma call `Wasm_control_codec.roundtrip`
- `testsuite/tests/vox/wasm_local_double.ml:21:43`: lemma call `S.add32_def`
- `testsuite/tests/vox/wasm_local_probe.ml:18:31`: lemma call `Hmc_wasm_pc_update.offset_def`
- `testsuite/tests/vox/wasm_local_probe.ml:18:65`: lemma call `Header.number_def`
- `testsuite/tests/vox/wasm_local_probe.ml:18:87`: lemma call `Header.number_def`
- `testsuite/tests/vox/wasm_local_probe.ml:18:112`: lemma call `W.equal_def`
- `testsuite/tests/vox/wasm_locals_demo.ml:30:15`: lemma call `L.other_local`
- `testsuite/tests/vox/wasm_shallow_calls.ml:164:10`: lemma call `M.with_stack_def`
- `testsuite/tests/vox/wasm_static_fixture.ml:124:8`: lemma call `R.load`
- `testsuite/tests/vox/wasm_static_fixture.ml:124:43`: lemma call `R.store`
- `testsuite/tests/vox/wasm_word_transport.ml:64:15`: lemma call `W.equal_def` (precise mode only)

### Other tests (not a catalogue demo) (366 lemma calls, 14 `assume_`, 10 arguments)

- `testsuite/tests/vox/assume_runtime.ml:19:9`: `assume_`
- `testsuite/tests/vox/atomic_operations.ml:77:9`: lemma call `Invariant.holds_def`
- `testsuite/tests/vox/atomic_operations.ml:78:9`: lemma call `Invariant.holds_def`
- `testsuite/tests/vox/avl_set_client.ml:18:4`: lemma call `Avl_sets.lookup_empty`
- `testsuite/tests/vox/avl_set_client.ml:19:4`: lemma call `Avl_sets.lookup_add`
- `testsuite/tests/vox/avl_set_client.ml:20:4`: lemma call `Avl_sets.lookup_union`
- `testsuite/tests/vox/avl_set_client.ml:21:4`: lemma call `Avl_sets.size_zero`
- `testsuite/tests/vox/avl_set_client.ml:22:4`: lemma call `Avl_sets.equal_lookup`
- `testsuite/tests/vox/avl_set_client.ml:31:4`: lemma call `Avl_sets.extensional`
- `testsuite/tests/vox/big_time_credits.ml:45:9`: lemma call `C.nonnegative`
- `testsuite/tests/vox/big_time_credits.ml:67:10`: lemma call `roundtrip`
- `testsuite/tests/vox/bigints.ml:185:25`: argument `bound`
- `testsuite/tests/vox/borrow_parallel.ml:31:2`: lemma call `Borrow.Slice.finish`
- `testsuite/tests/vox/borrow_parallel.ml:68:4`: lemma call `Borrow.Slice.finish`
- `testsuite/tests/vox/borrow_ranges.ml:48:9`: lemma call `Borrow.Model.set_length`
- `testsuite/tests/vox/clean_pooled_copy.ml:33:15`: lemma call `Copy_heap_proofs.put_frame` (precise mode only)
- `testsuite/tests/vox/collections_boundary_client.ml:62:9`: lemma call `set_commutes`
- `testsuite/tests/vox/compression_demo.ml:28:35`: lemma call `active_all`
- `testsuite/tests/vox/compression_origin_demo.ml:20:4`: lemma call `Copy_spec.cell_def`
- `testsuite/tests/vox/compression_origin_demo.ml:37:57`: lemma call `Copy_spec.cell_def`
- `testsuite/tests/vox/compression_origin_demo.ml:61:4`: lemma call `Compression_finite_proofs.compress_readback`
- `testsuite/tests/vox/compression_proofs.ml:74:6`: lemma call `order`
- `testsuite/tests/vox/connectivity.ml:61:4`: lemma call `U.observations`
- `testsuite/tests/vox/connectivity.ml:61:43`: lemma call `fee_bounds`
- `testsuite/tests/vox/connectivity.ml:152:10`: lemma call `budget_def`
- `testsuite/tests/vox/connectivity.ml:152:38`: lemma call `available_def`
- `testsuite/tests/vox/connectivity.ml:170:4`: lemma call `U.added_law`
- `testsuite/tests/vox/connectivity.ml:181:4`: lemma call `U.added_law`
- `testsuite/tests/vox/connectivity.ml:193:4`: lemma call `U.added_law`
- `testsuite/tests/vox/connectivity.ml:206:4`: lemma call `U.added_law`
- `testsuite/tests/vox/connectivity.ml:251:4`: lemma call `U.joined_law`
- `testsuite/tests/vox/connectivity.ml:267:4`: lemma call `U.joined_law`
- `testsuite/tests/vox/connectivity.ml:268:4`: lemma call `U.joined_law`
- `testsuite/tests/vox/connectivity.ml:311:10`: lemma call `budget_def`
- `testsuite/tests/vox/connectivity.ml:311:38`: lemma call `available_def`
- `testsuite/tests/vox/connectivity.ml:332:4`: lemma call `C.nonnegative` (precise mode only)
- `testsuite/tests/vox/deferred_arguments.ml:288:33`: `assume_`
- `testsuite/tests/vox/deferred_arguments.ml:376:8`: argument `premise`
- `testsuite/tests/vox/effective_compressed_representative.ml:48:12`: lemma call `this function`
- `testsuite/tests/vox/effective_compressed_representative.ml:49:146`: lemma call `Copy_heap_proofs.put_frame`
- `testsuite/tests/vox/effective_compression_demo.ml:32:36`: lemma call `this function` (precise mode only)
- `testsuite/tests/vox/effective_compression_proofs.ml:52:4`: lemma call `scope` (precise mode only)
- `testsuite/tests/vox/effective_copy_complete.ml:42:93`: argument `_fit`
- `testsuite/tests/vox/effective_copy_complete.ml:62:6`: lemma call `scope` (precise mode only)
- `testsuite/tests/vox/effective_copy_complete.ml:98:6`: lemma call `scope` (precise mode only)
- `testsuite/tests/vox/effective_copy_demo.ml:133:4`: lemma call `Effective_copy_order.sweep_ordered`
- `testsuite/tests/vox/effective_copy_demo.ml:176:31`: lemma call `Copy_heap_proofs.put_frame`
- `testsuite/tests/vox/effective_copy_demo.ml:229:8`: lemma call `Copy_heap_proofs.put_frame` (precise mode only)
- `testsuite/tests/vox/effective_copy_demo.ml:233:8`: lemma call `Copy_heap_proofs.put_frame` (precise mode only)
- `testsuite/tests/vox/effective_copy_finite.ml:28:31`: lemma call `Copy_spec.cell_def`
- `testsuite/tests/vox/effective_copy_heap_proofs.ml:24:46`: lemma call `Copy_spec.cell_def`
- `testsuite/tests/vox/effective_copy_heap_proofs.ml:51:71`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/effective_copy_heap_proofs.ml:57:86`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/effective_copy_heap_proofs.ml:215:93`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/effective_copy_heap_proofs.ml:219:86`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/effective_copy_heap_proofs.ml:237:137`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/effective_copy_heap_proofs.ml:239:86`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/effective_copy_metadata.ml:19:135`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/effective_copy_metadata.ml:21:84`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/effective_copy_pool.ml:28:74`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/effective_copy_pool.ml:34:96`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/effective_copy_pool.ml:46:113`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/effective_copy_pool.ml:49:113`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/effective_copy_runtime.ml:55:70`: lemma call `Copy_heap_proofs.put_frame`
- `testsuite/tests/vox/effective_copy_sound.ml:38:76`: lemma call `Copy_spec.session_mark_def`
- `testsuite/tests/vox/effective_level.ml:129:4`: lemma call `Representative_level.representative_covered_def`
- `testsuite/tests/vox/effective_level.ml:130:4`: lemma call `Representative_level.representatives_member`
- `testsuite/tests/vox/effective_lower_demo.ml:67:36`: lemma call `values` (precise mode only)
- `testsuite/tests/vox/effective_lower_proofs.ml:132:4`: lemma call `frame` (precise mode only)
- `testsuite/tests/vox/effective_lower_runtime.ml:94:91`: lemma call `this function`
- `testsuite/tests/vox/effective_lower_runtime.ml:133:8`: lemma call `this function`
- `testsuite/tests/vox/effective_lower_runtime.ml:216:14`: lemma call `this function`
- `testsuite/tests/vox/effective_lower_shared_demo.ml:71:18`: lemma call `Copy_heap_proofs.put_frame` (precise mode only)
- `testsuite/tests/vox/effective_lower_shared_demo.ml:74:15`: lemma call `Copy_spec.cell_def`
- `testsuite/tests/vox/effective_lower_write.ml:27:32`: lemma call `Copy_heap_proofs.put_frame` (precise mode only)
- `testsuite/tests/vox/effective_template.ml:49:4`: lemma call `generic_def`
- `testsuite/tests/vox/effective_unifier_datatype_demo.ml:49:61`: lemma call `active_all`
- `testsuite/tests/vox/effective_unifier_datatype_demo.ml:49:76`: lemma call `active_all`
- `testsuite/tests/vox/effective_unifier_datatype_demo.ml:107:27`: lemma call `effective_active`
- `testsuite/tests/vox/effective_unifier_datatype_demo.ml:107:48`: lemma call `effective_active`
- `testsuite/tests/vox/effective_unifier_datatype_demo.ml:111:55`: lemma call `here_def`
- `testsuite/tests/vox/effective_unifier_datatype_demo.ml:111:68`: lemma call `here_def` (precise mode only)
- `testsuite/tests/vox/effective_unifier_datatype_demo.ml:113:42`: lemma call `E.level_def`
- `testsuite/tests/vox/effective_unifier_datatype_demo.ml:113:78`: lemma call `E.level_def` (precise mode only)
- `testsuite/tests/vox/effective_unifier_demo.ml:143:71`: lemma call `Copy_spec.cell_def`
- `testsuite/tests/vox/effective_unifier_demo.ml:165:113`: lemma call `Copy_spec.cell_def`
- `testsuite/tests/vox/effective_unifier_model.ml:165:10`: lemma call `model`
- `testsuite/tests/vox/effective_unifier_runtime.ml:87:44`: lemma call `final_fn_def`
- `testsuite/tests/vox/effective_unifier_runtime.ml:129:44`: lemma call `mid_fn_def`
- `testsuite/tests/vox/effective_unifier_runtime.ml:156:44`: lemma call `final_fn_def`
- `testsuite/tests/vox/effective_unifier_runtime.ml:203:36`: lemma call `one_fn_def`
- `testsuite/tests/vox/effective_unifier_runtime.ml:226:36`: lemma call `two_fn_def`
- `testsuite/tests/vox/effective_unifier_runtime.ml:239:31`: lemma call `this function`
- `testsuite/tests/vox/effective_unifier_runtime.ml:239:77`: lemma call `this function`
- `testsuite/tests/vox/effective_unifier_runtime.ml:270:6`: lemma call `Compression_path_proofs.resolution_terminal`
- `testsuite/tests/vox/effective_unifier_shared_demo.ml:38:32`: lemma call `active_all` (precise mode only)
- `testsuite/tests/vox/effective_unifier_shared_demo.ml:98:4`: lemma call `Copy_spec.cell_def`
- `testsuite/tests/vox/effective_unifier_shared_demo.ml:98:19`: lemma call `Copy_spec.cell_def`
- `testsuite/tests/vox/effective_unifier_shared_demo.ml:148:20`: lemma call `solution`
- `testsuite/tests/vox/effective_unifier_shared_demo.ml:148:35`: lemma call `solution`
- `testsuite/tests/vox/egraph_derivation.ml:26:2`: lemma call `EP.sort_sound`
- `testsuite/tests/vox/egraph_derivation.ml:29:2`: lemma call `EP.sort_sound`
- `testsuite/tests/vox/egraph_match_observation.ml:25:2`: lemma call `M.root_def`
- `testsuite/tests/vox/egraph_rule_union.ml:37:4`: lemma call `G.valid_def`
- `testsuite/tests/vox/egraph_rule_union.ml:38:4`: lemma call `G.valid_def`
- `testsuite/tests/vox/equality.ml:156:9`: `assume_`
- `testsuite/tests/vox/equality.ml:165:9`: `assume_`
- `testsuite/tests/vox/equality.ml:180:11`: `assume_`
- `testsuite/tests/vox/equality.ml:181:11`: `assume_`
- `testsuite/tests/vox/equality.ml:182:9`: `assume_`
- `testsuite/tests/vox/equality.ml:200:9`: `assume_`
- `testsuite/tests/vox/equality.ml:210:9`: `assume_`
- `testsuite/tests/vox/fast_environment.ml:89:68`: argument `f`
- `testsuite/tests/vox/fast_environment.ml:100:24`: lemma call `tree_sized`
- `testsuite/tests/vox/fast_environment.ml:136:33`: lemma call `tree_sized` (precise mode only)
- `testsuite/tests/vox/fast_term.ml:163:6`: argument `n`
- `testsuite/tests/vox/fibonacci.ml:77:4`: lemma call `double` (precise mode only)
- `testsuite/tests/vox/fibonacci.ml:83:6`: lemma call `mul_identity`
- `testsuite/tests/vox/fibonacci.ml:84:6`: lemma call `mul_identity`
- `testsuite/tests/vox/fibonacci.ml:89:6`: lemma call `double` (precise mode only)
- `testsuite/tests/vox/fibonacci.ml:116:6`: lemma call `double` (precise mode only)
- `testsuite/tests/vox/flat_hashtbl_public.ml:135:4`: lemma call `equal_def` (precise mode only)
- `testsuite/tests/vox/flat_hashtbl_public.ml:135:34`: lemma call `equal_def` (precise mode only)
- `testsuite/tests/vox/ghost_erasure.ml:62:11`: lemma call `Proof.visit`
- `testsuite/tests/vox/ghost_function_records.ml:14:24`: lemma call `identity`
- `testsuite/tests/vox/ghost_function_records.ml:51:51`: lemma call `identity`
- `testsuite/tests/vox/ghost_refinements.ml:184:11`: `assume_`
- `testsuite/tests/vox/hof_challenges.ml:416:64`: argument `premise`
- `testsuite/tests/vox/hof_challenges.ml:418:9`: lemma call `nonzero_def`
- `testsuite/tests/vox/implicit_predicates.ml:109:2`: lemma call `Instance.explicit`
- `testsuite/tests/vox/implicit_predicates.ml:110:2`: lemma call `Instance.implicit`
- `testsuite/tests/vox/implicit_predicates.ml:111:2`: lemma call `Instance.branch`
- `testsuite/tests/vox/implicit_predicates.ml:112:2`: lemma call `Instance.branch`
- `testsuite/tests/vox/implicit_predicates.ml:113:2`: lemma call `Instance.local`
- `testsuite/tests/vox/implicit_predicates.ml:116:2`: lemma call `Instance.constrained`
- `testsuite/tests/vox/implicit_predicates.ml:117:2`: lemma call `Instance.short_circuit`
- `testsuite/tests/vox/implicit_predicates.ml:118:2`: lemma call `Instance.short_circuit`
- `testsuite/tests/vox/implicit_predicates.ml:119:2`: lemma call `Copy.ghosted`
- `testsuite/tests/vox/implicit_predicates.ml:120:2`: lemma call `Instance.runtime`
- `testsuite/tests/vox/implicit_refinements.ml:502:12`: argument `index`
- `testsuite/tests/vox/int_sets.ml:20:4`: lemma call `List_int_set.lookup_empty`
- `testsuite/tests/vox/int_sets.ml:21:4`: lemma call `List_int_set.lookup_add`
- `testsuite/tests/vox/int_sets.ml:22:4`: lemma call `List_int_set.lookup_union`
- `testsuite/tests/vox/int_sets.ml:23:4`: lemma call `List_int_set.size_zero`
- `testsuite/tests/vox/int_sets.ml:30:4`: lemma call `List_int_set.extensional`
- `testsuite/tests/vox/leaf_provenance_proofs.ml:86:50`: lemma call `Level_unifier_proofs.redirect_desc`
- `testsuite/tests/vox/leaf_provenance_proofs.ml:92:50`: lemma call `Level_unifier_proofs.redirect_desc`
- `testsuite/tests/vox/leaf_provenance_proofs.ml:225:4`: lemma call `Leaf_provenance_spec.low_var_def` (precise mode only)
- `testsuite/tests/vox/leaf_provenance_proofs.ml:225:25`: lemma call `Leaf_provenance_spec.low_var_def`
- `testsuite/tests/vox/list_int_set.ml:214:6`: lemma call `same_repr_reflexive`
- `testsuite/tests/vox/list_int_set.ml:215:6`: lemma call `same_repr_equal`
- `testsuite/tests/vox/list_int_set.ml:293:6`: lemma call `same_repr_reflexive`
- `testsuite/tests/vox/list_int_set.ml:294:6`: lemma call `same_repr_equal`
- `testsuite/tests/vox/lz4_buffer.ml:40:11`: lemma call `M.location_law`
- `testsuite/tests/vox/lz4_buffer.ml:41:11`: lemma call `M.location_law`
- `testsuite/tests/vox/maps.ml:760:49`: lemma call `compare_def`
- `testsuite/tests/vox/maps.ml:760:66`: lemma call `compare_def`
- `testsuite/tests/vox/maps.ml:929:49`: lemma call `compare_def`
- `testsuite/tests/vox/maps.ml:929:66`: lemma call `compare_def`
- `testsuite/tests/vox/merge_sort.ml:78:4`: lemma call `Sort.P.permutation_count`
- `testsuite/tests/vox/nested_pool.ml:62:38`: lemma call `Copy_heap_proofs.put_frame` (precise mode only)
- `testsuite/tests/vox/nested_pool.ml:64:8`: lemma call `Generalize_proofs.close_idempotent` (precise mode only)
- `testsuite/tests/vox/optimized_compression_demo.ml:29:35`: lemma call `active_all`
- `testsuite/tests/vox/optimized_model_proofs.ml:158:10`: lemma call `model`
- `testsuite/tests/vox/optimized_shared_demo.ml:38:32`: lemma call `active_all` (precise mode only)
- `testsuite/tests/vox/optimized_shared_demo.ml:98:4`: lemma call `Copy_spec.cell_def`
- `testsuite/tests/vox/optimized_shared_demo.ml:98:19`: lemma call `Copy_spec.cell_def`
- `testsuite/tests/vox/optimized_shared_demo.ml:126:8`: lemma call `solution`
- `testsuite/tests/vox/optimized_shared_demo.ml:126:20`: lemma call `solution`
- `testsuite/tests/vox/optimized_unifier.ml:163:14`: lemma call `this function`
- `testsuite/tests/vox/optimized_unifier.ml:292:28`: lemma call `Compression_proofs.frame`
- `testsuite/tests/vox/optimized_unifier_datatype_demo.ml:44:61`: lemma call `active_all`
- `testsuite/tests/vox/optimized_unifier_datatype_demo.ml:44:76`: lemma call `active_all`
- `testsuite/tests/vox/polymorphic_list_set.ml:54:6`: lemma call `Element.compare_reverse`
- `testsuite/tests/vox/polymorphic_list_set.ml:93:6`: lemma call `Element.compare_transitive`
- `testsuite/tests/vox/polymorphic_list_set.ml:113:6`: lemma call `Element.compare_transitive`
- `testsuite/tests/vox/polymorphic_list_set.ml:131:6`: lemma call `Element.compare_reverse`
- `testsuite/tests/vox/polymorphic_list_set.ml:383:4`: lemma call `equal_repr_reflexive` (precise mode only)
- `testsuite/tests/vox/polymorphic_list_set.ml:384:4`: lemma call `equal_repr_lookup` (precise mode only)
- `testsuite/tests/vox/polymorphic_list_set.ml:480:4`: lemma call `equal_repr_reflexive` (precise mode only)
- `testsuite/tests/vox/polymorphic_list_set.ml:481:4`: lemma call `equal_repr_lookup` (precise mode only)
- `testsuite/tests/vox/polymorphic_list_set.ml:637:16`: lemma call `equivalent_transitive`
- `testsuite/tests/vox/polymorphic_sets.ml:58:4`: lemma call `Set.lookup_empty`
- `testsuite/tests/vox/polymorphic_sets.ml:59:4`: lemma call `Set.lookup_add`
- `testsuite/tests/vox/polymorphic_sets.ml:60:4`: lemma call `Set.lookup_union`
- `testsuite/tests/vox/polymorphic_sets.ml:61:4`: lemma call `Set.size_zero`
- `testsuite/tests/vox/polymorphic_sets.ml:62:4`: lemma call `Set.equal_lookup`
- `testsuite/tests/vox/polymorphic_sets.ml:70:4`: lemma call `Set.extensional`
- `testsuite/tests/vox/predicate_paths.ml:19:11`: lemma call `conditional_fact`
- `testsuite/tests/vox/predicate_paths.ml:20:11`: lemma call `conditional_fact`
- `testsuite/tests/vox/predicate_paths.ml:21:11`: lemma call `conditional_fact`
- `testsuite/tests/vox/predicate_paths.ml:22:11`: lemma call `conditional_fact`
- `testsuite/tests/vox/predicate_paths.ml:23:11`: lemma call `conditional_fact`
- `testsuite/tests/vox/predicate_paths.ml:24:11`: lemma call `conditional_fact`
- `testsuite/tests/vox/predicate_paths.ml:25:11`: lemma call `conditional_fact`
- `testsuite/tests/vox/predicate_paths.ml:26:11`: lemma call `conditional_fact`
- `testsuite/tests/vox/predicate_paths.ml:27:12`: lemma call `conditional_fact`
- `testsuite/tests/vox/provenance_demo.ml:78:39`: lemma call `scope1`
- `testsuite/tests/vox/provenance_demo.ml:102:39`: lemma call `scope2` (precise mode only)
- `testsuite/tests/vox/provenance_demo.ml:116:22`: lemma call `Copy_spec.cell_def`
- `testsuite/tests/vox/pruned_lower.ml:100:8`: lemma call `Level_proofs.lowering_at` (precise mode only)
- `testsuite/tests/vox/pruned_lower.ml:187:8`: lemma call `Level_proofs.lowering_at`
- `testsuite/tests/vox/pruned_lower_shared_demo.ml:49:15`: lemma call `Copy_spec.cell_def`
- `testsuite/tests/vox/raw_memory_demo.ml:26:11`: lemma call `M.range_at` (precise mode only)
- `testsuite/tests/vox/raw_memory_demo.ml:52:11`: lemma call `M.location_law`
- `testsuite/tests/vox/raw_memory_demo.ml:53:11`: lemma call `M.location_law`
- `testsuite/tests/vox/raw_memory_demo.ml:100:10`: argument `cut`
- `testsuite/tests/vox/raw_memory_demo.ml:161:11`: lemma call `M.location_law`
- `testsuite/tests/vox/recursive_annotations.ml:99:33`: argument `xs`
- `testsuite/tests/vox/regex.ml:349:15`: lemma call `Regex.complete`
- `testsuite/tests/vox/regex.ml:362:9`: lemma call `Regex.complete`
- `testsuite/tests/vox/regex.ml:372:9`: lemma call `Regex.complete`
- `testsuite/tests/vox/relative_generalization_demo.ml:93:39`: lemma call `scope1` (precise mode only)
- `testsuite/tests/vox/relative_generalization_demo.ml:121:39`: lemma call `scope2` (precise mode only)
- `testsuite/tests/vox/relative_generalization_demo.ml:135:22`: lemma call `Copy_spec.cell_def`
- `testsuite/tests/vox/relative_generalization_demo.ml:241:8`: lemma call `Relative_generalization.relative_interpret`
- `testsuite/tests/vox/relative_generalization_demo.ml:247:39`: lemma call `next`
- `testsuite/tests/vox/relative_generalization_demo.ml:247:47`: lemma call `preserved`
- `testsuite/tests/vox/relative_generalization_demo.ml:247:63`: lemma call `equal`
- `testsuite/tests/vox/relative_generalization_demo.ml:247:73`: lemma call `eta_model`
- `testsuite/tests/vox/relative_generalization_demo.ml:258:4`: lemma call `Level_finite_proofs.with_finite_model`
- `testsuite/tests/vox/representative_pool.ml:61:40`: lemma call `Copy_heap_proofs.put_frame` (precise mode only)
- `testsuite/tests/vox/representative_pool.ml:65:10`: lemma call `Generalize_proofs.close_idempotent` (precise mode only)
- `testsuite/tests/vox/rsa.ml:56:11`: `assume_` (precise mode only)
- `testsuite/tests/vox/rsa.ml:57:11`: `assume_` (precise mode only)
- `testsuite/tests/vox/rsa.ml:100:26`: `assume_` (precise mode only)
- `testsuite/tests/vox/rsa.ml:121:13`: `assume_` (precise mode only)
- `testsuite/tests/vox/rsa_public_client.ml:64:9`: lemma call `Spec.prime_def`
- `testsuite/tests/vox/rsa_public_client.ml:64:36`: lemma call `Spec.prime_def`
- `testsuite/tests/vox/rsa_rejected.ml:222:41`: lemma call `Facts.nine_composite`
- `testsuite/tests/vox/sat_cdcl.ml:20:12`: lemma call `Vox_sat.unsat_at`
- `testsuite/tests/vox/sat_cdcl_total.ml:59:12`: lemma call `Vox_sat.unsat_at` (precise mode only)
- `testsuite/tests/vox/sat_cdcl_total.ml:64:12`: lemma call `Vox_sat.unsat_at` (precise mode only)
- `testsuite/tests/vox/sat_cdcl_total.ml:65:12`: lemma call `Vox_sat.unsat_at`
- `testsuite/tests/vox/sat_public.ml:85:12`: lemma call `Vox_sat.unsat_at` (precise mode only)
- `testsuite/tests/vox/sat_public.ml:86:12`: lemma call `Vox_sat.unsat_at`
- `testsuite/tests/vox/sat_public.ml:93:12`: lemma call `Vox_sat.unsat_at`
- `testsuite/tests/vox/sat_solver.ml:34:23`: lemma call `Vox_sat.unsat_at`
- `testsuite/tests/vox/sets.ml:532:49`: lemma call `compare_def`
- `testsuite/tests/vox/sets.ml:532:66`: lemma call `compare_def`
- `testsuite/tests/vox/sets.ml:557:49`: lemma call `compare_def`
- `testsuite/tests/vox/sets.ml:557:66`: lemma call `compare_def`
- `testsuite/tests/vox/sets.ml:646:49`: lemma call `compare_def`
- `testsuite/tests/vox/sets.ml:646:66`: lemma call `compare_def`
- `testsuite/tests/vox/stlc_generate.ml:41:13`: lemma call `Stlc_graph_proofs.allocation_mem`
- `testsuite/tests/vox/stlc_generate.ml:68:13`: lemma call `Stlc_graph_proofs.allocation_mem`
- `testsuite/tests/vox/stlc_generate.ml:72:13`: lemma call `Stlc_graph_proofs.allocation_mem`
- `testsuite/tests/vox/stlc_generate.ml:76:14`: lemma call `Stlc_graph_proofs.allocation_mem`
- `testsuite/tests/vox/table_model.ml:101:4`: lemma call `equal_def` (precise mode only)
- `testsuite/tests/vox/table_model.ml:101:34`: lemma call `equal_def` (precise mode only)
- `testsuite/tests/vox/table_model.ml:104:19`: lemma call `hash_def` (precise mode only)
- `testsuite/tests/vox/table_model.ml:104:31`: lemma call `hash_def` (precise mode only)
- `testsuite/tests/vox/table_model.ml:121:4`: lemma call `M.initial_def`
- `testsuite/tests/vox/table_model.ml:136:4`: lemma call `M.initial_def`
- `testsuite/tests/vox/table_model.ml:278:13`: lemma call `H.put_law` (precise mode only)
- `testsuite/tests/vox/table_model.ml:285:13`: lemma call `H.put_law` (precise mode only)
- `testsuite/tests/vox/table_ownership_rejected.ml:21:4`: lemma call `equal_def` (precise mode only)
- `testsuite/tests/vox/table_ownership_rejected.ml:21:34`: lemma call `equal_def` (precise mode only)
- `testsuite/tests/vox/table_ownership_rejected.ml:24:19`: lemma call `hash_def` (precise mode only)
- `testsuite/tests/vox/table_ownership_rejected.ml:24:31`: lemma call `hash_def` (precise mode only)
- `testsuite/tests/vox/terminal_lower_proofs.ml:148:32`: lemma call `frame` (precise mode only)
- `testsuite/tests/vox/time_credits.ml:45:9`: lemma call `C.nonnegative`
- `testsuite/tests/vox/unifier_finite_demo.ml:161:6`: lemma call `model`
- `testsuite/tests/vox/unifier_finite_demo.ml:161:15`: lemma call `model`
- `testsuite/tests/vox/unifier_finite_demo.ml:180:20`: lemma call `solution`
- `testsuite/tests/vox/unifier_finite_demo.ml:180:32`: lemma call `solution`
- `testsuite/tests/vox/unifier_proofs.ml:312:10`: lemma call `model`
- `testsuite/tests/vox/union_find.ml:85:39`: lemma call `U.alpha_bounds`
- `testsuite/tests/vox/union_find.ml:94:10`: lemma call `U.contents_def`
- `testsuite/tests/vox/union_find.ml:117:10`: lemma call `U.contents_def`
- `testsuite/tests/vox/union_find.ml:130:10`: lemma call `U.contents_def`
- `testsuite/tests/vox/union_find.ml:158:42`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find.ml:159:42`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find.ml:174:10`: lemma call `U.contents_def`
- `testsuite/tests/vox/union_find.ml:176:42`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find.ml:178:42`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find.ml:191:10`: lemma call `U.contents_def`
- `testsuite/tests/vox/union_find.ml:192:42`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find.ml:194:42`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find.ml:208:10`: lemma call `U.contents_def`
- `testsuite/tests/vox/union_find.ml:210:42`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find.ml:211:42`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find.ml:212:42`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find.ml:231:10`: lemma call `U.contents_def`
- `testsuite/tests/vox/union_find.ml:232:4`: lemma call `F.member_same`
- `testsuite/tests/vox/union_find.ml:232:42`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find.ml:234:42`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find.ml:235:42`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find.ml:248:10`: lemma call `U.contents_def`
- `testsuite/tests/vox/union_find.ml:249:4`: lemma call `F.member_same`
- `testsuite/tests/vox/union_find.ml:249:42`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find.ml:250:4`: lemma call `F.member_same`
- `testsuite/tests/vox/union_find.ml:250:42`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find.ml:252:42`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find.ml:265:10`: lemma call `U.contents_def`
- `testsuite/tests/vox/union_find.ml:266:4`: lemma call `F.member_same`
- `testsuite/tests/vox/union_find.ml:266:42`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find.ml:267:4`: lemma call `F.member_same`
- `testsuite/tests/vox/union_find.ml:267:42`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find.ml:268:4`: lemma call `F.member_same`
- `testsuite/tests/vox/union_find.ml:268:42`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find.ml:282:10`: lemma call `U.contents_def`
- `testsuite/tests/vox/union_find.ml:283:4`: lemma call `F.member_same`
- `testsuite/tests/vox/union_find.ml:283:42`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find.ml:284:4`: lemma call `F.member_same`
- `testsuite/tests/vox/union_find.ml:284:42`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find.ml:285:4`: lemma call `F.member_same`
- `testsuite/tests/vox/union_find.ml:285:42`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find.ml:286:4`: lemma call `F.member_same`
- `testsuite/tests/vox/union_find.ml:286:42`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find.ml:289:44`: lemma call `C.nonnegative`
- `testsuite/tests/vox/union_find.ml:357:4`: lemma call `U.account_bounds`
- `testsuite/tests/vox/union_find_online.ml:51:30`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:53:45`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:61:11`: lemma call `U.union_semantics`
- `testsuite/tests/vox/union_find_online.ml:65:6`: lemma call `F.member_same`
- `testsuite/tests/vox/union_find_online.ml:66:39`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:67:6`: lemma call `U.representative_def`
- `testsuite/tests/vox/union_find_online.ml:81:6`: lemma call `U.member_def` (precise mode only)
- `testsuite/tests/vox/union_find_online.ml:84:11`: lemma call `U.member_def` (precise mode only)
- `testsuite/tests/vox/union_find_online.ml:94:30`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:96:45`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:97:6`: lemma call `U.observations` (precise mode only)
- `testsuite/tests/vox/union_find_online.ml:104:11`: lemma call `U.union_semantics`
- `testsuite/tests/vox/union_find_online.ml:108:6`: lemma call `F.member_same`
- `testsuite/tests/vox/union_find_online.ml:109:39`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:110:6`: lemma call `U.representative_def`
- `testsuite/tests/vox/union_find_online.ml:113:12`: lemma call `U.observations` (precise mode only)
- `testsuite/tests/vox/union_find_online.ml:124:6`: lemma call `U.member_def` (precise mode only)
- `testsuite/tests/vox/union_find_online.ml:127:11`: lemma call `U.member_def` (precise mode only)
- `testsuite/tests/vox/union_find_online.ml:137:30`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:139:45`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:140:6`: lemma call `U.observations` (precise mode only)
- `testsuite/tests/vox/union_find_online.ml:147:11`: lemma call `U.union_semantics`
- `testsuite/tests/vox/union_find_online.ml:151:6`: lemma call `F.member_same`
- `testsuite/tests/vox/union_find_online.ml:152:39`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:153:6`: lemma call `U.representative_def`
- `testsuite/tests/vox/union_find_online.ml:156:12`: lemma call `U.observations` (precise mode only)
- `testsuite/tests/vox/union_find_online.ml:166:12`: lemma call `F.member_same`
- `testsuite/tests/vox/union_find_online.ml:167:6`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:286:30`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:288:45`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:296:11`: lemma call `U.union_semantics`
- `testsuite/tests/vox/union_find_online.ml:300:6`: lemma call `F.member_same`
- `testsuite/tests/vox/union_find_online.ml:301:39`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:302:6`: lemma call `U.representative_def`
- `testsuite/tests/vox/union_find_online.ml:316:6`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:319:11`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:329:30`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:331:45`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:339:11`: lemma call `U.union_semantics`
- `testsuite/tests/vox/union_find_online.ml:343:6`: lemma call `F.member_same`
- `testsuite/tests/vox/union_find_online.ml:344:39`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:345:6`: lemma call `U.representative_def`
- `testsuite/tests/vox/union_find_online.ml:359:6`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:362:11`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:372:30`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:374:11`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:384:30`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:386:11`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:396:30`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:398:11`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:408:30`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:410:11`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:420:30`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:422:11`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:432:30`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:434:45`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:442:11`: lemma call `U.union_semantics`
- `testsuite/tests/vox/union_find_online.ml:446:6`: lemma call `F.member_same`
- `testsuite/tests/vox/union_find_online.ml:447:39`: lemma call `U.member_def`
- `testsuite/tests/vox/union_find_online.ml:448:6`: lemma call `U.representative_def`
- `testsuite/tests/vox/union_find_online.ml:461:12`: lemma call `F.member_same`
- `testsuite/tests/vox/union_find_online.ml:462:6`: lemma call `U.member_def`
- `verification/library/borrow.ml:137:13`: lemma call `Model.set_length`
- `verification/library/borrow.ml:143:13`: lemma call `Model.set_length`
- `verification/library/borrow.ml:185:13`: lemma call `Model.append_length`
- `verification/library/borrow_iarray.ml:120:13`: lemma call `Vox_iarray.updated_length`
- `verification/library/vox_big_credits.ml:55:12`: lemma call `credits_def` (precise mode only)
- `verification/library/vox_big_credits.ml:70:12`: lemma call `credits_def` (precise mode only)
- `verification/library/vox_big_credits.ml:75:7`: argument `right` (precise mode only)
- `verification/library/vox_big_credits.ml:85:12`: lemma call `credits_def`
- `verification/library/vox_big_credits.ml:85:30`: lemma call `credits_def`
- `verification/library/vox_credits.ml:56:12`: lemma call `credits_def` (precise mode only)
- `verification/library/vox_credits.ml:71:12`: lemma call `credits_def` (precise mode only)
- `verification/library/vox_credits.ml:87:12`: lemma call `credits_def` (precise mode only)
- `verification/library/vox_credits.ml:87:30`: lemma call `credits_def` (precise mode only)
- `verification/library/vox_iarray.ml:384:6`: lemma call `slice_length`
- `verification/library/vox_int_sequence.ml:193:6`: lemma call `element_at`
- `verification/library/vox_int_sequence.ml:220:7`: lemma call `element_at`
