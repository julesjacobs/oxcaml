# The talk's evidence: slide → test

The talk (`research/presentation-20260927/STRUCTURE-v3.md`) promises that
every number and quoted output comes from `./dev test` at the pinned commit.
This manifest maps each slide that quotes a program's verdict or output to
the test that produces it. `extract.py` reads the outputs from these files:
from the `[%%expect{|…|}]` blocks of expect tests (the first block, not a
`Principal{|…|}` one), from `*.reference` files for run tests, and from
`*.compilers.reference` files for compiler output.

Paths are relative to `testsuite/tests/vox/`. Run one test with
`./dev test vox/<path>`, or all of them with `./dev test vox`.

Status:
- **trunk**: passes on `jujacobs/vox/trunk-20260926`.
- **awaits fix**: fails on trunk by design; its expected output is the
  output with the named fix merged, and it passes then. When the fix is
  merged, the test passes unchanged; if the fix's diagnostics change before
  it lands, re-promote the test on the merged commit and check the diff.

## §1 The language in 150 seconds

| Slide item | Test | What it checks | Status |
|---|---|---|---|
| The four playground examples (`refinements.ml`, `counterexample.ml`, `lemma.ml`, `ghost_token.ml`), native verdicts | `talk-playground/test.ml` → `test.reference` | Compiles the files in `verification/playground/examples` as they are at this commit; `counterexample.ml` gives `counterexample: x = 4611686018427387903`; `ghost_token.ml` gives the uniqueness error | trunk |
| `x + 1 > x` fails at `max_int` | `talk_language_basics.ml` (`incr_pos`) | `counterexample: x = 4611686018427387903`; with `x < max_int` accepted | trunk |
| Distinction 1: `x * x >= 0` gives "countermodel for abstract multiplication: x = 0" | `talk_language_basics.ml` (`square`) | the verdict and message | trunk |
| Distinction 3: totality is not purity | `talk_language_basics.ml` (`read_counter`) | reading a `ref` in total code is rejected (`(!)` is partial) | trunk |

## §2 What worked (claim 6)

| Slide item | Test | What it checks | Status |
|---|---|---|---|
| Binary-search midpoint: `(left + right)/2` rejected with `counterexample: lower = 4611686018427326839, upper = 4611686018427387903, …`; `left + half` accepted | `talk_midpoint_overflow.ml` | both verdicts, the counterexample verbatim | trunk |

## §3 The hash table

| Slide item | Test | What it checks | Status |
|---|---|---|---|
| Tombstones: EMPTY fails at "no new EMPTY byte" (`vox_table_update_proofs.ml`, lines 442-443); 254 accepted | `talk_flat_hashtbl.ml` (`Empty_check`, `Tombstone_check`) | the rejection and the stated-here location in the library; the 254 proof accepted | trunk |
| A commuted `symmetric` law is accepted; a law stating one direction of symmetry is rejected with a counterexample | `talk_flat_hashtbl.ml` (`Commuted_table`, `One_way_table`) | `module Commuted_table : sig end`; `The value "One_way_key.symmetric" does not satisfy the functor's parameter.` with `counterexample: x = 0, y = 1`; `Int_key` with the laws as in `Key` accepted (`Int_table`) | trunk (with subsumption merged) |
| "Store 84, read 85": 85 rejected with the law calls | `flat_hashtbl_boundary.ml` (existing) | the false claim 85 is rejected | trunk |
| "Without them even 84 is rejected" | `talk_flat_hashtbl.ml` (`Store_84_without_laws`, `Store_84_with_laws`) | 84 without `Key.reflexive`/`Map.put_get` rejected; with them accepted | trunk |
| Ownership walk: stale view, reused token, also without the extension | `flat_hashtbl_boundary.ml`, `flat_hashtbl_stale.compilers.reference`, `flat_hashtbl_reused.compilers.reference`, `flat_hashtbl_unowned.compilers.reference` (existing) | refinement error; uniqueness error; both without `-extension refinement_types` | trunk |

## §0 Cold open

| Slide item | Test | Status |
|---|---|---|
| `find_after_replace`, its `-O3` Cmm, and the erasure audit | `flat_hashtbl_public.ml`, `flat_hashtbl_boundary.ml` with `flat_hashtbl_boundary.checks-native.reference` (existing) | trunk. The emitted Cmm is checked by pattern, not recorded verbatim; the ISA shown (DECIDE N) is the machine's. |

## §4 Mechanisms

| Beat | Slide item | Test | What it checks | Status |
|---|---|---|---|---|
| 4a | The two false-contract mutants (no-op sort, zeroing sort) | `quicksort_rejected.ml` (existing, the modules after `Blocking_callback`) | each half of the contract is needed | trunk |
| 4b | fork-join parallel sort | `quicksort.ml` (existing) | | trunk |
| 4b | `roundtrip` accepted; forged `int C.recv` read rejected | `one_shot_public_client.ml`, `one_shot_rejected.ml` (`forged_result`) (existing) | | trunk |
| 4c | The six-row restriction table | `talk-atomics-deletions/test.ml` → `test.reference` | each row: the lines the weakened copy changes, the client against the real interface and against the weakened copy, and the run where the table says what happens (see below) | trunk |
| 4c | "Natively the CAS is `caml_vox_atomic_cas a 1 3 48059 48059`" | `talk_atomic_erasure.ml` → `talk_atomic_erasure.compilers.reference` | the native `-dcmm` of `try_acquire`: `(extcall "caml_vox_atomic_cas" a 1 3 48059 48059 …)` | trunk |
| 4d | `e8_frame.ml`: framing by exact contract and by `Pref.split`; without the split rejected | `talk_heap_frame.ml` | `whole`, `split_off` accepted and run (15, 10); `no_split` rejected at `pref.mli` line 139 | trunk |
| 4e | The six credits attacks | `time_credits_rejected.ml` (existing, one module added) | `Mint`, `Reuse`, `Borrowed_spend` (added), `Ghost_consume`, `Foreign_budget`, `Free_comparison` | trunk |
| 4f | `unreachable_ ()` for `Unknown` compiles against `solve_complete`, rejected against `solve fuel` | `talk_sat_unknown.ml` | both verdicts | trunk |
| 4f | "Drop the weight and the proof fails at the line that needs it" | `talk-sat-measure/test.ml` → `test.reference` | a mutant copy of `vox_cdcl_total_proof.ml` with measure `absent * 1 + unassigned` is rejected in `progress_learning` (line 4197); the real module is accepted | trunk |
| 4g | `add_def`'s printed type via `ocamlc -i` | `talk_add_def_interface.ml` → `talk_add_def_interface.compilers.reference` | the `-i` output verbatim, and the induction `add_zero` accepted | trunk |

### The atomics table (4c): weakened copies

The rows need interfaces with one restriction deleted. The copies are never
stored: `talk-atomics-deletions/run.sh` derives each one at test time from
the library's current `verified_atomic.mli/.ml` (or `unique_cell.mli/.ml`)
by one substitution, and the reference records exactly the lines it
changed. So each copy is always "today's trusted interface minus exactly
this restriction", and if the interface changes so that a substitution no
longer applies, the recorded change lines show it. The copies exist only in
the test's build directory and are labelled "weakened copy" in the output.

| Row | Deleted | Real interface | Weakened copy |
|---|---|---|---|
| 1 | `@ local contended` on the handle | `spawn_client.ml` compiles | compiles: nothing breaks |
| 2 | `@@ portable` on the five operations | (compiles) | `The value bump is nonportable` |
| 3 | `@ unique` on the caller token | `stale_read.ml`: uniqueness error at the read after release | compiles; runs and prints `refinement {v \| v = 5} violated: read 6` |
| 4 | `@ unique` on the transition results | `dup.ml`: uniqueness error | compiles |
| 5 | `@ total` on the transition | `diverge.ml`: `The value loop is partial` | compiles; runs: the CAS on a held lock returns false and the cell is written anyway |
| 6 | `Slot.take`'s ghost premise | `confuse.ml`: refinement error at `unique_cell.mli` line 77 | compiles; runs and crashes: `exit 139 (SIGSEGV)` |

Row 3 differs from the investigation's run: that used two domains (a
multidomain build) and printed the violation a few times per run. The test
runs one domain through the same interleaving (the second holder's
critical section happens between the first holder's release and its read),
so the output is deterministic. The slide should say "an interleaving
prints", or be recorded on a multidomain build.

## §5 Testing

| Slide item | Test | What it checks | Status |
|---|---|---|---|
| The idiom `let u = () in assume_ u`, the driver, the recorded run | `property_testing.ml` → `property_testing.reference` | fixed seed (`Random.init 2026`); the same output in bytecode, bytecode `-principal`, and native `-O3 -noassert`: `prop_insert OK, 10000 cases`, `prop_insert_buggy FAILED after 2 cases: x = 1, xs = [-3; -2; 1; 3] (property_testing.ml:48)` | trunk |
| `assume_ ()` is a compile error | `property_testing_rejected.ml` | `"assume_" requires a plain local variable` | trunk |
| The lemma used as a fact; delete the call and it is rejected | `property_testing_rejected.ml` (`insert_sorted`, twice) | accepted with `prop_insert x xs;`, rejected without | trunk |
| Remove the driver's filter and the call is rejected | `property_testing_rejected.ml` (`run`) | rejected at `prop x xs` | trunk |
| The check computes only `sorted (insert x xs)`, never `sorted xs` | `property_testing_lambda.ml` | the `-dlambda` of the property: `(if (apply sorted (apply insert x xs)) u (raise …))` | trunk |

Note: the line in the run's output is 48, not the investigation's 32,
because the test file starts with its test header.

## §6 How we hunt soundness bugs

Verdicts only, each with an accepted control.

| Row | Reproducer | Test | Status |
|---|---|---|---|
| Out-of-range shifts | `shift.ml`: `(1 lsl n) = (1 lsl 64)` | `int_shift_range.ml` (`same`, `same_lsr`, `same_asr`; controls `bit`, `bounds`), added by the fix | trunk (fix merged, `cca8ba889a`) |
| | `oob.ml`: the string read that segfaulted | `talk_soundness_shift_read.ml` (control `read_in_range`, run) | trunk |
| Effectful callbacks | `rel.ml`, `rel2.ml`, `rel3.ml`: `apply tick 0 = apply tick 0`, `List.map`, `unreachable_` | `call_congruence.ml` (`stateful_callback`, `stateful_map`, `crash`; control `apply inc`), added by the fix | trunk (fix merged, `0a76bf52f1`) |
| Hidden types in the totality check | `knot.ml` (GADT existential), `knot_false.ml` (abstract type) | `talk_soundness_totality_knot.ml` (controls: an immediate existential, an immediate abstract type) | **passes**: fix merged in `09dfd7e2fa`. One route remains open: an abstract type equal to a function type, consumed by an exported total function |
| Ghost-field locality | `x4_borrow_escape.ml`, `x5_store_borrow.ml` | `ghost_field_ownership.ml` (`peek`/`one`, `box`; controls `get_borrowed`, `peek_global`, `box_global`), added by the fix | trunk (fix merged, `a0b99ddfeb`) |
| Name-keyed built-ins | `natname.ml`: `caml_bigint_add`/`caml_bigint_sub` | `builtin_declaration_identity.ml` (control `library`) | **passes**: fixed by `9864700994`, merged in `a187b9ab9c` |
| | `e1_forged.ml`: a client external named `caml_borrow_finish` | `talk_soundness_borrow_symbol.ml` (control `resolved`) | **passes**: same fix; test added in `a9affa02d4` |

## §9 The ask, and the hard questions

| Slide item | Test | Status |
|---|---|---|
| "The totality axis is active even without the extension: `let rec ones = 1 :: ones` is rejected" | `talk_cyclic_list_no_extension.ml` (compiled without the extension; a non-inductive cyclic value stays accepted) | trunk |

## Presenting limits

| Slide item | Test | Status |
|---|---|---|
| `e7_partial.ml` "promises" 99, writes 7, and raises | `talk_heap_frame.ml` (`fake`, accepted) | trunk |

## Not converted

- **Numbers from measurement** (the functor's 383 vs 4 queries, checking
  times, resource counts, `_def` counts, compile overhead, HM→Wasm step
  ratios) are P0 item 5, regenerated by `extract.py` at the pinned commit,
  not by tests. Resource counts differ between platforms and are not part
  of test output.
- **The two-domain race of row 3 of the atomics table** needs a
  multidomain build; the test runs the same interleaving in one domain.
- **The total `alloc` example** (§4d, optional: `Pref.equal (mk ()) (mk ())`
  "verified true", prints false) is not a test: it declares a second
  external for `caml_pref_alloc_step`, whose meaning the name-keyed fix
  (`9864700994`, merged) removes, so this example no longer applies.
- **DECIDE M** (a rejected non-minimal Myers script): `nonminimal` in `diff_rejected.ml` is that test; scene 04g quotes it.
