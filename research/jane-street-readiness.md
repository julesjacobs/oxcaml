# Jane Street readiness checklist

What remains before the Vox demos are shown to the Jane Street compiler
team. Update this file as items land: tick the box and add the commit.
Decisions and their reasons are in
`research/warts-investigation-20260927/DECISIONS.md` (outside the repo, in
the Vox research directory); the catalogue is `verification/catalogue`.

## How this is being worked (27 September)

Waves, merged into `vox` with a full suite run on the AMD box after each:
1. **Now:** the ghost-field soundness fix; verifier and solver warts
   (`jujacobs/vox/fix-verifier-20260927`); build, tooling and checks in the
   suite (`jujacobs/vox/fix-tooling-20260927`); HM-to-Wasm examples, run-time
   input and model tests on the AMD box; the subsumption design.
2. **After the soundness fix merges:** typer bugs and small language warts;
   diagnostics and lints; removal of stale proof workarounds; code
   presentation; the remaining weak contracts.
3. **After wave 2 merges:** the second review round of every page, page
   corrections, then status **Ready for owner review**.
4. **Started early where independent:** subsumption stages 1–4, solver
   encoding and termination measures (34, 77), inductive relations and named
   predicates (56, 20), all on the AMD box. Later: ghost ownership and
   layouts (after the soundness fix), the prebuilt test library (after the
   tooling branch), the x86 benchmark (on an idle AMD box).

Testing policy (27 September): a branch merges when its agent's own run
passes and every affected test passes (`./dev test --affected <base>` once
item 13 lands). The full suite then runs asynchronously on the AMD box after
merges, batched; a red result means a fix-up or a revert. `vox` is pushed
only at commits whose full suite passed.

No GitHub CI (owner's decision, 27 September): the fork's upstream workflows
need Jane Street's paid runners and never run. The full suite is our own run
on the AMD box (`~/vox-check-run.sh` on the `incoming` branch there).

Catalogue statuses: **Reviewed** means checked by the owner. **Ready for owner
review** means two independent reviews found no false claim and the page
states every gap. Decisions that need the owner are collected at the end of
this file.

## Must have

- [ ] **1. Soundness.**
  - [x] Ghost-field ownership fix, merged in `3f313103a3` (checker fix
        `ec365027eb`). No demo took a token twice; the demos that broke relied
        on the construction-side hole (repairs in `3b2a3047e5`, `13f99dd368`).
  - [x] W8: type parameters used only in refinement predicates are now
        invariant (merged in `4e24de3229`). Still open through type
        abbreviations and re-exported signatures; closes with subsumption
        stage 2 (predicates compared with their types).
  - [x] W9: the caches are keyed by the compiler digest and solver version
        (`4e24de3229`); follow-up: query cache keyed by solver only, and a
        timeout on the version probe.
  - [ ] Compare refinement predicates with their types (subsumption
        stage 2; design in `research/subsumption-design-20260927`).
- [ ] **2. Second review round** (brief:
      `research/demo-review-20260926/ROUND2-BRIEF.md`, Claude and Codex per
      demo), after item 1 merges; correct each page until **Reviewed**.
  - [x] flat-hash-table, one-shot-channels: **owner-review** (merged
        `103a45730c`; the rejected table example lacked the proof that
        made its false claim the only reason for failure, fixed; the Cmm
        excerpt regenerated from the current build on x86-64). Owner:
        keep the full 201-line interface or use excerpts? The Cmm stamps
        go stale on compiler changes and nothing checks the excerpt.
  - [x] regex-automata, hindley-milner, binary-search, constant-folding,
        online-union-find: **owner-review** (merged `8de1969188`). HM: all
        eight unreachable arms are `unreachable_ ()`; regex: the
        open-question section corrected (iarray bound reason, `lower`
        provably `None` for u ≥ 63 in the logic).
  - [x] mode-solver, register-allocation, rsa: **owner-review** (merged
        `f55601c65d`; regalloc's "native code drops 93 lemmas" was false:
        92 lemmas, each a placeholder function in both backends).
  - [x] functional-queue, merge-sort, quicksort, sparse-arrays:
        **owner-review** (merged `bf28fa14ea`; merge-sort and quicksort
        each had a false "no test rejects …" claim).
  - [ ] In flight (27 September, evening): myers-diff · http · lz4 |
        avl-sets · rings · lists-trees.
  - [ ] Also in flight: egraphs | sat-solver | dfa-equivalence ·
        reference-locks (after deduplication and the inventory comments).
  - [ ] hm-wasm-compiler, after item 3.
- [ ] **3. HM-to-Wasm is not trivial** (AMD box).
  - [x] Example programs compiled by the verified compiler and run, with
        exact results and resource use, as a suite test: eight programs, all
        agreeing with Node (`hmc_compilation_examples.ml`, merged in
        `e8eb54786f`). Resource theorems: not attempted; what they need is in
        `research/hmc-resources-20260927/resource-theorem-notes.md` (only self
        tail calls reuse frames; no garbage collector; layout rejections
        depend on inference, which is not proved deterministic).
  - [x] Input as a run-time value through the exported `payload` global;
        theorems for every input (merged in `01b1df3709`); the example suite
        runs one module per program on several inputs (`1111bd1927`). Open,
        stated on the page: a module whose heap starts full could make
        `sufficient` false and always report exhaustion.
  - [x] WebAssembly model tested against Node: 100,000 random modules, no
        disagreement; a 40-module smoke check in the suite (merged in
        `123348b625`, `78020e455a`).
- [x] **4. Every page claim checked by `./dev test`** (merged in
      `075d6f9146`). The 13 hand-run scripts are ocamltest tests with exact
      expected errors and positive controls, and are deleted; every page's
      Reproduce section lists only `./dev test` commands. Outside the suite,
      stated on the pages: `lz4_interop.py` (needs liblz4) and `run-diff-demo`
      on other inputs.
  - [x] E-graphs: boundary reviewed (the trusted surface is 13 files, as the
        page says), manifest regenerated, check in the suite.
- [x] **5. Bugs an expert could hit live**, all fixed with regression tests:
      the `include` escape error and crash, `-i` dropping `@@ total`, the
      dependent-parameter sugar, the identical-types `let rec` error (typer
      branch, merged in `011f3cafb8`); `assert false` and `assert e`,
      `max_int`/`min_int`/`abs`/shifts (verifier branch, `10d8f2bcd8`).
- [ ] **6. A person reads the four route pages end to end:** flat hash
      table, Myers diff, one-shot channels, SAT.

## Should have

- [x] **7. Code presentation of the shown demos** (presentation merged
      `9a6bc6ab5d`, deduplication merged `70d651c363`): comments in large files
      (AVL, e-graphs, the compiler); hand-written interfaces instead of
      compiler-printed ones (HTTP, mode solver, SAT spec); remove duplicated
      code (two DFA pipelines, near-copy lock modules, SAT solver variants).
- [ ] **8. Remaining weak contracts** (fixed, or stated on the page):
  - [x] LZ4 `decompress` contract, via labelled arguments in dependent
        types (wart 38; `?capacity:(c : int) ->`; merged `2c5c38bbb0`).
        Follow-ups: `parsing/attributes.ml` and `parsing/extensions.ml`
        `-dparsetree` references lack the `None` binder line (pre-existing);
        Merlin not updated for Vox typer changes.
  - [x] HTTP: malformed request and header lines must be rejected (three
        rejection laws; `Invalid_crlf` and mid-line budget exhaustion stay
        unspecified, stated on the page; merged `a4f6450a19`).
  - [x] Rings: insert and remove in the `Owned` interface (any length,
        exact heap; uses the heap laws `put_law`/`commute_law`, stated;
        `Pref_ring_general.Proofs` is public; merged `a4f6450a19`).
  - [x] Lists/trees: lemma relating `contents`, `nodes` and `List.rev`
        (via `rev_onto`; `Pref_tree.observe` returns an exact model; merged
        `a4f6450a19`).
- [ ] **9. Diagnostics and proof noise.**
  - [x] Name the failing conjunct of an `&&` goal (wart 14, `10d8f2bcd8`).
  - [ ] Erasure lints: total function with a ghost result whose body is not
        `ghost_`; real code discarding a ghost result (warts 41, 42).
  - [x] Remove the stale workarounds now unnecessary: about 555 lines net in
        15 demos, merged in `4c74143232`. Leftovers for item 7: client files
        the pages quote still use `refine_` and `let u = () in u`
        (quicksort, queue, sorted-array, expressions, the DFA and regex
        clients); `dfa_equivalence_proof` (313), `register_allocation` (85)
        and the HM files still have many `let u = () in`.
- [ ] **10. Performance story.**
  - [ ] Flat hash table benchmark on x86-64 (AMD box).
  - [ ] State on the compiler page that the backend manages its own stack
        in memory (for honest exhaustion) and is not an optimizing backend.

## Nice to have

- [ ] **11. Medium warts:** ghost ownership inside `ghost_` (12, 45, 46,
      52), layouts (10, 11, 13), well-formedness obligations for partial
      operations (67). Done: termination measures (77) and bitwise solver
      fallback (34); named predicates (20) as `[@def transparent]`, with the
      flat hash table's precondition named `current table view` (merged
      `0a37492c3d`); inductive relations (56) are not a language feature —
      the owner chose the hand-written derivation pattern
      (`testsuite/tests/vox/relations.ml`) and listed inductive definitions
      as a future mechanism in the catalogue.
- [ ] **12. Subsumption stages 1–4**, implemented as one piece from a
      complete design (branch `jujacobs/vox/subsumption-20260927`, AMD box).
      Afterwards, compare the implementation with the independent design in
      `research/subsumption-design-20260927/DESIGN.md` and its tests, and
      report divergences before merging.
      *27 September:* implemented (`IMPLEMENTATION.md` §7 lists deviations);
      owner accepted deviation 1 (modes pass only to tuple components) and
      deviation 2 (predicate node types compared by skeleton), the latter
      subject to an independent check that the verifier's encoding depends
      only on skeletons. Agent is merging trunk and dropping its own cache
      fix in favour of trunk's.
- [x] **13. Prebuilt test library** (wart 33), `./dev test --affected` and
      `VOX_TEST_TIMEOUT` (merged in `6390fe236b`): the full suite takes about
      3 minutes warm and 6.5 after a compiler change, from 43 and 60. Being
      made fully green on the merged trunk (branch
      `jujacobs/vox/suite-green-20260927`), including a cache-dependent
      counterexample in two tests.
- [ ] **Unused proof steps from unsat cores** (owner's request, 27 September).
      When a goal is proved, ask Z3 which named facts it used (`(get-unsat-core)`),
      and report lemma calls and argument or path assumptions that no proof in
      the function needed, as a warning. Notes:
      - Naming assertions changes Z3's search and so the resource counts;
        keep normal proofs unaffected by rerunning an already proved query with
        named facts only when the check is requested, under a flag
        (e.g. `-dsmt-unused-facts`, or a warning off by default), and cache it.
      - Cores are not minimal. A fact is reported only if it is absent from
        every core that covers its uses, or confirmed by re-proving without it
        (more precise, slower; both behind the flag).
      - Map facts back to source: each lemma call's result, each refined
        argument, each `assume_`; batched goals need per-goal cores.
      - Measure the cost on the library; if small, consider running it in
        `dev test` by default.
- [ ] **Warning 226 precision** (owner's decision, 27 September): keep 226
      on by default; fix the check rather than suppressing. (a) An
      application is erasable only if its arguments' required modes survive
      capture by `ghost_` (immutable, aliased); (b) an undetermined mode is
      not capturable; (c) report all layers at once: a candidate is
      proof-only if every use is ghost or inside another proof-only
      candidate's definition (fixpoint). Then remove the two
      `[@warning "-226"]` in `maps.ml` and `dependent_expressions.ml`.
      Start after subsumption merges (both touch `typecore.ml`).
- [x] **E-graph comments in the 13 inventory-locked files**, regenerating
      `vox_egraph_rule_handle.spec.json` (owner: yes): a header comment in
      each, before the first declaration; the inventory changes only in
      hashes and line numbers (`d7e234bcfa`, merged). Flat hash table
      page keeps its full interface (owner: yes).
- [x] **Browser playground** (owner's request): `verification/playground/`,
      merged `354f2c5dfd`; front end and verifier via js_of_ocaml, Z3 4.16.0
      as Wasm; 0 disagreements with native on 265 inputs; clickable
      locations. Hosting is the owner's decision (needs COOP/COEP headers
      or the bundled coi-serviceworker).
- [ ] **Hand-written mode-solver semantics interfaces** (the three `.mli`
      printed by the compiler, primed names), like the public one.
- [ ] **`-principal` rejections** of verified code: the RSA library
      (`vox_rsa_fermat.ml:150`, "ys is partial but expected total"),
      unannotated parameters used in predicates (labelled-args report),
      polymorphic `[@def] rec` helpers under `ghost_` (presentation report).
      One root cause is likely; investigate and fix in the typer.
- [ ] **Erased lemmas still compile to placeholder functions** (both
      backends; stated on the pages). Stripping exported lemma fields would
      be a compiler change; decide whether it matters for the pitch.
- [ ] **Multidomain test runs**: no build is configured with
      `--enable-multidomain`, so the 7 parallel tests (one-shot, buffer,
      raw memory, borrow, quicksort frame, locks) never run. Agent setting
      up `~/git/vox-multidomain` on the AMD box (27 September).
- [ ] **`_trust.md` line 13** (owner-reviewed page) conflates totality and
      statelessness: total functions may write through uniquely owned
      storage (`Quicksort.sort`). Wording proposed to the owner.
- [ ] **14. Upstream OxCaml reports:** the expect tool overwrites single
      blocks with principal output; the `node option` kind error.
- [ ] **Small warts** (group 3 in the conversation of 27 September):
      ghost field of a ghost record (9), implicit `'a` in predicates (1),
      constructor terms at any type (4), `[@def]` on constants (5), lemma
      premise assumed (57), top-level `@ total` in expect tests (79),
      comparator key distinctness (65), total division (71),
      `Bigint.to_int_opt` (69), iarray length bound (68), heap
      extensionality (74), atomics (73), table modules at `-O3` (19, done),
      `binary_modules` documentation (85, done in `075d6f9146`), nested `ghost_` warning (80),
      ghost hint (81), unused refined binding hint (21), real bindings used
      only in ghost code (44), refined-result helper diagnostic (63),
      `./dev --promote` exit status (25, done), verifier ignores
      `module B = Base` (37), `[@def]` partial application order (50),
      polymorphic `let` outside ghost code (62).

## Deferred by decision

Automatic laws and quantifiers (16, 55); automatic unfolding (53, 54); one
function for proving and running (48); termination for stateful code (88).

## Decisions waiting for the owner

- **`[@def]` lemmas are generated and trusted, not re-proved by the
  verifier** (found by the typer group's Codex review). Every change to the
  lemma generator rests on the generator being right. Proposed: add a
  re-proof of each generated lemma as a safety net (small to medium).

- **Item 6**: reading the four route pages is yours.
- **Subsumption open questions** (DESIGN.md §9), answered with defaults so
  the implementation can proceed; to confirm when comparing it with the
  design: keep a side table of sites with a fallback; stage 3 checks types,
  not implementation bodies; recursive modules and packs stay syntactic;
  invariant positions stay syntactic; functor applications in type paths
  stay syntactic; the proposed error wording; measure stage 2's fallout
  first; the mode condition uses position modes; `refine_` stays as it is.
