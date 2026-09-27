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

Multidomain runs: our other builds are configured without
`--enable-multidomain`, so the seven tests marked `multicore` (one-shot
parallel, channel buffer, raw memory, borrow, quicksort frame, the two lock
tests) are skipped and `parallel_sort` runs sequentially. The AMD clone
`~/git/vox-multidomain` is configured with `--enable-multidomain
--enable-poll-insertion` (configure refuses the first without the second).
Run the suite there with each batched full run, and before merging changes to
the concurrency libraries, `Borrow` or the runtime. From the Mac, in the
worktree to test:
Push policy: `vox` is pushed only after both the single-domain suite
(Mac or AMD) and, for changes touching the runtime, the concurrency
libraries or the parallel demos, the multidomain check pass.

```
amd=jules@jules-b650-aorus-elite-ax-v2
base=$(ssh $amd git -C git/vox-multidomain rev-parse HEAD)
git bundle create /tmp/vox-md.bundle HEAD ^$base
scp /tmp/vox-md.bundle $amd:vox-multidomain-incoming.bundle
ssh $amd "nohup ./vox-multidomain-check.sh $(git rev-parse HEAD) \
  > vox-multidomain-check.log 2>&1 &"
ssh $amd cat vox-multidomain-check.log    # when it has finished
```

The script builds incrementally, runs `./dev test vox`, prints the counts and
exits nonzero if a test fails or is skipped (`Domain.recommended_domain_count
()` below 2 would skip them). First run (27 September, `8527a3fb7b`): 464
passed, none skipped; 6.5 minutes with a cold library after a 12-minute
initial build, 4.5 minutes for a documentation-only commit (script run on
`a6838a87e0`). The parallel tests and the quicksort clients also
passed 200 runs each, native and bytecode, with a 4k-word minor heap and on
two CPUs (`~/vox-multidomain-stress.sh`).

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
  - [ ] **Totality knot (found 27 September by the mastery investigation):**
        a total function stored in a record whose domain is an existential
        package, unpacked back to the record's type by a GADT witness,
        diverges; `false` is then proved and `ghost_` erases the call
        (`research/mastery-20260927/scratch/history-limits/knot.ml` prints
        "verified to be 1: 2"). Agent on
        `jujacobs/vox/totality-knot-20260927`: map every route, principled
        fix, Codex review.
  - [ ] **Effectful callbacks break call congruence (found 27 September):**
        a call to a total, stateless function is encoded as a function of its
        arguments even when a callback argument is partial and stateful
        (`apply tick 0 = apply tick 0` proved, false at run time). Agent on
        `jujacobs/vox/call-congruence-20260927`.
  - [ ] **Hidden types in the totality check:** the knot above, plus an
        abstract type in a signature hiding negative recursion. One rule for
        existentials, abstract/open types and unpacked modules (possibly
        recursive unless the jkind excludes functions or a recorded guarantee
        holds). Agent on `jujacobs/vox/totality-existentials-20260927`.
  - [ ] **Ghost-field reads cross locality (found 27 September):** a token
        borrowed for a read escapes its `borrow_` region through a ghost
        field ("verified to be 1: 2"). Agent on
        `jujacobs/vox/ghost-field-locality-20260927`.
  - [ ] **Out-of-range shift counts (found 27 September):** one
        uninterpreted function per shift operator equates results that
        differ between constant-folded and run-time code; a verified program
        returns false where true was proved, and segfaults through an
        unchecked string read. Agent on `jujacobs/vox/shift-encoding-20260927`
        (in-range count as an obligation; audit other unspecified
        operators; platform in the cache key).
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
  - [x] avl-sets, rings, lists-trees: **owner-review** (merged
        `217d986873`; rings and lists had stale quoted messages; the
        structures erasure check strengthened to an allow-list).
  - [x] myers-diff, http, lz4, egraphs: **owner-review** (merged
        `35e8e3cc07`, egraphs after). LZ4's explanation of `assert false`
        was false (it discharges its branch; the client now uses
        `unreachable_ ()`); e-graphs now states the search is exhaustive,
        not e-matching, one rewrite per round.
  - [x] dfa-equivalence, reference-locks, sat-solver: **owner-review**
        (merged `11439178cf`, `b231c28d78`). Locks: the hidden-atomic
        fixtures named modules that no longer existed after deduplication,
        so they rejected vacuously; fixed. SAT: ~750 lines of dead proof
        code removed, including a non-resolution `Exhaustion` rule; new
        "How it is proved" section. Erasure checks now see every Lambda
        apply form (`Emitted_code.direct_calls`).
  - All 24 demo pages except hm-wasm-compiler are at owner-review.
  - [ ] Also in flight: egraphs | sat-solver | dfa-equivalence ·
        reference-locks (after deduplication and the inventory comments).
  - [x] hm-wasm-compiler: **owner-review** (review merged with this
        commit; two reproduce scripts, including the AMD box's 100k
        differential, had silently selected no modules since the switch
        to `prebuilt_modules`; fixed to fail on an empty list).
- [x] **3. HM-to-Wasm is not trivial** (AMD box).
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
  - [x] State on the compiler page that the backend manages its own stack
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
      *Diagnosed 27 September* (branch `jujacobs/vox/expect-principal-20260927`,
      `research/warts-investigation-20260927/new-limitations.md`): two
      causes. (1) RSA and the `[@def] rec` case are one Vox bug that also
      occurs without `-principal`: typing a predicate that mentions `h a`
      changes `h`'s own inferred modes, so a later, unrelated `ghost_` use
      of `h`'s result is rejected (default-mode repro with
      `let (dup @ total) x = [x]`). Fix in the typer. (2) Unannotated
      parameters: principal mode rightly refuses mode crossing on a type
      learned by unification, and Vox predicates need crossing (variables
      at `immutable`, functions expecting `read_write`). Fix: no
      `read_write` requirement in erased predicates. The `node option`
      and sparse-arrays rejections are the upstream kind limitation
      (ticket 5111), nothing to fix in Vox.
- [ ] **Erased lemmas still compile to placeholder functions** (both
      backends; stated on the pages). Stripping exported lemma fields would
      be a compiler change; decide whether it matters for the pitch.
- [x] **Multidomain test runs**: `~/git/vox-multidomain` on the AMD box
      (see the testing policy). All 7 parallel tests pass there, with the
      rest of the suite; stress runs found no failure or hang. Open: the
      one-shot and reference-lock pages name only `--enable-multidomain`,
      which configure rejects without `--enable-poll-insertion`.
- [x] **`_trust.md` line 13** (fixed with the owner's approval; page back
      to owner-review) conflates totality and
      statelessness: total functions may write through uniquely owned
      storage (`Quicksort.sort`). Wording proposed to the owner.
      Also: it says ghost fields are removed before code generation, but
      bytecode keeps an empty slot for them (native removes them).
- [ ] **`assert false` lint**: it ends a path with nothing to prove, which
      is sound for normal-return claims (and rejected in `total` code), but
      reviews found several demos relying on it silently. Warn in verified
      code and suggest `unreachable_ ()`.
- [ ] **HTTP laws for `Invalid_crlf` and budget exhaustion** (new proofs;
      stated as unspecified on the page). Owner to decide.
- [ ] **Typer bugs found by the `-principal` investigation** (after
      subsumption merges; see `research/expect-principal-20260927/repro/`):
      (b2, default mode too) typing a predicate that mentions `h a` narrows
      `h`'s ungeneralized top-level modes, so a later unrelated use fails
      ("ys is partial but is expected to be total"); (b1, `-principal`)
      predicates use variables at `immutable` but `>=` etc. expect
      `read_write`: do not require `read_write` in erased predicates;
      (b3, cosmetic) refinement carriers print through the alias the
      predicate names.
- [ ] **Trusted base hardening** (from the mastery audit; agent on
      `jujacobs/vox/trust-hardening-20260927`): built-in meanings keyed by
      declaration identity, not C primitive name (a user `external` naming
      `caml_bigint_add` got addition semantics while native subtracted);
      warning for refined/total externals and `trust_total` casts outside the
      library; `-vox-audit` listing trusted items transitively; mark units
      compiled with `-smt-assume-verified`; enforce the solver version; prove
      the four heap laws extensionality now gives; shrink `trust_total`;
      corrected `_trust.md` draft for owner review.
- [ ] **14. Upstream OxCaml reports:** the owner wants to understand and be
      convinced first; an agent is reproducing both on upstream OxCaml
      (`research/upstream-bugs-20260927/REPORT.md`). Previously listed: the expect tool overwrites single
      blocks with principal output; the `node option` kind error.
      *27 September:* the expect-tool bug is fixed upstream (`9dc1ddfb2f`,
      #6806); cherry-picked as `ba068ca404` on
      `jujacobs/vox/expect-principal-20260927`, with nine tests promoted to
      two blocks (`e0a5031b11`); merge into the trunk. The kind error is
      upstream ticket 5111; no report needed.
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

- *Decided 27 September:* `[@def]` lemmas are not re-proved. The generated
  equation is the definition mechanism itself (how the verifier knows an
  identifier equals its right-hand side), so there is nothing separate to
  prove.
- *Decided 27 September:* no stated limitation becomes proof work before
  the pitch (HTTP CRLF/budget laws, e-matching, erased-lemma stubs stay as
  stated).
- *Decided 27 September:* host everything at lab.julesjacobs.com/vox/
  (landing page, catalogue, playground, film). Caddy route with COOP/COEP
  for /vox/playground/ added (backup `/srv/lab/Caddyfile.pre-vox`).

- **Item 6**: reading the four route pages is yours.
- *Confirmed by the owner, 27 September:* **Subsumption open questions**
  (DESIGN.md §9), answered with defaults: keep a side table of sites with a fallback; stage 3 checks types,
  not implementation bodies; recursive modules and packs stay syntactic;
  invariant positions stay syntactic; functor applications in type paths
  stay syntactic; the proposed error wording; measure stage 2's fallout
  first; the mode condition uses position modes; `refine_` stays as it is.
