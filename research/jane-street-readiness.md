# Jane Street readiness checklist

What remains before the Vox demos are shown to the Jane Street compiler
team. Update this file as items land: tick the box and add the commit.
Decisions and their reasons are in
`research/warts-investigation-20260927/DECISIONS.md` (outside the repo, in
the Vox research directory); the catalogue is `verification/catalogue`.

## Status on 28 September

Trunk is `jujacobs/vox/trunk-20260926` at `eb42cf5245` (= `personal/vox`).
The site is live at https://lab.julesjacobs.com/vox/ with five parts: the
landing page, the catalogue, the playground, the source explorer and the
talk. Every soundness bug in the ledger is fixed on trunk. What remains
before the pitch:
- the owner rehearses the talk with the deck's practice mode;
- the owner reads the four tour pages (flat hash table, Myers diff, one-shot
  channels, SAT) and the trust page (item 6);
- the upstream OxCaml bug reports (item 14), which the owner wants to
  understand first;
- in progress: the proof cleanup from warning 227 (branch
  `jujacobs/vox/proof-cleanup-20260928`) and the x86 hash-table benchmark on
  the AMD box (item 10);
- deferred until after the pitch: the last route of the hidden-types
  totality bug (an abstract type equal to a function type), the effects of
  borrowing in the mode system and the totality of mutable code, and the
  other items under "Deferred by decision".

The open items under "Should have" and "Nice to have" are listed below with
their state.

## How this was worked (27–28 September)

The four waves planned on 27 September have all merged: the soundness fixes,
the verifier, solver, typer and tooling fixes, the second review of every
page, subsumption, the presentation work, and the fixes found by the mastery
investigation.

Testing policy (27 September): a branch merges when its agent's own run
passes and every affected test passes (`./dev test --affected <base>`). The
full suite then runs asynchronously on the AMD box after merges, batched; a
red result means a fix-up or a revert. `vox` is pushed only at commits whose
full suite passed. Documentation-only commits do not need a suite run.

Push policy: `vox` is pushed only after both the single-domain suite (Mac or
AMD) and, for changes touching the runtime, the concurrency libraries or the
parallel demos, the multidomain check pass.

No GitHub CI (owner's decision, 27 September): the fork's upstream workflows
need Jane Street's paid runners and never run. The full suite is our own run
on the AMD box (`~/vox-check-run.sh` on the `incoming` branch there).

Multidomain runs: our other builds are configured without
`--enable-multidomain`, so the six tests marked `multicore` (one-shot
parallel, channel buffer, raw memory, borrow, the two lock tests) are skipped
and `parallel_sort_array` runs sequentially. The AMD clone
`~/git/vox-multidomain` is configured with `--enable-multidomain
--enable-poll-insertion` (configure refuses the first without the second).
Run the suite there with each batched full run, and before merging changes to
the concurrency libraries, `Borrow` or the runtime. From the Mac, in the
worktree to test:

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

Catalogue statuses: every demo page had two independent reviews that found
no remaining false claim, and each page states its gaps. The published pages
carry no review-status labels (owner's decision, 28 September;
`acf428e647`). Decisions that need the owner are collected at the end of
this file.

## Must have

- [x] **1. Soundness.** The soundness ledger
      (`research/soundness-ledger-20260927/LEDGER.md`, Vox research
      directory) lists 33 bugs in the current generation; all are fixed on
      trunk as of `f236636ecd`. One route of T9 (hidden types) is open and
      deferred until after the pitch.
  - [x] Ghost-field ownership fix, merged in `3f313103a3` (checker fix
        `ec365027eb`). No demo took a token twice; the demos that broke relied
        on the construction-side hole (repairs in `3b2a3047e5`, `13f99dd368`).
  - [x] W8: type parameters used only in refinement predicates are
        invariant (merged in `4e24de3229`). The routes through type
        abbreviations and re-exported signatures closed with W1 (predicates
        compared with their types, `0a9bab592b`).
  - [x] W9: the caches are keyed by the compiler digest and solver version
        (`4e24de3229`); the query cache is keyed by the solver alone and the
        version probe has a timeout (`04f5daf1e7`). The probe allows 30 s, so
        a loaded machine does not lose the version (`896e20c9d4`).
  - [x] **Totality knot** (found 27 September by the mastery investigation;
        fixed by the hidden-types rule below, merged in `09dfd7e2fa`): a
        total function stored in a record whose domain is an existential
        package, unpacked back to the record's type by a GADT witness,
        diverged; `false` was then proved and `ghost_` erased the call
        (`research/mastery-20260927/scratch/history-limits/knot.ml` printed
        "verified to be 1: 2"). Talk test:
        `talk_soundness_totality_knot.ml`.
  - [x] **Effectful callbacks broke call congruence** (found 27 September;
        merged in `0a76bf52f1`): a call to a total, stateless function was
        encoded as a function of its arguments even when a callback argument
        was partial and stateful (`apply tick 0 = apply tick 0` proved, false
        at run time). Calls are now congruent only at total, stateless
        arguments.
  - [x] **Hidden types in the totality check** (merged in `09dfd7e2fa`): the
        knot above, plus an abstract type in a signature hiding negative
        recursion. One rule covers existentials, abstract and open types and
        unpacked modules: they are treated as possibly recursive unless the
        jkind excludes functions or a recorded guarantee holds. One route is
        open: an abstract type equal to a function type, consumed by an
        exported total function. A declared matchability attribute was
        prototyped on `jujacobs/vox/matchability-20260928`; it is not merged
        and is deferred until after the pitch.
  - [x] **Ghost-field reads crossed locality** (found 27 September; merged in
        `a0b99ddfeb`, commits `f409ea963e`, `9300e9c1fe`, `f05afd93e9`): a
        token borrowed for a read escaped its `borrow_` region through a
        ghost field ("verified to be 1: 2"). Fixed with record kinds that
        count ghost fields and a one-field ghost layout.
  - [x] **Out-of-range shifts** (found 27 September by the mastery
        investigation; merged in `cca8ba889a`): one uninterpreted function
        per shift operator made `(1 lsl n) = (1 lsl 64)` provable for
        n = 64, false in native code. Every shift in verified code needs a
        count in [0, 63] (`cbca48d5d5`), the standard shifts are partial and
        `Int.Refined` has total ones, and a shift may be declared `@@ total`
        only with that refinement (`fc1e8cdc5e`, a route Codex found). Caches
        are keyed by the solver's platform (`b267ebcecd`). Notes in
        `research/shift-encoding-20260927` (Vox research directory).
  - [x] **Name-keyed built-ins** (B3): fixed by the trust hardening below
        (`a187b9ab9c`).
  - [x] **Refinement subsumption and W1**: predicates are compared with
        their node types, at inclusion and coercion (merged in `8c2e40a562`
        and `0a9bab592b`; design in `research/subsumption-design-20260927`).
        The merge introduced U3, a one-sided treatment of elimination
        wrappers; fixed in `e2ff6c5c28` (the verifier derives the facts
        behind the wrappers itself, and predicate equality ignores them),
        merged in `49971c310a`.
- [x] **2. Second review round** (brief:
      `research/demo-review-20260926/ROUND2-BRIEF.md`, Claude and Codex per
      demo). Every demo page has had its second review and corrections.
  - [x] flat-hash-table, one-shot-channels (merged `103a45730c`; the
        rejected table example lacked the proof that made its false claim
        the only reason for failure, fixed; the Cmm excerpt regenerated from
        the current build on x86-64). The page keeps its full interface
        (owner's decision). Open: the Cmm stamps go stale on compiler changes
        and nothing checks the excerpt. The public client uses an integer key
        with a real hash (`01a86f5103`, merged in `f236636ecd`); before, its
        hash was constant 0.
  - [x] regex-automata, hindley-milner, binary-search, constant-folding,
        online-union-find (merged `8de1969188`, `99bf4013c7`). HM: all
        eight unreachable arms are `unreachable_ ()`; regex: the
        open-question section corrected (iarray bound reason, `lower`
        provably `None` for u ≥ 63 in the logic).
  - [x] mode-solver, register-allocation, rsa (merged `f55601c65d`;
        regalloc's "native code drops 93 lemmas" was false: 92 lemmas, each
        a placeholder function in both backends).
  - [x] functional-queue, merge-sort, quicksort, sparse-arrays (merged
        `bf28fa14ea`; merge-sort and quicksort each had a false "no test
        rejects …" claim).
  - [x] avl-sets, rings, lists-trees (merged `217d986873`; rings and lists
        had stale quoted messages; the structures erasure check strengthened
        to an allow-list).
  - [x] myers-diff, http, lz4, egraphs (merged `35e8e3cc07` and
        `99010e1cf7`). LZ4's explanation of `assert false` was false (it
        discharges its branch; the client now uses `unreachable_ ()`); the
        e-graphs page states that the search is exhaustive and applies one
        rewrite per round; it does not do e-matching.
  - [x] dfa-equivalence, reference-locks, sat-solver (merged `11439178cf`,
        `b231c28d78`, `94dbe6f9fb`). Locks: the hidden-atomic fixtures named
        modules that no longer existed after deduplication, so they rejected
        vacuously; fixed. SAT: ~750 lines of dead proof code removed,
        including a non-resolution `Exhaustion` rule; new "How it is proved"
        section. Erasure checks now see every Lambda apply form
        (`Emitted_code.direct_calls`).
  - [x] hm-wasm-compiler (merged `f8e0ab7091`; two reproduce scripts,
        including the AMD box's 100k differential, had silently selected no
        modules since the switch to `prebuilt_modules`; fixed to fail on an
        empty list).
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
- [ ] **6. The owner reads the four tour pages end to end** (flat hash
      table, Myers diff, one-shot channels, SAT) **and the trust page**.
- [x] **Public site** at https://lab.julesjacobs.com/vox/ (merged in
      `bf290a8249`): landing page, catalogue, playground, source explorer and
      talk.
  - [x] Playground build fixed: the browser solver reports its Z3 version
        (merged in `c98d22489f`).
  - [x] Source explorer at `/vox/source/`: treemap, descriptions and tours
        of all 25 demos (merged in `6a5eb1bc25`; content updated after the
        subsumption merge in `ce84810bda`).
  - [x] Talk deck at `/vox/talk/`, with a practice mode and a presenter view
        (`c110ac7bb8`). The introduction film is retired (`839c215d1b`).
  - [x] Review-status labels removed from the published catalogue pages
        (`acf428e647`).
- [x] **Tests the talk quotes** (merged in `f236636ecd`): each slide's claim
      has a test (manifest `3cec777a08`), including `talk_totality_bogus.ml`
      (why totality), `talk_fib_def_interface.ml` (`fib_def`'s printed type)
      and `talk_atomic_nested.ml` (opening a nested invariant).
- [ ] **The owner rehearses the talk** with the deck's practice mode.

## Should have

- [x] **7. Code presentation of the shown demos** (presentation merged
      `9a6bc6ab5d`, deduplication merged `70d651c363`): comments in large files
      (AVL, e-graphs, the compiler); hand-written interfaces for HTTP, the
      mode solver and the SAT spec in place of compiler-printed ones; removed
      duplicated code (two DFA pipelines, near-copy lock modules, SAT solver
      variants).
- [x] **8. Remaining weak contracts** (fixed, or stated on the page):
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
- [x] **9. Diagnostics and proof noise.**
  - [x] Name the failing conjunct of an `&&` goal (wart 14, `10d8f2bcd8`).
  - [x] Erasure lints (warts 41, 42): warning 223 for a total function with
        a ghost result whose body is not `ghost_`, warning 224 for real code
        discarding a ghost result (merged in `81c853024a`).
  - [x] Remove the stale workarounds now unnecessary: about 555 lines net in
        15 demos, merged in `4c74143232`. The client files the pages quote no
        longer use `refine_` (`9a6bc6ab5d`). `let u = () in` is still common
        in proof files, most of all in the HM proofs and `regex_core.ml`.
- [ ] **10. Performance story.**
  - [ ] Flat hash table benchmark on x86-64. *In progress on the AMD box.*
  - [x] State on the compiler page that the backend manages its own stack
        in memory (for honest exhaustion) and is not an optimizing backend.

## Nice to have

- [ ] **11. Medium warts:** ghost ownership inside `ghost_` (12, 45, 46,
      52), layouts (10, 11, 13), well-formedness obligations for partial
      operations (67). Done: termination measures (77) and bitwise solver
      fallback (34); named predicates (20) as `[@def transparent]`, with the
      flat hash table's precondition named `current table view` (merged
      `0a37492c3d`). Inductive relations (56) are not a language feature:
      the owner chose the hand-written derivation pattern
      (`testsuite/tests/vox/relations.ml`) and listed inductive definitions
      as a future mechanism in the catalogue.
- [x] **12. Subsumption stages 1–4**, implemented as one piece from a
      complete design (merged in `8c2e40a562`, then `0a9bab592b` with W1).
      `IMPLEMENTATION.md` §7 lists the deviations from the independent design;
      the owner accepted deviation 1 (modes pass only to tuple components)
      and deviation 2 (predicate node types compared by skeleton). U3, found
      after the merge, is fixed (`49971c310a`).
- [x] **13. Prebuilt test library** (wart 33), `./dev test --affected` and
      `VOX_TEST_TIMEOUT` (merged in `6390fe236b`): the full suite takes about
      3 minutes warm and 6.5 after a compiler change, from 43 and 60. The
      full suite was made green on the merged trunk (`d1aa7e44a0`).
- [x] **Unused proof steps from unsat cores** (owner's request, 27
      September): warning 227, off by default, reports lemma calls and
      argument or path assumptions that no proof in the function needed,
      found from Z3 unsat cores (merged in `9bb8b90d43`).
- [ ] **Proof cleanup from warning 227** (owner: yes): remove the ~900
      reported unused lemma calls, keeping the two RSA budget helpers and
      intentional API-exercising calls in test clients; re-verify everything.
      *In progress on `jujacobs/vox/proof-cleanup-20260928`.*
- [ ] **Warning 226 precision** (owner's decision, 27 September): keep 226
      on by default; fix the check rather than suppressing. (a) An
      application is erasable only if its arguments' required modes survive
      capture by `ghost_` (immutable, aliased); (b) an undetermined mode is
      not capturable; (c) report all layers at once: a candidate is
      proof-only if every use is ghost or inside another proof-only
      candidate's definition (fixpoint). Then remove the two
      `[@warning "-226"]` in `maps.ml` and `dependent_expressions.ml`, which
      are still there. Subsumption has merged, so this can start.
- [x] **Soundness ledger** for the talk (owner: list all soundness bugs
      briefly, including the early ones):
      `research/soundness-ledger-20260927/LEDGER.md`. 33 rows for the current
      generation, all fixed on trunk; one route of T9 open and deferred.
- [x] **E-graph comments in the 13 inventory-locked files**, regenerating
      `vox_egraph_rule_handle.spec.json` (owner: yes): a header comment in
      each, before the first declaration; the inventory changes only in
      hashes and line numbers (`d7e234bcfa`, merged in `50824fe942`).
- [x] **Browser playground** (owner's request): `verification/playground/`,
      merged `354f2c5dfd`; front end and verifier via js_of_ocaml, Z3 4.16.0
      as Wasm; 0 disagreements with native on 265 inputs; clickable
      locations. Hosted at `/vox/playground/` with COOP/COEP headers.
- [ ] **Hand-written mode-solver semantics interfaces** (the three `.mli`
      printed by the compiler, primed names), like the public one.
- [ ] **`-principal` rejections** of verified code: the RSA library
      (`vox_rsa_fermat.ml:150`, "ys is partial but expected total"),
      unannotated parameters used in predicates (labelled-args report),
      polymorphic `[@def] rec` helpers under `ghost_` (presentation report).
      *Diagnosed 27 September* (`research/warts-investigation-20260927/new-limitations.md`):
      two causes. (1) RSA and the `[@def] rec` case are one Vox bug that
      also occurs without `-principal`: typing a predicate that mentions
      `h a` changes `h`'s own inferred modes, so a later, unrelated `ghost_`
      use of `h`'s result is rejected (default-mode repro with
      `let (dup @ total) x = [x]`). Fix in the typer. (2) Unannotated
      parameters: principal mode rightly refuses mode crossing on a type
      learned by unification, and Vox predicates need crossing (variables
      at `immutable`, functions expecting `read_write`). Fix: no
      `read_write` requirement in erased predicates. The `node option`
      and sparse-arrays rejections are the upstream kind limitation
      (ticket 5111), nothing to fix in Vox.
- [x] **Multidomain test runs**: `~/git/vox-multidomain` on the AMD box
      (see the testing policy; merged in `92a5fa0748`). All 7 parallel tests
      pass there, with the rest of the suite; stress runs found no failure or
      hang. The one-shot, reference-lock and quicksort pages name both
      configure flags.
- [x] **`_trust.md`** (corrected with the owner's approval; `6316145367`,
      `b15c25e0ff`, `ab26aaf9de`): it no longer conflates totality and
      statelessness (total functions may write through uniquely owned
      storage, e.g. `Borrow.Slice.set`), and it says that bytecode keeps an
      empty slot for ghost fields while native code removes them. The owner
      reads it before the pitch (item 6).
- [ ] **`assert false` lint**: it ends a path with nothing to prove, which
      is sound for normal-return claims (and rejected in `total` code), but
      reviews found several demos relying on it silently. Warn in verified
      code and suggest `unreachable_ ()`.
- [ ] **Typer bugs found by the `-principal` investigation** (see
      `research/expect-principal-20260927/repro/`; subsumption has merged, so
      these can start): (b2, default mode too) typing a predicate that
      mentions `h a` narrows `h`'s ungeneralized top-level modes, so a later
      unrelated use fails ("ys is partial but is expected to be total");
      (b1, `-principal`) predicates use variables at `immutable` but `>=`
      etc. expect `read_write`: do not require `read_write` in erased
      predicates; (b3, cosmetic) refinement carriers print through the alias
      the predicate names.
- [x] **Trusted base hardening** (merged `a187b9ab9c`; from the mastery
      audit): built-in meanings keyed by declaration identity instead of the
      C primitive name (a user `external` naming `caml_bigint_add` got
      addition semantics while native subtracted); warnings 228 and 229 for
      refined or total externals and `trust_total` casts outside the library;
      `-vox-audit` listing trusted items transitively; units compiled with
      `-smt-assume-verified` are marked; the solver version is enforced; the
      four heap laws that extensionality now gives are proved; fewer
      `trust_total` casts.
- [ ] **14. Upstream OxCaml reports:** the owner wants to understand and be
      convinced first. Both suspects were reproduced on upstream OxCaml
      (`research/upstream-bugs-20260927/REPORT.md`). The expect tool
      overwriting single blocks with principal output is a real bug, fixed
      upstream (`9dc1ddfb2f`, #6806); the fix is cherry-picked
      (`ba068ca404`, nine tests promoted to two blocks in `e0a5031b11`) and
      merged in `0e798b0a8c`. The `node option` kind error is a known
      upstream limitation of `-principal`, internal ticket 5111. Neither
      needs a new upstream report unless the owner decides otherwise.
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

Stated on the pages and left as they are before the pitch (decided 27
September): the HTTP laws for `Invalid_crlf` and budget exhaustion;
e-matching in the e-graph demo; erased lemmas still compile to placeholder
functions in both backends (stripping exported lemma fields would be a
compiler change).

The last route of the hidden-types totality bug (T9): an abstract type equal
to a function type, consumed by an exported total function. The matchability
attribute prototyped on `jujacobs/vox/matchability-20260928` is not merged;
this waits until after the pitch.

**Effects in the mode system (owner, 28 September).**
- **The principle:** a `total` function is a mathematical function.
  Unique-in/unique-out code and heap tokens fit it, read as functional
  threading: path copying, or a threaded map.
- **What does not fit:** borrowing, read the RustHorn way, is an effect.
  Creating a borrow chooses its prophecy nondeterministically, and `finish`
  assumes `final = current`, which kills the other worlds.
- **Before 28 September:** `finish` and borrow creation were typed `total`.
  Erasing a borrowing call was prevented only because `ghost_` captures real
  values as aliased.
- **Implemented 28 September** (merged in `eb42cf5245`): prophecy creation
  (`with_mut`, `Slice.split_at`, `split3`, `with_range`) and `finish` are
  partial, in `Borrow` and `Borrow_iarray`. All the quicksorts (`Quicksort`,
  `Quicksort_iarray`) borrow and are partial; the owner does not need a total
  quicksort, since the demo exists to show borrowing. A borrow-free total
  `sort_array` was tried on the branch and reverted. `borrow_partial.ml`
  checks the verdicts.
- **Future work:** model these effects with finer-grained modes, and with
  them the totality of mutable code. Then the in-place sort can state that
  it terminates, with an argument better than "every operation it uses is
  total".

## Decisions waiting for the owner

Waiting:
- **Item 6**: reading the four tour pages and the trust page.
- **Item 14**: whether to file anything upstream, once the owner has read
  the report.
- Rehearsing the talk with the practice mode.

Decided:
- *Decided 28 September:* the talk is presented live by the owner. There is
  no ElevenLabs narration and no MP4. It ends on future directions, with no
  concrete proposal. Authorship is not narrated; reviewing AI-written code
  through its specifications is one of the core motivations.
- *Decided 28 September:* the published catalogue pages carry no review
  labels.
- *Decided 28 September:* a `total` function is a mathematical function.
  Borrowing is effectful (RustHorn), so borrow creation and `finish` are
  partial; the totality of mutable code is future work (see "Deferred by
  decision").
- *Decided 27 September:* `[@def]` lemmas are not re-proved. The generated
  equation is the definition mechanism itself (how the verifier knows an
  identifier equals its right-hand side), so there is nothing separate to
  prove.
- *Decided 27 September:* no stated limitation becomes proof work before
  the pitch (HTTP CRLF/budget laws, e-matching, erased-lemma stubs stay as
  stated).
- *Decided 27 September, updated 28 September:* host everything at
  lab.julesjacobs.com/vox/: the landing page, catalogue, playground, source
  explorer and talk. The introduction film is retired; the talk deck
  replaces it. The Caddy route has COOP/COEP headers for /vox/playground/
  (backup `/srv/lab/Caddyfile.pre-vox`).
- *Confirmed by the owner, 27 September:* **Subsumption open questions**
  (DESIGN.md §9), answered with defaults: keep a side table of sites with a
  fallback; stage 3 checks types and does not look at implementation
  bodies; recursive modules and packs stay syntactic; invariant positions
  stay syntactic; functor applications in type paths stay syntactic; the
  proposed error wording; measure stage 2's fallout first; the mode
  condition uses position modes; `refine_` stays as it is.
