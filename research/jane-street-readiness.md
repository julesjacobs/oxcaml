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

Catalogue statuses: **Reviewed** means checked by the owner. **Ready for owner
review** means two independent reviews found no false claim and the page
states every gap. Decisions that need the owner are collected at the end of
this file.

## Must have

- [ ] **1. Soundness.**
  - [ ] Merge the ghost-field ownership fix (branch
        `jujacobs/vox/ghostfield-20260927`; checker fix `ec365027eb`),
        with the list of demo proofs that relied on the bug.
  - [ ] Compare refinement predicates with their types (subsumption
        stage 2; design in `research/subsumption-design-20260927`).
- [ ] **2. Second review round** (brief:
      `research/demo-review-20260926/ROUND2-BRIEF.md`, Claude and Codex per
      demo), after item 1 merges; correct each page until **Reviewed**.
  - [ ] binary-search · constant-folding · dfa-equivalence · egraphs ·
        flat-hash-table · hindley-milner · http · lz4 · mode-solver ·
        one-shot-channels · online-union-find · reference-locks ·
        regex-automata (its lowering gap stays, stated as an open question)
  - [ ] hm-wasm-compiler, after item 3.
- [ ] **3. HM-to-Wasm is not trivial** (AMD box).
  - [ ] Example programs compiled by the verified compiler and run, with
        exact results and resource use, as a suite test (branch
        `jujacobs/vox/hmc-resources-20260927`).
  - [ ] Input as a run-time value through the exported `payload` global;
        theorems for every input (branch `jujacobs/vox/hmc-input-20260927`).
  - [ ] WebAssembly model tested against Node by random modules (branch
        `jujacobs/vox/wasm-diff-20260927`).
- [ ] **4. Every page claim checked by `./dev test`.**
  - [ ] Port the 13 hand-run check scripts into the suite (wart 26).
  - [ ] E-graphs: review the public boundary, regenerate
        `vox_egraph_rule_handle.spec.json`, put the check in the suite.
- [ ] **5. Bugs an expert could hit live.**
  - [ ] `include M` with a refinement on an included value: escape error,
        and a `Refinement_scope_escape` crash with `let rec`.
  - [ ] `ocamlc -i` drops `@@ total`.
  - [ ] Spurious escape error with the dependent-parameter sugar.
  - [ ] `let rec` with a refined parameter: type error with identical types.
  - [ ] `assert false` counted as returning; `assert e` not assumed after.
  - [ ] `max_int`, `min_int` and other primitives unknown to the solver.
- [ ] **6. A person reads the four route pages end to end:** flat hash
      table, Myers diff, one-shot channels, SAT.

## Should have

- [ ] **7. Code presentation of the shown demos:** comments in large files
      (AVL, e-graphs, the compiler); hand-written interfaces instead of
      compiler-printed ones (HTTP, mode solver, SAT spec); remove duplicated
      code (two DFA pipelines, near-copy lock modules, SAT solver variants).
- [ ] **8. Remaining weak contracts** (fixed, or stated on the page):
  - [ ] LZ4 `decompress` contract, via labelled arguments in dependent
        types (wart 38).
  - [ ] HTTP: malformed request and header lines must be rejected.
  - [ ] Rings: insert and remove in the `Owned` interface.
  - [ ] Lists/trees: lemma relating `contents`, `nodes` and `List.rev`.
- [ ] **9. Diagnostics and proof noise.**
  - [ ] Name the failing conjunct of an `&&` goal (wart 14).
  - [ ] Erasure lints: total function with a ghost result whose body is not
        `ghost_`; real code discarding a ghost result (warts 41, 42).
  - [ ] Remove the stale workarounds now unnecessary (about 160 lines;
        `logic-automation.md`, items 58–62).
- [ ] **10. Performance story.**
  - [ ] Flat hash table benchmark on x86-64 (AMD box).
  - [ ] State on the compiler page that the backend manages its own stack
        in memory (for honest exhaustion) and is not an optimizing backend.

## Nice to have

- [ ] **11. Medium warts:** ghost ownership inside `ghost_` (12, 45, 46,
      52), layouts (10, 11, 13), inductive relations (56), named predicates
      (20), termination measures (77), bitwise solver fallback (34),
      well-formedness obligations for partial operations (67).
- [ ] **12. Subsumption stages 1–4**, implemented as one piece from a
      complete design (branch `jujacobs/vox/subsumption-20260927`, AMD box).
      Afterwards, compare the implementation with the independent design in
      `research/subsumption-design-20260927/DESIGN.md` and its tests, and
      report divergences before merging.
- [ ] **13. Prebuilt test library** (wart 33).
- [ ] **14. Upstream OxCaml reports:** the expect tool overwrites single
      blocks with principal output; the `node option` kind error.
- [ ] **Small warts** (group 3 in the conversation of 27 September):
      ghost field of a ghost record (9), implicit `'a` in predicates (1),
      constructor terms at any type (4), `[@def]` on constants (5), lemma
      premise assumed (57), top-level `@ total` in expect tests (79),
      comparator key distinctness (65), total division (71),
      `Bigint.to_int_opt` (69), iarray length bound (68), heap
      extensionality (74), atomics (73), table modules at `-O3` (19),
      `binary_modules` documentation (85), nested `ghost_` warning (80),
      ghost hint (81), unused refined binding hint (21), real bindings used
      only in ghost code (44), refined-result helper diagnostic (63),
      `./dev --promote` exit status (25), verifier ignores
      `module B = Base` (37), `[@def]` partial application order (50),
      polymorphic `let` outside ghost code (62).

## Deferred by decision

Automatic laws and quantifiers (16, 55); automatic unfolding (53, 54); one
function for proving and running (48); termination for stateful code (88).

## Decisions waiting for the owner

(none yet)
