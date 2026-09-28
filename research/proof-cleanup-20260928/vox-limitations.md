# Vox limitations met during the proof cleanup (28 September 2026)

Branch `jujacobs/vox/proof-cleanup-20260928`: removal of the lemma calls that
warning 227 reports as unused, and of `let u = () in` scaffolding.

## Warning 227

- **A report is not a safe removal.** Of 822 calls removed in the first
  round, 10 made verification slower and were put back:
  - `vox_rsa.ml`: without `lambda_def p q`, one query went from 0.2 million
    to 2.2 million resource units.
  - `hm_effective_complete.ml`: without six `_def` calls a batch no longer
    fit its budget and split (148 to 231 queries, +47%).
  - `table_model.ml`: without `Inv.reserve_def 16` the top-level query went
    from 0.25 million to 1.0 million units.
  - `vox_lz4_roundtrip.ml`: removing two calls in `snapshot_at` and
    `literal_heap_at` made queries in two later, unrelated functions of the
    same unit 25% and 51% slower. Queries are independent, so this is
    solver state carried between queries of one unit.

  Workaround: measure each unit before and after with `-dsmt-resources`.
  Suggested fix: the precise mode could also reject a step whose removal
  raises the query's resource count by more than a margin.
- **Cores are not stable.** A second collection after the first round
  reported 22 new unused calls, and a third collection 6 more; typically the
  second of two calls that give the same fact.
- **Cascades.** Removing a call can leave a local lemma, or a ghost binding
  such as `let h = ghost_ (Pref.own (borrow_ t)) in`, unused. Warning 26
  then fails the test. Local lemmas whose only use was the removed call
  were kept by keeping the call.
- **`Borrow.Slice.finish` is reported as an unused lemma call.** It is the
  loan's finish, an assumption, not a proof step. Suggested fix: do not
  report applications whose callee is partial or has effects.
- **Expect tests.** `VOX_UNUSED_STEPS` reports from expect tests have no
  file name; they were matched to files by line and call text.

## Scaffolding

- `refine_` accepts any expression, and a `{u : unit | P}` premise accepts
  `()`, so `let u = () in ... refine_ u` is no longer needed. 1,444 such
  bindings were rewritten with no change to any solver query. Bindings
  used by `assume_` stay (it needs a plain variable). About 95 bindings stay
  because the typed tree places a use of `u` at a location of generated
  code, which the rewrite could not map back to source. Expect tests
  (263 bindings) were left alone: most are rejection tests whose expected
  output quotes the code.

## Tooling

- `./dev test` on a subset regenerates the test library's `build.mk` with
  the flags of the selected tests only, so compiling other units against
  `_build/vox-test-library` afterwards fails with "inconsistent
  assumptions over interface Vox_sequence". A full `./dev test vox`
  restores it.
- Explorer anchors that end at `let u = () in u` broke when the scaffolding
  went; anchors on proof text are fragile under proof cleanups.
