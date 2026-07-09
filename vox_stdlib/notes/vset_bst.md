# Vset_bst — language-needs notes

Module: exposed-ADT sorted-BST set backend (wave 1). Ops: `empty`, `member`,
`insert`, `remove`. Shipped laws (all .mli obligations, obligation form):
`bok_insert`, `bmem_insert`, `bok_delete`, `bmem_delete`. Private .ml
scaffolding: `bnot_mem_lt/gt`, `ball_lt/gt_insert`, `ball_lt/gt_mono`,
`bmem_join`, `ball_lt/gt_join`, `bok_join`, `ball_lt/gt_delete`. Sealed green
with the real solver; zero trust.

### Vset_bst · model theory authored in both blocks
- **site:** vox_stdlib/Vset_bst.ml:24 (block) mirrors vox_stdlib/Vset_bst.mli:22 (block)
- **milestone/gap:** model-dup
- **what I tried:** author `bmem`/`ball_lt`/`ball_gt`/`bok`/`bins`/`bjoin`/`bdel`
  once (in the .mli, `public`) and have the .ml see them from the interface.
- **error:** none at this site — but the .ml block must RESTATE all seven model
  defs verbatim (without `public`) or its theorems cannot mention them; the
  seal re-elaborates the interface against the .ml's own copies.
- **workaround used:** the house model-duplication tax (§4): seven defs copied
  into the .ml block, dropping `public`.
- **removed by:** an "implementation sees its own interface's block defs"
  mechanism (the .ml imports the VoxSig defs instead of restating them).
- **severity:** MAJOR-ERGONOMIC

### Vset_bst · client-facing law statement typed twice
- **site:** vox_stdlib/Vset_bst.mli:36-42 (4 axioms) and vox_stdlib/Vset_bst.ml (4 theorems)
- **milestone/gap:** M1
- **what I tried:** state each client-facing law (`bok_insert`, `bmem_insert`,
  `bok_delete`, `bmem_delete`) once and prove it once.
- **error:** none — but the obligation pattern requires the FULL statement
  (name, binders, `grind_pattern`) written identically in the .mli (as `public
  axiom`) and the .ml (as the discharging `theorem`). 4 laws → 4 verbatim
  duplications, each with its own duplicated `grind_pattern`.
- **workaround used:** obligation form as mandated (§4); statements + patterns
  kept byte-identical across the two blocks.
- **removed by:** a prove-only export form (a `.mli` marker that a same-named
  `.ml` theorem satisfies without re-typing the statement).
- **severity:** MAJOR-ERGONOMIC

### Vset_bst · call result into a dependent parameter (smoke client)
- **site:** vox_stdlib/clients/smoke_vset_bst.ml (`member x (insert x s)` etc.)
- **milestone/gap:** C1 (named-call-result injection)
- **what I tried:** `member x (insert x s)` / `member x (remove x s)` — pass a
  call result directly as the `set`-typed dependent argument of `member`.
- **error:** `vox: the argument for a dependent parameter must be a variable or
  a pure expression the logic can name (let-bind it first)`
- **workaround used:** `let s' = insert x s in member x s'` — let-bind the call
  result to a variable, then pass the variable (same pattern as the wave-2
  `smoke_vset.ml`).
- **removed by:** letting the logic name an intermediate call result itself
  (auto let-insertion for pure call arguments feeding a dependent parameter).
- **severity:** MINOR

### Vset_bst · two-child BST delete needs a pivot-parameterized join lemma + multi-trigger
- **site:** vox_stdlib/Vset_bst.ml (bjoin, bok_join, ball_lt/gt_mono, ball_lt/gt_join)
- **milestone/gap:** new (proof-engineering friction, the real work here)
- **what I tried:** `bdel` handles the two-child case by merging the two
  subtrees with `bjoin` at the removed pivot. The natural hope was that
  `theorem bok_delete ... := by induction t <;> grind` would close on its own.
- **error:** it does NOT, until the join's invariant-preservation is hand-proved
  and registered. Two structural obstacles: (1) the join preserves `bok` only
  under a pivot separation "all of l < b < all of r", so the lemma
  `bok_join (l r) (b) (bok l) (bok r) (ball_lt l b) (ball_gt r b) : bok (bjoin l r)`
  carries a bound `b` that does NOT appear in its conclusion `bok (bjoin l r)`;
  a single-term `grind_pattern => bok (bjoin l r)` leaves `b` free and grind
  cannot instantiate it. (2) closing `bok_join`'s own inductive step needs to
  slide `r`'s lower bound down from the join bound to the local pivot, i.e.
  monotonicity of `ball_gt`/`ball_lt`.
- **workaround used:** a 3-part multi-trigger
  `grind_pattern bok_join => bok (bjoin l r), ball_lt l b, ball_gt r b` (the
  `ball_lt l b`/`ball_gt r b` terms bind `b` from context), plus explicit
  `ball_lt_mono`/`ball_gt_mono` lemmas and `bmem_join`/`ball_lt_join`/
  `ball_gt_join`. Six private lemmas in total feed the one-line
  `induction t <;> grind` on `bok_delete`/`bmem_delete`.
- **removed by:** grind support for existential/context-bound trigger variables
  in a lemma conclusion (so a pivot bound need not be smuggled in via an
  auxiliary trigger term), and built-in monotonicity for the bounded-tree
  predicates. Absent those, every ordered-container delete/merge pays this tax.
- **severity:** MAJOR-ERGONOMIC

### Vset_bst · invariant law is not forced by an op's refined result type
- **site:** vox_stdlib/clients/smoke_vset_bst.ml (ok_after_insert / ok_after_remove)
- **milestone/gap:** new (dead-block-theorem hazard; cf. the §6.7 dead-law check)
- **what I tried:** to satisfy §6.7 I first forced `bok_insert`/`bok_delete` with
  goals like `let s1 = remove x s in let s2 = insert y s1 in member y s2` —
  expecting the nested op to require its argument be a proven `set`.
- **error:** those goals do NOT force the `bok_*` laws. `insert`/`remove` already
  declare a `set` result in the .mli, so a client is handed `bok` for free by
  the interface; and inside the module, `remove`'s own `set` result obligation
  is discharged by the per-site scaffolding (`ball_lt/gt_delete` + `bok_join`),
  not by the top-level `bok_delete`. A per-law removal sweep confirmed the
  module still SEALS with `bok_delete` deleted — it would have shipped as a
  silently dead obligation.
- **workaround used:** force the invariant law with a goal that demands
  `bok (bdel x s)` / `bok (bins x s)` for a SYMBOLIC `s`
  (`unit{ bok (bdel x s) }`): grind cannot induct on a variable, so it must use
  the shipped lemma. Sweep now shows deleting either `bok_*` law breaks its
  forcing goal.
- **removed by:** a compiler lint for block obligations/theorems that no VC or
  client goal actually consumes (backlog "lint for silently-dead block
  theorems") — it would have caught the mis-designed forcing goals mechanically
  instead of by a hand-run sweep.
- **severity:** MAJOR-ERGONOMIC

### Vset_bst · one-path tail-recursive member does NOT hit #32 (calibration)
- **site:** vox_stdlib/Vset_bst.ml (`if x<v then member x l else member x r`)
- **milestone/gap:** #32 (NON-occurrence — recalibration evidence, rev. 2)
- **what I tried:** the honest one-path search: branch on `x<v` and recurse into
  exactly one subtree, no intermediate bool binding.
- **error:** none. #32 (a refined-bool fact lost across a branch) bites only a
  **bind-then-branch on a spec'd bool** (`let b = go l in if b then …`), e.g. an
  OR-style membership over both subtrees. A tail-recursive one-path search never
  binds a bool to branch on, so it verifies with NO workaround.
- **workaround used:** none needed — filed only to evidence the rev.-2
  recalibration (do not file a spurious #32 note against this shape).
- **removed by:** n/a (non-occurrence; documents that the tail-recursive
  one-path member never hits #32)
- **severity:** COSMETIC

### Vset_bst · exposed representation is required (no #31 here)
- **site:** vox_stdlib/Vset_bst.mli (`type set = tree{ bok _ }`), vox_stdlib/Vset_bst.ml (`let l' = remove x l in Node (l', v, r)`, `let lr' = join lr r in ...`)
- **milestone/gap:** new (backend-tier design consequence)
- **what I tried:** thread the recursive result `l'`/`lr'` (a refined `set`/tree)
  through a `let` into a `Node` constructor, on a plain refined ADT — in both
  `insert`, `remove`, and the `join` helper.
- **error:** none. Unlike a via-abstract op (Vlist.append, the #31 site), a plain
  refined-ADT `set = tree{ bok _ }` keeps its predicate across the `let`: the
  skeleton IS the tree and grind re-derives the refinement from the structural
  equation. So insert/remove/join need no #31 skeleton-threading workaround.
  This is only possible because the repr is EXPOSED — the R7 probe shows a
  downstream unit cannot unpack a hidden repr, so the wave-2 `Vset` face must sit
  over this exposed backend.
- **workaround used:** none; exposed `tree`/`set` by design (backend tier, §4/§8).
  `bjoin` is public only because it appears in `bdel`'s exposed body.
- **removed by:** n/a (intended design, not a gap) — noted so Phase C does not
  misread the exposed repr as a hygiene violation.
- **severity:** COSMETIC
