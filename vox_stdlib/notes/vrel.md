# Vrel — language-needs notes

Vrel is the **capstone higher-order module**: relations as dependent parameters
of function type (`(r : (int -> int -> bool))`), supplied at a CALL SITE as an
OCaml lambda (task #68 reflection) or a named `[@@vox.reflect]` value, and
combinators (`iter`/`map`/`fold`/`filter`/`compose2`) specified by RELATING the
result to the input through the relation. The callback `f` is never modeled;
only its per-element contract — the relation — which is passed WHOLE to the
block lifting defs (`rHolds`/`relIter`/`listRel`/`allP`), never applied
directly. Every combinator verified; both negative controls fail closed. The
frictions below are the price, in the §5 format.

### Vrel · dependent function-type binder must be parenthesised
- **site:** vox_stdlib/Vrel.mli:96 (`(r : (int -> int -> bool))`; also :102/:111/:118)
- **milestone/gap:** new (S_arrow surface syntax)
- **what I tried:** the natural unparenthesised binder
  `(r : int -> int -> bool) -> ...`.
- **error:** parse error — the dependent-binder grammar accepts only an ATOMIC
  inner type after the `:`, so a bare arrow type does not parse; the arrow must
  be wrapped `(r : (int -> int -> bool))`. (Study doc D1; the #68 demo carries
  the same note.)
- **workaround used:** parenthesise every function-typed dependent binder.
- **removed by:** extend the dependent-binder grammar to accept a full type
  (arrow-typed binder) after the `:`, not just an atomic one.
- **severity:** MINOR (one pair of parens per binder, but a real surface wart
  on the exact feature the module exists to demonstrate).

### Vrel · relation type aliases must be `abbrev`, not `def`
- **site:** vox_stdlib/Vrel.mli:34 (`public abbrev IntRel := Int -> Int -> Prop`)
- **milestone/gap:** new (reducibility across the import boundary)
- **what I tried:** `public def IntRel := Int -> Int -> Prop` (a plain def, the
  first instinct for a named type).
- **error:** Lean "application type mismatch" at the client — the S_arrow binder
  `(r : (int -> int -> bool))` emits as a bare `Int -> Int -> Prop` in the VC,
  and across the import boundary an OPAQUE `def IntRel` does not unfold to unify
  that arrow against the `IntRel`-typed imported defs (`rHolds` etc.).
- **workaround used:** declare `IntRel`/`IntPred` as `abbrev` (reducible), so
  unification unfolds them transparently on both sides of the import.
- **removed by:** either make imported `def`s reducible-on-demand during
  unification, or have the S_arrow emitter reuse the alias symbol rather than
  the expanded arrow.
- **severity:** MINOR (one keyword, but silent and only bites cross-unit — an
  in-file `def` would have "worked" and hidden the trap).

### Vrel · lifting defs must be `expose`d for the client to reduce them
- **site:** vox_stdlib/Vrel.mli:41–74 (`@[grind, expose]` on every lifting def)
- **milestone/gap:** model-dup (adjacent)
- **what I tried:** ship the lifting/algebra defs as plain `public def`s
  (visible name, opaque body) as the opaque-oset house rule would prefer.
- **error:** client goals like `rHolds (fun p q => ..) a b` and
  `relIter r n x y` do not close — grind cannot beta-reduce the substituted
  lambda / unfold `relIterN` against a concrete or symbolic relation without
  the body.
- **workaround used:** `@[grind, expose]` on `rHolds`/`pHolds`/`rcomp`/`rand`/
  `ror`/`rconverse`/`relIterN`/`relIter`/`listRel`/`il_len`/`allP`. These are
  legitimately load-bearing to EXPOSE (unlike the opaque-oset ops): the client's
  whole reasoning is unfolding them at the substituted relation, so the
  expose-kills-laws test passes — each is exercised live by clients/smoke_vrel.ml.
- **removed by:** n/a — this is the correct shape for a lifting theory the
  client reasons *through*. Noted as the contrast to the opaque-op house rule.
- **severity:** COSMETIC.

### Vrel · a `>=`-conclusion length invariant needs an explicit grind_pattern
- **site:** vox_stdlib/Vrel.ml:44 (`il_len_nonneg`), :46 (its `grind_pattern`)
- **milestone/gap:** M3
- **what I tried:** rely on `@[grind] theorem il_len_nonneg (l) : il_len l >= 0`
  to fire wherever `il_len l` appears in fold/iter fuel reasoning.
- **error:** without a pattern, grind never instantiates the nonneg fact at the
  fold's list-length argument, so `relIter r (il_len u) a` cannot bridge to
  `relIterN r (il_len u).toNat` (the toNat_succ guard `1 <= il_len u` is never
  established). Contract VC "NOT PROVED".
- **workaround used:** `grind_pattern il_len_nonneg => il_len l` — the M3
  `>=`-conclusion trigger finding: a bound variable absent from the conclusion's
  head needs a multi/term trigger on the application term.
- **removed by:** grind heuristics that auto-instantiate a monotone/nonneg fact
  at every occurrence of its subject term.
- **severity:** MINOR.

### Vrel · toNat bridges must be PUBLIC so a client folding a concrete list reduces the fuel
- **site:** vox_stdlib/Vrel.mli:65–67 (`public theorem toNat_nonpos`/`toNat_succ`);
  mirrored internal at vox_stdlib/Vrel.ml:31–33
- **milestone/gap:** M3
- **what I tried:** keep the two `toNat` bridge lemmas INTERNAL to the `.ml`
  (they discharge the symbolic-fuel recursion in `iter`/`fold`), and let the
  client's `fold` goal over a concrete list evaluate `(il_len l).toNat` on its own.
- **error:** clients/smoke_vrel.ml:41 `fold_le` — "NOT PROVED", goal
  `a <= *unknown*` under `relIter (fun p q -> p<=q) (il_len l) a *unknown*`,
  `l = Icons (b, Inil)`. Without the bridges in the imported VoxSig, grind will
  not reduce `(il_len l).toNat` (congruence to a literal `1` then `Int.toNat`)
  and cannot unfold `relIterN`. A LITERAL count (`iter .. 3`) does NOT hit this —
  grind computes `(3 : Int).toNat` directly — so only the fold-over-list client
  exposes it.
- **workaround used:** re-declare both bridges as `@[grind] public theorem` in
  the `.mli` block (proved-theorem-in-interface, self-contained arithmetic), so
  they ride the VoxSig olean to the client. Not obligations (theorems, not
  axioms), so the `.ml` keeps its own copies for its symbolic proofs.
- **removed by:** grind computing `(e).toNat` from a known `e = <lit>` fact
  without a user lemma, i.e. an `Int.toNat`-of-nonneg simp/grind normal form.
- **severity:** MINOR.

### Vrel · constructor application straight into a dependent list parameter (C1)
- **site:** clients/smoke_vrel.ml:42 (`let l = Vrel.Icons (b, Vrel.Inil) in`)
- **milestone/gap:** C1 (named-call-result injection)
- **what I tried:** inline the list argument at the call site:
  `Vrel.fold r f a (Vrel.Icons (b, Vrel.Inil))`.
- **error:** the inline constructor application flowing straight into the
  dependent `xs` parameter (whose type feeds `il_len xs` in the postcondition)
  did not thread its structure to the goal; the C1 `*unknownN*` placeholder for
  the argument leaves `il_len l` un-evaluable.
- **workaround used:** `let`-bind the constructor value first
  (`let l = Icons (b, Inil) in ... fold .. l`), so the binder carries the
  equation `l = Icons (b, Inil)` into the VC (the C1 house pattern).
- **removed by:** binding call arguments at the skeleton the way transparent-via
  let-binders already do, so an inline constructor into a dependent param keeps
  its structural equation.
- **severity:** MINOR.

### Vrel · an UNAPPLIED named predicate over a symbolic structure must be a reducible `abbrev`
- **site:** clients/smoke_vrel.ml:14 (`abbrev isPos`), :48 (`allP isPos _`)
- **milestone/gap:** new (the higher-order analogue of the IntRel-abbrev finding)
- **what I tried:** name the filter goal's predicate as a `def`
  (`@[grind, expose] def isPos : Int -> Prop := fun x => x > 0`) — a lambda may
  not appear in refinement text, so the client MUST use a named symbol — and let
  the postcondition `allP isPos result` close against filter's contract
  `allP (fun x -> x > 0) result` (the reflected call-site lambda).
- **error:** clients/smoke_vrel.ml:48 `filter_pos` — "NOT PROVED", goal
  `allP isPos *unknown*`, hyp `allP (fun x -> x > 0) *unknown*`. Confirmed in
  standalone Lean: grind normalises the lambda body (`x > 0` → `-1*x+1 ≤ 0`) but
  does NOT unfold a `def isPos` where it appears UNAPPLIED as `allP isPos l` over
  a symbolic `l` — the `isPos.eq_1` ematch pattern only fires on APPLIED
  `isPos _`, and allP cannot reduce on a symbolic list. `unfold isPos; grind`
  and a function-level `isPos = (fun x => x>0) := rfl` fact both close it, but a
  client cannot inject a tactic into the generated `by grind`.
- **workaround used:** declare the predicate as `@[grind, expose] abbrev isPos`
  (reducible). grind then normalises `isPos` to its body EVERYWHERE, including
  unapplied, so `allP isPos l` and `allP (fun x => x>0) l` become the same node
  by congruence. This is the exact IntRel-abbrev lesson, now for a client
  predicate held over a whole structure rather than a relation applied per-step.
- **removed by:** grind unfolding an exposed `def` at unapplied argument
  positions (a function-level eq lemma, `f.eq_def`, registered for congruence),
  OR a `[@@vox.reflect]` binding usable on a plain OCaml `let` (today it is
  silently ignored on a `let` — the value reflects as an opaque atom, not the
  paired symbol — so a client cannot name a non-primitive predicate like
  `x > 0` and have it denote a shared Lean symbol; only `val`/`external` carry
  the attribute, and there is no unary `int -> bool` reflection primitive).
- **severity:** MAJOR-ERGONOMIC (the ONLY way to consume filter's
  `allP p _` postcondition with a named predicate is the abbrev trick; a `def`
  silently fails, and the `[@@vox.reflect]`-on-`let` path that would give a
  checked/named alternative does not exist).

### Vrel · model defs authored in both blocks (model-dup)
- **site:** vox_stdlib/Vrel.mli:34–90 vs vox_stdlib/Vrel.ml:12–55
- **milestone/gap:** model-dup
- **what I tried:** author the relation algebra + lifting defs once.
- **error:** n/a — the `.ml` seal re-elaborates against its own block, so every
  `def`/`abbrev` (IntRel, IntPred, rHolds, pHolds, rcomp, rand, ror, rconverse,
  relIterN, relIter, listRel, il_len, allP) plus the two toNat bridges is
  restated verbatim in the `.ml` (the `.mli` copies are `public`, the `.ml`
  copies are not). ~14 defs duplicated.
- **workaround used:** duplicate; keep the two blocks textually in sync.
- **removed by:** a `.mli`-block import into the `.ml` seal so shared model defs
  are written once.
- **severity:** MINOR-ERGONOMIC (mechanical, but the largest such duplication in
  the stdlib so far — the whole relation theory).

### Vrel · the one shipped obligation, typed twice (M1)
- **site:** vox_stdlib/Vrel.mli:88 (`axiom listRel_len`) vs vox_stdlib/Vrel.ml:52
  (`theorem listRel_len`)
- **milestone/gap:** M1
- **what I tried:** state the length-preservation law (`listRel r a b ->
  il_len a = il_len b`) once.
- **error:** n/a — the interface `axiom` is the obligation; the `.ml` seal
  demands a same-named proved `theorem`, so the statement + its `grind_pattern`
  are typed verbatim in both files (1 obligation).
- **workaround used:** duplicate statement; `.ml` proof
  `induction a generalizing b <;> cases b <;> grind`. Verified load-bearing:
  deleting it leaves `.mli`/`.ml` green but breaks the client `map_len` goal
  (`_ = il_len xs`), which is exactly the consequence a map client names without
  mentioning the relation.
- **removed by:** obligation statements shared between `.mli` and `.ml` (write
  once, prove once).
- **severity:** MINOR.
