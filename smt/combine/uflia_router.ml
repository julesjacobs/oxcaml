open Oxsmt_core

type owner =
  | A
  | B
  | Both

(* [owner] is the REGISTER fan-out (combine.mli: the union of children that receive the
   atom at register time), which is NOT the same as who asserts it. A LIA order atom
   ([Le_zero]) is [B] (LIA only): under internalization the combinator no longer registers
   it with EUF as a [Both]/[K_foreign] atom — instead, for any B-owned atom it calls
   [A.internalize_term] on the whole term, so EUF's e-graph still gains nodes for the
   atom's [App] subterms and congruence closes over them (the W1 fix — [f x],[f y] that
   occur ONLY inside a LIA atom must still become congruent under [x = y]). The register
   fan-out is therefore exactly ownership, and the EUF-visibility of a foreign atom moves
   to the combinator's internalize step. *)
let owner term =
  match Theory_view.atom term with
  | Theory_view.Le_zero _ -> B
  | Theory_view.Predicate _ -> A
  | Theory_view.Bool_lit _ -> A
  | Theory_view.Equality (x, _) ->
    (match x.Term.sort with
     | Sort.Int _ -> Both
     (* A datatype, array, or bitvector equality/disequality is decided by congruence (all
        are e-graph clients alongside EUF), so it routes to EUF like an uninterpreted-sort
        equality — no arithmetic arrangement. The bit-blasting engine supplies the bit-level
        semantics separately at the propositional layer. *)
     | Sort.Bool | Sort.Uninterpreted _ | Sort.Datatype _ | Sort.Array _ | Sort.BitVec _
       -> A)
;;

(* The ASSERT fan-out — a subset of [owner] (combine.mli). Two narrowings:
   - A [Le_zero] atom is registered with both children (for EUF's foreign congruence
     visibility, above) but ASSERTED only to LIA ([B]): EUF holds it as [K_foreign] and
     [Euf_adapter.assert_lit] on a foreign atom raises, so it must never be asserted
     there.
   - A NEGATIVE shared (Int) equality routes to EUF only (codex S1): LIA raises
     [Unsupported] on a disequality (lia.mli). Sound (EUF handles diseq natively). The
     intended completeness net is that if LIA's candidate model later equates the pair, the
     shared-pair disagreement splits into the ℤ-trichotomy, whose [<]/[>] branches carry the
     ordering to LIA. That net is INCOMPLETE for a variable-vs-CONSTANT disequality
     ([x <> c], the nec-smt ITE-condition shape): [Combine.find_disagreement] ranges over the
     Int-sorted, both-valued INTERFACE members and misses [x <> c] because the variable is
     EUF-only-used (not a both-used interface member) and the constant is not an interface
     node, so LIA never hears such a disequality and its model may set [x = c] — a spurious candidate R1
     then rejects (→ unknown; task #30, logs/nec-probe-report.md). The flag
     [OXSMT_LIA_MODEL_REPAIR] (combine.ml [repair_split]) closes the gap by scanning the
     negatively-pinned pairs LIA's model equates at Final and emitting the same ℤ-trichotomy;
     default-ON since task #59, forced OFF by [OXSMT_LIA_MODEL_REPAIR=0] (this narrowing
     stays incomplete-but-sound as before). Every other atom/polarity asserts as [owner]. *)
let assert_to term ~positive =
  match Theory_view.atom term with
  | Theory_view.Le_zero _ -> B
  | Theory_view.Predicate _ | Theory_view.Bool_lit _ | Theory_view.Equality _ ->
    (match owner term with
     | Both when not positive -> A
     | o -> o)
;;

(* The ℤ-trichotomy: three DISTINCT atoms whose disjunction is valid over the integers, so
   the SAT core retains the clause (a [x=y ∨ x≠y] pair would be dropped as [A ∨ ¬A]).

   INVARIANT (QF_UFLIA shared-sort): [x] and [y] are always Int-sorted here. The
   combinator only ever splits a pair its disagreement search returns, and that search
   filters to Int-sorted terms both children's models value (combine.ml
   [find_disagreement]) — so the split pair is always Int-sorted and the trichotomy is
   well-formed. This rests on LIA's language being Int-only: the sole way a term becomes
   shared with LIA is by appearing in a LIA atom ([Le_zero] / Int [Eq]), whose subterms
   are Int-sorted; an uninterpreted-sorted term is private to EUF (a convex theory, which
   propagates its own entailed equalities internally), so no cross-theory sort-U equality
   split arises. The guard below turns any violation of that invariant into a loud
   exception rather than a confusing [Term.Sort_error] deep in [Context]; escaping, it
   degrades the query to [unknown] via CONTRACT-POISON (sound — never a wrong verdict). It
   raises the SAME {!Combine.Combination_unsound} the combinator uses for every other
   degrade-to-unknown path, so the poison surface is one exception, not two (S10). *)
let equality_split ctx x y =
  (match x.Term.sort, y.Term.sort with
   | Sort.Int _, Sort.Int _ -> ()
   | _ ->
     raise
       (Combine.Combination_unsound
          "Uflia_router.equality_split: non-Int shared term (unreachable in QF_UFLIA — \
           LIA is Int-only, so every shared term is Int-sorted)"));
  [ Context.eq ctx x y; Context.lt ctx x y; Context.gt ctx x y ]
;;
