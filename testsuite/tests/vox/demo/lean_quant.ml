(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Demo: quantifiers in predicates.  [forall_ x. p], [exists_ x. p],
   and implication [p -> q] (sugar for [not p || q]); binders are
   untyped like the rest of the predicate language, and Lean infers
   their sorts.  Every obligation below is really proved, with no
   intro forms anywhere: obligations arise implicitly at annotations.

   The automation envelope, measured against grind:
   - a [forall_] GOAL is reliable: grind introduces the binder and the
     rest is quantifier-free reasoning;
   - an [exists_] FACT is reliable: grind skolemizes it, and the
     witness's facts flow;
   - an [exists_] GOAL is provable when the predicate leads with a
     witness equation ([exists_ y. y = 3 && ...]) whose right-hand
     side grind can evaluate; a bare existential goal generally is
     not (fails closed);
   - instantiating a [forall_] FACT is where grind is weakest: its
     E-matching is keyed on syntactic patterns that linear-arithmetic
     normalization easily perturbs.  Heavy quantified reasoning --
     e.g. a sortedness hypothesis instantiated at discovered indices
     -- still belongs in prelude lemmas ([%%vox.lean]), where the
     [@[grind]] annotations control the patterns. *)

(* A forall_ GOAL, with implication: max2 is not just an upper bound
   but the LEAST one.  Each branch proves the quantified conclusion
   under its path fact. *)
let max2
  : (x : int) -> (y : int)
    -> int{ x <= _ && y <= _ && (forall_ z. x <= z && y <= z -> _ <= z) }
  =
  fun x y -> if x < y then y else x

(* An exists_ FACT: the evenness of [x] arrives existentially
   (parameters are contracts: the predicate is a fact about [x] in the
   body).  grind skolemizes the witness, and the parity conclusion is
   then quantifier-free. *)
let even_not_odd
  : (x : int{ exists_ y. _ = 2 * y }) -> (z : int)
    -> unit{ not (x = 2 * z + 1) }
  =
  fun x z -> ()

(* An exists_ GOAL, by the witness-equation idiom: leading with
   [y = 3] hands grind the witness; the remaining conjunct is ground
   arithmetic. *)
let six_even : unit{ exists_ y. y = 3 && 6 = 2 * y } = ()

(* Binder sorts come from Lean's inference, so a formula whose atoms
   involve only binders is stuck at polymorphic operators
   ([forall_ i j. i + j = j + i] has nothing pinning Int); an integer
   literal anywhere in the formula grounds it.  Quantified truths then
   travel like any other refinement: proved once, re-proved trivially
   at an alpha-variant annotation (rigid types compare quantifiers
   under the binder pairing, so this is the SAME type). *)
let comm : unit{ forall_ i j. i + j - (j + i) = 0 } = ()
let comm2 : unit{ forall_ a b. a + b - (b + a) = 0 } = comm
