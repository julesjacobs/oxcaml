(* TEST
 expect;
*)

(* NEGATIVE / EXPECTED-FAILURE test tracking the sibling-reference boundary gap
   (VOX2_SIBLING_BOUNDARY_TODO).

   The persistence stage rewrites all four reference-head representations under
   Subst, but a predicate that references a same-signature sibling value does
   not yet survive signature equality/inclusion end-to-end.  Two things are
   still missing, both OUTSIDE the persistence fence and assigned to the primary
   lane's next stage:
     - lowering produces Rglobal(Pident ...) for a sibling reference (sibling-
       head production is future work; fail-closed until then); and
     - the includemod value-pairing arm (typing/includemod.ml Sig_value) adds
       nothing to the pairing substitution, so the two signatures' value stamps
       are never aligned and the heads compare on raw stamps.

   The functor identity below therefore fails: the parameter and result copies
   of [S] carry different stamps for [base], so [g]'s predicate heads differ.

   When the sibling-reference fix lands, THIS expect block must flip to
   acceptance (the functor is well typed and no error is printed).  Do not
   silence it before then. *)

module type S = sig
  val base : int
  val g : int{ _ = base }
end

module F (X : S) : S = X
[%%expect {|
Line 1:
Error: Module type declarations do not match:
         module type S =
           sig
             val base : int
             val g : int{ (app[Stdlib!.=] _ global[base/289]) }
           end
       does not match
         module type S =
           sig
             val base : int
             val g : int{ (app[Stdlib!.=] _ global[base/293]) }
           end
       At position "module type S = <here>"
       Module types do not match:
         sig
           val base : int
           val g : int{ (app[Stdlib!.=] _ global[base/289]) }
         end
       is not equal to
         sig
           val base : int
           val g : int{ (app[Stdlib!.=] _ global[base/296]) }
         end
       At position "module type S = <here>"
       Values do not match:
         val g : int{ (app[Stdlib!.=] _ global[base/289]) }
       is not included in
         val g : int{ (app[Stdlib!.=] _ global[base/293]) }
       The type "int{ (app[Stdlib!.=] _ global[base/289]) }"
       is not compatible with the type
         "int{ (app[Stdlib!.=] _ global[base/293]) }"
|}]
