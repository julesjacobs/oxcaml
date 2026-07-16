(* TEST
 readonly_files = "sibling_ref.mli";
 setup-ocamlc.byte-build-env;
 module = "sibling_ref.mli";
 ocamlc.byte;
 module = "sibling_boundary.ml";
 ocamlc.byte;
*)

(* Positive regression test for the sibling-reference boundary.

   A predicate that references a same-signature sibling value
   ([val g : int{ _ = base }]) lowers to [Rglobal (Pident base)].  Signature
   equality ([module type of]) and inclusion each independently rename the
   value component, so the two copies used to carry different stamps for
   [base] and the predicate heads failed to match.  The fix pairs [Sig_value]
   identifiers in [includemod]'s component pairing (shared by the inclusion
   and module-type-equality paths).  Every declaration below must COMPILE;
   before the fix each failed with a "values do not match" stamp mismatch.

   [sibling_ref.mli] compiling at all covers the functor-signature variant
   ([module Make : functor (...) -> T], where [T]'s predicate names a sibling
   of the functor result). *)

(* [module type of] exercises the module-type-equality path; the re-ascription
   exercises the inclusion path. *)
module type D = module type of Sibling_ref

module Reexport : D = Sibling_ref

(* Functor identity over a sibling-referencing signature (inclusion). *)
module type S = sig
  val base : int
  val g : int{ _ = base }
end

module F (X : S) : S = X
