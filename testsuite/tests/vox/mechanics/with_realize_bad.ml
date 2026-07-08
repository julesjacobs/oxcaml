(* TEST
 flags = "-vox-dry-run";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* vox FM-1 guard: an abstract Lean ghost sort carrying block axioms may
   NOT be realized at a concrete type by a [with type] constraint -- that
   would substitute the abstract sort for a concrete one with NO discharge
   of the axioms.  The sound realization route is functor instantiation.
   A sort-PRESERVING [with type] (to another type refining the same sort)
   is still allowed. *)

type mykey [@@vox.sort lean "MyKey"]

module type ORD = sig
  type t : value refines (mykey)
  val compare : (x : t) -> (y : t) -> int
end

(* rejected: realizes MyKey at the concrete int with no discharge *)
module type ORD_int = ORD with type t = int
