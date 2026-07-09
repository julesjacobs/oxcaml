(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "functor_binder_fresh_lib.mli functor_binder_fresh_lib.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* D4 / capture-class C3 regression: functor application substitutes the result
   signature under the [Nothing] action, which (pre-fix) did NOT freshen the
   dependent-arrow binders (a, b in [leq]'s result contract).  Two applications
   of the same functor then SHARED binder stamps; a later arrow-pairing could
   cross them (the F-1 collision class, at [Make] instead of at .cmi import).
   [Subst.for_functor_application] now freshens the binders, so each
   instantiation's contract binds its own stamps.

   Canary: two instantiations, each result contract's binder-fact used in a
   DISTINGUISHING obligation ([from_a] needs [not (q<p)], [from_b] the swapped
   [not (p<q)]).  If the binders crossed, one obligation would take the other's
   fact and fail.  Verifies green post-fix. *)

module IntOrd = struct
  type t = int
  let compare : (x:int) -> (y:int)
    -> int{ (_ < 0 -> x < y) && (_ = 0 -> x = y) && (_ > 0 -> y < x) } =
    fun x y -> if x < y then -1 else if x = y then 0 else 1
end

module A = Functor_binder_fresh_lib.Make (IntOrd)
module B = Functor_binder_fresh_lib.Make (IntOrd)

let from_a (p : int) (q : int) : bool{ _ = true -> not (q < p) } = A.leq p q
let from_b (p : int) (q : int) : bool{ _ = true -> not (p < q) } = B.leq q p
