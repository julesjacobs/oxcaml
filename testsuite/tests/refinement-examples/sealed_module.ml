(* TEST
 modules = "vox_spec.ml";
 setup-ocamlc.byte-build-env;
 module = "vox_spec.ml";
 ocamlc.byte;
 module = "";
 flags += " -I ${test_build_directory}/ocamlc.byte";
 expect;
*)

(* A square is non-negative, and that is a fact about the mathematical
   integers: a machine product overflows and can wrap negative, so over [int]
   the sealed signature would be claiming something false.  It is written over
   [Bigint.t], whose arithmetic has the same unbounded meaning at run time as
   it does in the proof.

   The implementation says the result is above minus one and the signature
   says it is not below zero.  Of the integers those say the same thing, but
   they are not the same predicate, so sealing the one under the other makes
   the compiler prove the implication, which is what this example is for.
   The two are written in the same shape, a comparison of the result against
   a constant, because a seal relates predicates by walking them together and
   two differently shaped predicates do not reach the implication at all.

   Neither result mentions the parameter, so neither side carries a dependent
   binder; an implementation result that mentioned it could not be sealed
   under a signature result that did not. *)

(* @ex id=seal_square_nonnegative final=ACCEPT today=ACCEPT stable=yes *)
module Square : sig
  val square :
    Bigint.t{ Bigint.ge _ Bigint.zero } -> Bigint.t{ Bigint.ge _ Bigint.zero }
end = struct
  let square (x : Bigint.t{ Bigint.ge _ Bigint.zero }) =
    (Bigint.mul x x
      : Bigint.t{ Bigint.gt _ (Bigint.sub Bigint.zero Bigint.one) })
end

[%%expect {|
module Square :
  sig
    val square :
      Bigint.t{ Bigint.ge _ Bigint.zero } ->
      Bigint.t{ Bigint.ge _ Bigint.zero }
  end
|}]
