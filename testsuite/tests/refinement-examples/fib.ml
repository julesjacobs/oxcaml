(* TEST
 modules = "vox_spec.ml";
 setup-ocamlc.byte-build-env;
 module = "vox_spec.ml";
 ocamlc.byte;
 module = "";
 flags += " -I ${test_build_directory}/ocamlc.byte";
 expect;
*)

(* Fibonacci is non-negative of every non-negative argument, and that is a
   fact about the mathematical integers: over machine integers the sum
   overflows and wraps negative long before the recursion ends, so the claim
   would be false.  It is written over [Bigint.t], whose arithmetic has the
   same unbounded meaning at run time as it does in the proof.

   Each recursive call's own result contract is what the proof reads as its
   induction hypothesis, and the argument contract is what keeps the calls
   within the domain. *)

(* @ex id=fib_nonnegative final=ACCEPT today=ACCEPT stable=yes *)
let rec fib (n : Bigint.t{ Bigint.ge _ Bigint.zero })
    : Bigint.t{ Bigint.ge _ Bigint.zero }
  =
  if Bigint.le n Bigint.zero
  then Bigint.zero
  else if Bigint.equal n Bigint.one
  then Bigint.one
  else
    Bigint.add
      (fib (Bigint.sub n Bigint.one))
      (fib (Bigint.sub n (Bigint.of_int 2)))

[%%expect {|
val fib :
  Bigint.t{ Bigint.ge _ Bigint.zero } -> Bigint.t{ Bigint.ge _ Bigint.zero } =
  <fun>
|}]
