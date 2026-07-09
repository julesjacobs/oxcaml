(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/bignum.mli ../lib/bignum.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* ONE ARITHMETIC SPEC, TWO MACHINES.  bignum.mli is bignum's binary
   implementation behind the SAME sealed interface as lib/peano.mli --
   the [lnat] ghost naming [Nat], the same [zero]/[succ]/[add] specs.
   [add_comm] and [five] below are the SAME client body as
   lean_peano_seal.ml: because the client sees only [Nat] arithmetic, it
   verifies against the binary machine with no change (the representation
   -- unary there, little-endian binary here -- is invisible). *)

open Bignum

(* Commutativity through the abstraction, by LINEAR ARITHMETIC (identical
   to lean_peano_seal.ml). *)
let add_comm : (a : t) -> (b : t) -> t{ _ = b + a } =
  fun a b -> add a b

(* Concrete computation 2 + 3 = 5 (identical to lean_peano_seal.ml). *)
let five : unit -> t{ _ = 5 } =
  fun () ->
    let z = zero in
    let one = succ z in
    let two = succ one in
    let three = succ two in
    add two three

(* MODEL equality through the abstraction: [add zero x] denotes [x], so
   [equal (add zero x) x] is provably [true] -- [equal] compares the [Nat]
   denotations, not representations (binary [add] may add trailing bits
   that [equal] sees through). *)
let equal_add_zero : (x : t) -> bool{ _ = true } =
  fun x ->
    let z = zero in
    let y = add z x in
    equal y x
