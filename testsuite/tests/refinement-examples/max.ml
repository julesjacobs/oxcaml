(* TEST
 modules = "vox_spec.ml";
 setup-ocamlc.byte-build-env;
 module = "vox_spec.ml";
 ocamlc.byte;
 module = "";
 flags += " -I ${test_build_directory}/ocamlc.byte";
 expect;
*)

(* The larger of two machine integers is at least both of them, and nothing
   here can overflow, so the claim holds as written.

   The predicate used to go through the prelude wrapper [Vox_spec.int_ge].
   That wrapper is total now that direct integer comparisons are, but the
   verifier still has no interpretation for it: it is an ordinary function,
   so the obligation said nothing about an order and could not be discharged.
   Written with the comparison the verifier models, the example says what it
   means. *)

(* @ex id=max_upper_bound final=ACCEPT today=ACCEPT stable=yes *)
let max (x : int) (y : int) =
  (if x >= y then x else y : int{ _ >= x && _ >= y })

[%%expect {|
val max : (x : int) -> (y : int) -> int{ _ >= x && _ >= y } = <fun>
|}]
