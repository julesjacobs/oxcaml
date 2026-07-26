(* TEST
 modules = "vox_spec.ml";
 setup-ocamlc.byte-build-env;
 module = "vox_spec.ml";
 ocamlc.byte;
 module = "";
 flags += " -I ${test_build_directory}/ocamlc.byte";
 expect;
*)

(* Negating a machine integer is not enough to make it nonnegative: the
   minimum has no positive counterpart and negating it returns it unchanged.
   The example says so, returning the maximum there, and the claim then holds
   of every input.

   The predicate used to go through the prelude wrapper [Vox_spec.int_ge],
   which the verifier has no interpretation for; it is written here with the
   comparison the verifier models. *)

(* @ex id=abs_nonnegative final=ACCEPT today=ACCEPT stable=yes *)
let abs (x : int) =
  (if x >= 0 then x else if x > min_int then 0 - x else max_int
    : int{ _ >= 0 })

[%%expect {|
val abs : int -> int{ _ >= 0 } = <fun>
|}]
