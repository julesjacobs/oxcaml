(* TEST
 has-z3;
 setup-ocamlc.opt-build-env;
 flags = "-extension refinement_types -i";
 module = "talk_fib_def_interface.ml";
 ocamlc.opt;
 check-ocamlc.opt-output;
*)

(* Talk, scene 21 ("definitions"): what [ocamlc -i] prints for the [@def]
   definition of [fib]. The definition is a copy of the one in
   bigint_fibonacci.ml, compiled here without that file's module signature,
   so that the generated [fib_def] is part of the printed interface. Its type
   is the definition's equation. The printer writes the literals 0Z and 1Z
   as [Bigint.of_int 0] and [Bigint.of_int 1], and the operators of [Bigint]
   with their module path. [zero_one] uses [fib_def] and is accepted. *)

open Bigint

let[@def] rec fib n =
  if n <= 0Z then 0Z
  else if n = 1Z then 1Z
  else fib (n - 1Z) + fib (n - 2Z)
[@@decreases n]

let (zero_one @ total) () : {u : unit | fib 0Z = 0Z && fib 1Z = 1Z} =
  fib_def 0Z; fib_def 1Z
