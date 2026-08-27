(* TEST
 has-z3;
 flags = "-extension refinement_types -dvc -dsmtlib";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

let trivial x : {n : int | true} = refine_ x
