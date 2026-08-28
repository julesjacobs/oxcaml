(* TEST
 has-z3;
 flags = "-extension refinement_types -rectypes";
 timeout = "30";
 { setup-ocamlc.byte-build-env; ocamlc.byte; }
 { setup-ocamlopt.byte-build-env; ocamlopt.byte; }
*)

type endless = int -> endless

let apply (f : endless @ total) = f 0

let trivial x : {n : int | true} = refine_ x
