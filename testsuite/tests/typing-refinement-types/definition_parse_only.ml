(* TEST
 flags = "-extension refinement_types -stop-after parsing";
 { setup-ocamlc.byte-build-env; ocamlc.byte; }
 { setup-ocamlopt.byte-build-env; ocamlopt.byte; }
*)

let[@def] f x = x + 1
