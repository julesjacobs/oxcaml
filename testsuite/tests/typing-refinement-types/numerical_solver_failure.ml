(* TEST
 flags = "-extension refinement_types -smt-timeout 0";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
 file = "numerical_solver_failure.cmi";
 file-not-exists;
*)

let rec loop n = if n > 0 then loop (n - 1) else 0 [@@decreases n]
