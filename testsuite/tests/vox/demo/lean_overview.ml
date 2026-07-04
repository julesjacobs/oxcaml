(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The docs/vox demo page's "sixty seconds" example, kept here so CI
   verifies exactly what the page shows. *)

let div (a : int) (b : int{ not (_ = 0) }) : int = a / b

let safe x = if 0 < x then div 100 x else 0
