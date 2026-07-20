(* TEST
 readonly_files = "labeled_parameter_subtype_swapped.ml";
 setup-ocamlc.byte-build-env;

 module = "labeled_parameter_subtype_swapped.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
*)

type left =
  x:int{ _ = 0 } @ logical ->
  x:int{ _ = x + 1 } @ logical ->
  unit{ x = x }

type right =
  x:int{ _ = 0 } @ logical ->
  x:int{ _ = x + 1 } @ logical ->
  unit{ x = x }

let position_correct (f : left) : right = (f :> right)
