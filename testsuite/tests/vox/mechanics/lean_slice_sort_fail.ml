(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../demo/slice_lib.mli ../demo/slice_lib.ml ../demo/slice_sort_lib.mli ../demo/slice_sort_lib.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

open Slice_lib
open Slice_sort_lib

let negative_split :
  (m : slice) @ local unique -> unit =
  fun m -> merge_sorted_halves (-1) m
