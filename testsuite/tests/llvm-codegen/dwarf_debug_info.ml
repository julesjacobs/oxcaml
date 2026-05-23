(* TEST
 macos;
 arch_arm64;
 readonly_files = "dwarf_debug_info.sh";
 script = "sh ${test_source_directory}/dwarf_debug_info.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks the current distinction between OCaml backtrace metadata
   and standard DWARF debug info. *)
