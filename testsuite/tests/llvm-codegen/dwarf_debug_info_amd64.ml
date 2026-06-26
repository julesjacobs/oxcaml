(* TEST
 arch_amd64;
 readonly_files = "dwarf_debug_info.sh";
 script = "sh ${test_source_directory}/dwarf_debug_info.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* AMD64 coverage that [-g -llvm-backend] emits assembler-compatible DWARF
   debug info as well as OCaml frame-table metadata. *)
