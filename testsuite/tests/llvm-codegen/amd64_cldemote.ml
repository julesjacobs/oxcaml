(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_cldemote.sh";
 script = "sh ${test_source_directory}/amd64_cldemote.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks that AMD64 caml_cldemote reaches LLVM lowering. *)
