(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_prefetch.sh";
 script = "sh ${test_source_directory}/amd64_prefetch.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks that AMD64 Cprefetch reaches LLVM prefetch lowering. *)
