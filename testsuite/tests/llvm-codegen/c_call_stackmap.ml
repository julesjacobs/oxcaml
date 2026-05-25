(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "c_call_stackmap.sh";
 script = "sh ${test_source_directory}/c_call_stackmap.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)
