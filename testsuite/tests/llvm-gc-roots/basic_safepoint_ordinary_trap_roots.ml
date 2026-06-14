(* TEST
 macos;
 arch_arm64;
 readonly_files = "basic_safepoint_ordinary_trap_roots.sh";
 script = "sh ${test_source_directory}/basic_safepoint_ordinary_trap_roots.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks that basic safepoints in active ordinary trap regions keep
   normal live roots without pretending to unwind to the ordinary trap. *)
