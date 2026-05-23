(* TEST
 macos;
 arch_arm64;
 readonly_files = "long_frame_failure.sh";
 script = "sh ${test_source_directory}/long_frame_failure.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script generates the actual stress test.  Keeping the generated source
   out of the repository avoids checking in thousands of repetitive live-root
   bindings. *)
