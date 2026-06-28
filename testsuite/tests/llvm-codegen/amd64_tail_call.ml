(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 llvm-backend;
 readonly_files = "amd64_tail_call.sh";
 script = "sh ${test_source_directory}/amd64_tail_call.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* AMD64 coverage that OCaml tail calls reach LLVM as [musttail] calls and
   still execute correctly through the normal LLVM backend. *)
