(* TEST
 arch_amd64;
 readonly_files = "async_control_transfer.sh";
 script = "sh ${test_source_directory}/async_control_transfer.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* AMD64 coverage that LLVM stack-growth async transfers stay distinct from
   ordinary exception handlers. *)
