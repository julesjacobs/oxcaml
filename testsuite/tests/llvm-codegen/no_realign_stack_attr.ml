(* TEST
 not-windows;
 arch_amd64;
 readonly_files = "no_realign_stack_attr.sh";
 script = "sh ${test_source_directory}/no_realign_stack_attr.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks that AMD64 LLVM functions opt out of dynamic stack
   realignment before MachineFrameInfo creates stack slots. *)
