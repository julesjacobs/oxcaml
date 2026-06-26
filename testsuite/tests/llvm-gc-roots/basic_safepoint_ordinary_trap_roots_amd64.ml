(* TEST
 arch_amd64;
 readonly_files = "basic_safepoint_ordinary_trap_roots.sh";
 script = "sh ${test_source_directory}/basic_safepoint_ordinary_trap_roots.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* AMD64 coverage for normal GC roots at basic safepoints inside ordinary trap
   regions.  The x86 exception edge itself still uses the old wrap_try path, so
   the Arm-only invoke assertion remains disabled on this target. *)
