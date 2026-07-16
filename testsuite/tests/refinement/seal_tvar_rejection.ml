(* TEST
 readonly_files = "\
   seal_tvar_failwith.ml seal_tvar_failwith.mli \
   seal_tvar_failwith.reference \
   seal_tvar_list_hd.ml seal_tvar_list_hd.mli \
   seal_tvar_list_hd.reference \
   seal_tvar_module.ml seal_tvar_module.reference \
   seal_weak_magic.ml seal_weak_magic.reference \
   seal_bare_tautology.ml seal_bare_tautology.reference \
 ";
 setup-ocamlc.byte-build-env;

 module = "seal_tvar_failwith.mli";
 ocamlc.byte;
 module = "seal_tvar_failwith.ml";
 compiler_output = "seal_tvar_failwith.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/seal_tvar_failwith.reference";
 check-ocamlc.byte-output;

 ocamlc_byte_exit_status = "0";
 module = "seal_tvar_list_hd.mli";
 ocamlc.byte;
 module = "seal_tvar_list_hd.ml";
 compiler_output = "seal_tvar_list_hd.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/seal_tvar_list_hd.reference";
 check-ocamlc.byte-output;

 module = "seal_tvar_module.ml";
 compiler_output = "seal_tvar_module.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/seal_tvar_module.reference";
 check-ocamlc.byte-output;

 module = "seal_weak_magic.ml";
 compiler_output = "seal_weak_magic.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/seal_weak_magic.reference";
 check-ocamlc.byte-output;

 module = "seal_bare_tautology.ml";
 compiler_output = "seal_bare_tautology.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/seal_bare_tautology.reference";
 check-ocamlc.byte-output;

 module = "seal_tvar_rejection.ml";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
*)

(* The supported remedy is to introduce the refinement at the definition,
   then prove refined-to-refined implication at the seal. *)
module Remedy : sig
  val x : int{ _ > 0 }
end = struct
  let x = (1 : int{ _ = 1 })
end
