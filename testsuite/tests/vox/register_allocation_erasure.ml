(* TEST
 has-z3;
 flags = "-extension refinement_types";
 all_modules = "register_allocation_spec.ml register_allocation.mli register_allocation.ml";
 readonly_files = "register_allocation_erasure.ml";
 compile_only = "true";
 {
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   flags += " -drawlambda -dcanonical-ids";
   run-expectnat;
   check-program-output;
 }
*)

let (with_preservation @ total) program physical args fuel =
  ghost_ (Register_allocation.preserves program physical args fuel);
  7;;
[%%expect{|
(let
  (with_preservation/0 =
     (function {nlocal = 0}
       program/0[value<
                  (consts ())
                   (non_consts ([0:
                                 value<(consts (0)) (non_consts ([0: ?, *]))>,
                                 value<int>,
                                 value<(consts (0)) (non_consts ([0: ?, *]))>]))>]
       physical/0[value<int>]
       args/0[value<
               (consts (0))
                (non_consts ([0: ?,
                              value<(consts (0)) (non_consts ([0: ?, *]))>]))>]
       fuel/0[value<
               (consts (0))
                (non_consts ([0: value<(consts (0)) (non_consts ([0: *]))>]))>]
       : int 7))
  (makeblock 0 with_preservation/0))
val with_preservation :
  Register_allocation_spec.program @ total ->
  int @ total ->
  int list @ total -> Register_allocation_spec.fuel @ total -> int = <fun>
|}]
