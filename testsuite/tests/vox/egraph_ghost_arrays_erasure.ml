(* TEST
 has-z3;
 flags = "-extension refinement_types -drawlambda -dcanonical-ids";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_egraph_language_spec.ml vox_egraph_rule_spec.ml vox_egraph_derivation_spec.ml";
 readonly_files = "egraph_ghost_arrays_erasure.ml vox_egraph_ghost_arrays.ml";
 compile_only = "true";
 {
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run-expect;
   check-program-output;
 }
*)

#use "vox_egraph_ghost_arrays.ml";;
[%%expect{|
0
module L = Vox_egraph_language_spec
0
module E = Vox_egraph_derivation_spec
(let
  (origins/0 =
     (function {nlocal = 0} param/0[value<int>] : addrarray (opaque 24029)))
  (apply (field_imm 1 (global Toploop!)) "origins" origins/0))
val origins :
  unit -> {values : L.expr iarray | (Iarray.length values) = 512} @ ghost =
  <fun>
(let
  (edges/0 =
     (function {nlocal = 0} param/1[value<int>] : addrarray (opaque 24029)))
  (apply (field_imm 1 (global Toploop!)) "edges" edges/0))
val edges :
  unit ->
  {values : E.evidence option iarray | (Iarray.length values) = 512} @ ghost =
  <fun>
|}]
