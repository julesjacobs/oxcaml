(* TEST
 has-z3;
 source_directories = "${test_source_directory}/../../../verification/library";
 readonly_files = "pref.mli pref_tree.mli";
 setup-ocamlc.opt-build-env;
 flags = "-extension refinement_types -vox-library";
 module = "pref.mli";
 ocamlc.opt; flags = "-extension refinement_types";
 module = "pref_tree.mli";
 ocamlc.opt;
 module = "pref_tree_observe_rejected.ml";
 ocamlc_opt_exit_status = "2";
 ocamlc.opt;
 check-ocamlc.opt-output;
*)
open Pref_tree

(* [observe] returns the token that owns the whole tree, not an empty one. *)
let bad () : {t : node option Pref.token | Pref.own t === heap Empty} @ unique =
  let b = leaf 1 in
  let pointer = b.pointer in
  let model = b.model in
  let t = b.state in
  let t : {t : node option Pref.token | valid model && root model === pointer
    && Pref.own t === heap model} = t in
  let r = observe pointer model t in
  r.state
