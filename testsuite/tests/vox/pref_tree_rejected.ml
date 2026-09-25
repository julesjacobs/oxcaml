(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml pref_tree.mli pref_tree.ml pref_tree_client.ml";
 readonly_files = "pref_tree_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run-expect;
   check-program-output;
 }
 {
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   run-expectnat;
   check-program-output;
 }
*)

open Pref_tree

module Unchanged_tree = struct
  let bad : (model : tree) @ immutable ghost ->
      (t : {t : Pref.token | valid model && Pref.own t === heap model}) @ unique ->
      {t : Pref.token | Pref.own t === heap (flipped model)} @ unique =
    fun model t ->
    let refine_ t = t in
    refine_ t
end;;
[%%expect{|
Line 9, characters 4-13:
9 |     refine_ t
        ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Shared_subtree = struct
  let bad (n : node @ immutable) =
    let model = ghost_ (Branch (n, Empty, Empty)) in
    let shared = ghost_ (Branch (n, model, model)) in
    let refine_ definition = ghost_ (valid_def shared) in
    let u = () in
    let claim : {u : unit | valid shared} = refine_ u in
    ignore claim
end;;
[%%expect{|
Line 7, characters 44-53:
7 |     let claim : {u : unit | valid shared} = refine_ u in
                                                ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
