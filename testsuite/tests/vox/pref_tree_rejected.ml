(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml pref_tree.ml";
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
      (t : {t : node option Pref.token | valid model && Pref.own t === heap model}) @ unique ->
      {t : node option Pref.token | Pref.own t === heap (flipped model)} @ unique =
    fun model t ->
    let refine_ t = t in
    refine_ t
end;;
[%%expect{|
Line 5, characters 65-66:
5 |       (t : {t : node option Pref.token | valid model && Pref.own t === heap model}) @ unique ->
                                                                     ^
Error: The value "t" has type "Pref_tree.node option Pref.token"
       but an expression was expected of type "'a Pref.token"
       The kind of Pref_tree.node option is
           immutable_data with Pref_tree.node
         because it's a boxed variant type.
       But the kind of Pref_tree.node option must be a subkind of
           immutable_data.
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
