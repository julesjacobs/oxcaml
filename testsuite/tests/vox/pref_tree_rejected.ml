(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml vox_pref_semantics.mli vox_pref_semantics.ml pref_tree.mli pref_tree.ml pref_tree_client.ml";
 readonly_files = "pref_tree_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.opt-build-env;
   ocamlc.opt;
   run-expect;
   check-program-output;
 }
*)

open Pref_tree

module Unchanged_tree = struct
  let bad : (model : tree) @ immutable ghost ->
      (t : {t : node option Pref.token | valid model && Pref.own t === heap model}) @ unique ->
      {t : node option Pref.token | Pref.own t === heap (flipped model)} @ unique =
    fun model t ->
    t
end;;
[%%expect{|
Line 8, characters 4-5:
8 |     t
        ^
Error: Refinement could not be proved (counterexample)
|}, Principal{|
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
    let definition = ghost_ (valid_def shared) in
    let u = () in
    let claim : {u : unit | valid shared} = u in
    ignore claim
end;;
[%%expect{|
Line 5, characters 8-18:
5 |     let definition = ghost_ (valid_def shared) in
            ^^^^^^^^^^
Warning 26 [unused-var]: unused variable "definition".

Line 7, characters 44-45:
7 |     let claim : {u : unit | valid shared} = u in
                                                ^
Error: Refinement could not be proved (counterexample)
|}]

(* The v3 structures gate checked these outside ocamltest: private helpers are
   hidden by the interface, and a consumed owned handle cannot be reused. *)
let hidden = Pref_tree.set_links;;
[%%expect{|
Line 1, characters 13-32:
1 | let hidden = Pref_tree.set_links;;
                 ^^^^^^^^^^^^^^^^^^^
Error: Unbound value "Pref_tree.set_links"
|}]

let hidden = Pref_tree.Owned.model_def;;
[%%expect{|
Line 1, characters 13-38:
1 | let hidden = Pref_tree.Owned.model_def;;
                 ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unbound value "Pref_tree.Owned.model_def"
|}]

let tree_reuse () =
  let s = Pref_tree.Owned.leaf 1 in
  let _ = Pref_tree.Owned.mirror s in
  Pref_tree.Owned.mirror s;;
[%%expect{|
Line 4, characters 25-26:
4 |   Pref_tree.Owned.mirror s;;
                             ^
Error: This value is used here, but it has already been used as unique at:
Line 3, characters 33-34:
3 |   let _ = Pref_tree.Owned.mirror s in
                                     ^

|}]
