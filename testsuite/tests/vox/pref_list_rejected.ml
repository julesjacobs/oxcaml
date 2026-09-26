(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml vox_pref_semantics.mli vox_pref_semantics.ml pref_list.mli pref_list.ml pref_list_client.ml";
 readonly_files = "pref_list_rejected.ml";
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

open Pref_list

module No_reversal = struct
  let bad : (pointer : node option) @ immutable ->
      (xs : model) @ immutable ghost ->
      (t : {t : node option Pref.token | valid xs && root xs === pointer
        && Pref.own t === heap xs}) @ unique ->
      {r : result | r.pointer === root (rev_append xs Nil)
        && Pref.own r.state === heap (rev_append xs Nil)} @ unique =
    fun pointer xs t ->
    let r = {pointer; state = t} in
    r
end;;
[%%expect{|
Line 12, characters 4-5:
12 |     r
         ^
Error: Refinement could not be proved (counterexample)
|}, Principal{|
Line 7, characters 20-21:
7 |         && Pref.own t === heap xs}) @ unique ->
                        ^
Error: The value "t" has type "Pref_list.node option Pref.token"
       but an expression was expected of type "'a Pref.token"
       The kind of Pref_list.node option is
           immutable_data with Pref_list.node
         because it's a boxed variant type.
       But the kind of Pref_list.node option must be a subkind of
           immutable_data.
|}]

module Lost_node = struct
  let bad : (n : node) @ immutable -> (xs : model) @ immutable ghost ->
      (t : {t : node option Pref.token | Pref.own t === heap (Cons (n, xs))}) @ unique ->
      {t : node option Pref.token | Pref.own t === heap xs} @ unique = fun n xs t ->
    t
end;;
[%%expect{|
Line 5, characters 4-5:
5 |     t
        ^
Error: Refinement could not be proved (counterexample)
|}, Principal{|
Line 3, characters 50-51:
3 |       (t : {t : node option Pref.token | Pref.own t === heap (Cons (n, xs))}) @ unique ->
                                                      ^
Error: The value "t" has type "Pref_list.node option Pref.token"
       but an expression was expected of type "'a Pref.token"
       The kind of Pref_list.node option is
           immutable_data with Pref_list.node
         because it's a boxed variant type.
       But the kind of Pref_list.node option must be a subkind of
           immutable_data.
|}]

(* The v3 structures gate checked these outside ocamltest: private helpers are
   hidden by the interface, and consumed owned handles or raw tokens cannot be
   reused. *)
let hidden = Pref_list.reverse_into;;
[%%expect{|
Line 1, characters 13-35:
1 | let hidden = Pref_list.reverse_into;;
                 ^^^^^^^^^^^^^^^^^^^^^^
Error: Unbound value "Pref_list.reverse_into"
|}]

let hidden = Pref_list.Owned.model_def;;
[%%expect{|
Line 1, characters 13-38:
1 | let hidden = Pref_list.Owned.model_def;;
                 ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unbound value "Pref_list.Owned.model_def"
|}]

let list_reuse () =
  let s = Pref_list.Owned.of_list [1] in
  let _ = Pref_list.Owned.reverse s in
  Pref_list.Owned.reverse s;;
[%%expect{|
Line 4, characters 26-27:
4 |   Pref_list.Owned.reverse s;;
                              ^
Error: This value is used here, but it has already been used as unique at:
Line 3, characters 34-35:
3 |   let _ = Pref_list.Owned.reverse s in
                                      ^

|}]

let raw_reuse () =
  let s = Pref_list.Owned.of_list [1] in
  let b = Pref_list.Owned.release s in
  let _ = Pref_list.Owned.adopt b in
  Pref_list.Owned.adopt b;;
[%%expect{|
Line 5, characters 24-25:
5 |   Pref_list.Owned.adopt b;;
                            ^
Error: This value is used here, but it has already been used as unique at:
Line 4, characters 32-33:
4 |   let _ = Pref_list.Owned.adopt b in
                                    ^

|}]
