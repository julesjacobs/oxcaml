(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml pref_list.ml";
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
    let refine_ t = t in
    let r = {pointer; state = t} in
    refine_ r
end;;
[%%expect{|
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
    let refine_ t = t in
    refine_ t
end;;
[%%expect{|
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
