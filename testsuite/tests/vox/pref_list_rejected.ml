(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml pref_list.mli pref_list.ml pref_list_client.ml";
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
      (t : {t : Pref.token | valid xs && root xs === pointer
        && Pref.own t === heap xs}) @ unique ->
      {r : result | r.pointer === root (rev_append xs Nil)
        && Pref.own r.state === heap (rev_append xs Nil)} @ unique =
    fun pointer xs t ->
    let refine_ t = t in
    let r = {pointer; state = t} in
    refine_ r
end;;
[%%expect{|
Line 13, characters 4-13:
13 |     refine_ r
         ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Lost_node = struct
  let bad : (n : node) @ immutable -> (xs : model) @ immutable ghost ->
      (t : {t : Pref.token | Pref.own t === heap (Cons (n, xs))}) @ unique ->
      {t : Pref.token | Pref.own t === heap xs} @ unique = fun n xs t ->
    let refine_ t = t in
    refine_ t
end;;
[%%expect{|
Line 6, characters 4-13:
6 |     refine_ t
        ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
