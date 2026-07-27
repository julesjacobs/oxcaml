(* TEST
 readonly_files = "\
   key_intf.ml gen_ulist.mli wrong_ulist_duplicate.patch wrong_ulist_duplicate.reference \
 ";
 setup-ocamlc.byte-build-env;
 module = "key_intf.ml";
 ocamlc.byte;
 module = "gen_ulist.mli";
 flags = "-I ocamlc.byte";
 ocamlc.byte;
 script = "cp ${test_source_directory}/gen_ulist.ml gen_ulist.ml";
 script;
 script = "patch --silent gen_ulist.ml wrong_ulist_duplicate.patch";
 script;
 module = "gen_ulist.ml";
 compiler_output = "wrong_ulist_duplicate.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/wrong_ulist_duplicate.reference";
 check-ocamlc.byte-output;
*)

(* The mutation is a patch against [gen_ulist.ml] rather than a whole copy of
   it.  Three copies cost 2,368 lines to carry six changed ones, and a copy
   silently stops tracking the module it is supposed to be a mutation of.  A
   patch that no longer applies fails the test, which is the behaviour
   wanted: if [gen_ulist.ml] moves, the fixture has to be looked at. *)
