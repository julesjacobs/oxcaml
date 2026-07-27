(* TEST
 readonly_files = "\
   key_intf.ml int_key.mli int_key.ml pair_key.mli pair_key.ml \
   gen_ulist.mli gen_ulist.ml gen_bst.mli gen_bst.ml \
   gen_avl.mli gen_avl.ml gen_sorted.mli gen_sorted.ml \
   client.ml runtime.ml runtime.reference \
 ";
 setup-ocamlc.byte-build-env;
 module = "key_intf.ml";
 ocamlc.byte;
 module = "int_key.mli";
 flags = "-I ocamlc.byte";
 ocamlc.byte;
 module = "int_key.ml";
 ocamlc.byte;
 module = "pair_key.mli";
 ocamlc.byte;
 module = "pair_key.ml";
 ocamlc.byte;
 module = "gen_ulist.mli";
 ocamlc.byte;
 module = "gen_ulist.ml";
 ocamlc.byte;
 module = "gen_bst.mli";
 ocamlc.byte;
 module = "gen_bst.ml";
 ocamlc.byte;
 module = "gen_avl.mli";
 ocamlc.byte;
 module = "gen_avl.ml";
 ocamlc.byte;
 module = "gen_sorted.mli";
 ocamlc.byte;
 module = "gen_sorted.ml";
 ocamlc.byte;
 module = "client.ml";
 ocamlc.byte;
 module = "";
 program = "${test_build_directory}/runtime.exe";
 all_modules = "\
   key_intf.cmo int_key.cmo pair_key.cmo \
   gen_ulist.cmo gen_bst.cmo gen_avl.cmo gen_sorted.cmo \
   client.cmo runtime.ml \
 ";
 flags = "-I .";
 ocamlc.byte;
 output = "${test_build_directory}/runtime.output";
 stdout = "${output}";
 stderr = "${output}";
 run;
 reference = "${test_source_directory}/runtime.reference";
 check-program-output;
*)
