(* TEST
 readonly_files = "\
   key_intf.ml int_key.mli int_key.ml \
   gen_ulist.mli gen_ulist.ml gen_bst.mli gen_bst.ml \
   gen_avl.mli gen_avl.ml gen_rbt.mli gen_rbt.ml \
   client.ml \
 ";
 setup-ocamlc.byte-build-env;
 module = "key_intf.ml";
 ocamlc.byte;
 module = "int_key.mli";
 flags = "-I ocamlc.byte";
 ocamlc.byte;
 module = "int_key.ml";
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
 module = "gen_rbt.mli";
 ocamlc.byte;
 module = "gen_rbt.ml";
 ocamlc.byte;
 module = "client.ml";
 ocamlc.byte;
*)
