(* TEST
 readonly_files = "\
   set_intf.ml bal_intf.ml \
   bst.mli bst.ml rbt.mli rbt.ml avl.mli avl.ml ulist.mli ulist.ml \
   client.ml \
 ";
 setup-ocamlc.byte-build-env;
 module = "set_intf.ml";
 ocamlc.byte;
 module = "bal_intf.ml";
 flags = "-I ocamlc.byte";
 ocamlc.byte;
 module = "bst.mli";
 flags = "-I ocamlc.byte";
 ocamlc.byte;
 module = "bst.ml";
 ocamlc.byte;
 module = "rbt.mli";
 ocamlc.byte;
 module = "rbt.ml";
 ocamlc.byte;
 module = "avl.mli";
 ocamlc.byte;
 module = "avl.ml";
 ocamlc.byte;
 module = "ulist.mli";
 ocamlc.byte;
 module = "ulist.ml";
 ocamlc.byte;
 module = "client.ml";
 ocamlc.byte;
*)
