(* TEST
 flags = "-vox-dry-run";
 modules = "sortlean_i.mli sortlean_i.ml";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* An .mli/.ml pair must agree on [@@vox.sort lean "..."] too: the
   ghost sort travels through the .cmi as declaration metadata and is
   compared by its Lean name, so a name mismatch between interface and
   implementation is rejected at inclusion (like the base-sort case in
   vox_sort_intf.ml). *)

let unused = 0
