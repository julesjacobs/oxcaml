(* TEST
 flags = "-vox-dry-run";
 modules = "rlib.ml";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* vox: [type t : value refines int] read back from a REAL .cmi (rlib
   is a separate compilation unit).  [assume_] compiles a runtime
   check only for int/bool-sorted values, so this compiles exactly
   when the refines survived serialization. *)

let f (x : Rlib.t) : Rlib.t{ _ > 0 } = assume_ x
