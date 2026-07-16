(* TEST
 modules = "vox_spec.ml";
 setup-ocamlc.byte-build-env;
 module = "vox_spec.ml";
 flags = "-w -220";
 ocamlc.byte;
 module = "";
 flags += " -I ${test_build_directory}/ocamlc.byte";
 expect;
*)

(* This pins the inferred public surface of the separately compiled prelude. *)
#load "vox_spec.cmo";;

include Vox_spec

[%%expect {|
val implies : bool -> bool -> bool = <fun>
val conjunction : bool -> bool -> bool = <fun>
val int_lt : int -> int -> bool = <fun>
val int_le : int -> int -> bool = <fun>
val int_gt : int -> int -> bool = <fun>
val int_ge : int -> int -> bool = <fun>
val list_length : 'a list -> int = <fun>
|}]
