(* TEST
 flags = "-rectypes -vox-no-verify -c";
 readonly_files = "reflexive_recursive_carrier_source.ml";
 setup-ocamlc.byte-build-env;

 (* A structurally recursive carrier has no name to track, so deciding
    whether its equality is reflexive has to terminate on the type nodes
    themselves.  This compiled forever while only named paths were tracked. *)
 module = "reflexive_recursive_carrier_source.ml";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
*)
