(* TEST
 has-z3;
 readonly_files = "closures.mli closures.ml";
 setup-ocamlc.byte-build-env;
 flags = "-extension refinement_types";
 module = "closures.mli";
 ocamlc.byte;
 module = "closures.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* [stage 3] The mode side condition at a compilation-unit interface.
   "pure" is accepted (its returned closure's mode variable is constrained to
   total); "counter" is rejected at typing by MC, before any proof.  Without
   MC, clients (and the verifier, which reflects total functions as logical
   symbols) would treat the stateful counter as a total function.
   Currently: rejected at typing on "pure" already:
     Values do not match:
       val pure : unit -> int -> int
     is not included in
       val pure : unit -> {f : int -> int | true}
     ...
     Type int -> int is not compatible with type {f : int -> int | true} *)
