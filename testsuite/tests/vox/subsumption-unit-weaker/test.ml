(* TEST
 has-z3;
 readonly_files = "weaker.mli weaker.ml";
 setup-ocamlc.byte-build-env;
 flags = "-extension refinement_types";
 module = "weaker.mli";
 ocamlc.byte;
 module = "weaker.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
 file = "weaker.cmo";
 file-not-exists;
*)

(* [stage 3] The implementation is weaker than its interface: r = x does not
   imply r > x.  The error is reported by the verifier at the implementation
   of f, names the interface file, and points at the interface's predicate
   (by file name only, as for any refinement stated in another file).  No
   .cmo is written.
   Currently: rejected at typing (same exit status), with
     File "weaker.ml", line 1:
     Error: The implementation weaker.ml does not match the interface weaker.cmi:
            Values do not match:
              val f : (x : int) -> {r : int | r = x}
            is not included in
              val f : (x : int) -> {r : int | r > x}
            ...
            Type {r : int | r = x} is not compatible with type {r : int | r > x} *)
