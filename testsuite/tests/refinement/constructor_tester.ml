(* TEST
 readonly_files = "ctp_source.ml";
 setup-ocamlc.byte-build-env;

 (* Both SMT translations must interpret the arm facts, so each backend is
    exercised explicitly rather than left to the default. *)
 flags = "-vox-backend z3 -c";
 module = "ctp_source.ml";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;

 flags = "-vox-backend oxsmt -c";
 module = "ctp_source.ml";
 ocamlc.byte;
*)

(* A match arm that does not fire says its scrutinee is not that arm's
   constructor.  Both SMT translations interpret that as the datatype tester,
   so a fallback can name the constructor that remains even when an excluded
   arm carries a payload; and the fact is not registered as an opaque symbol
   keyed by constructor name, so one polymorphic constructor used at two
   instantiations does not collide. *)
