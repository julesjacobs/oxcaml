(* TEST
 readonly_files = "\
   sibconf.ml \
   scf_prov.mli scf_cli.ml scf_true.ml scf_reject.ml \
   scb_prov.mli scb_cli.ml \
   scc_prov.mli scc_cli.ml scc_true.ml \
   scd_prov.mli scd_cli.ml \
   scn_prov.mli scn_cli.ml \
 ";
 setup-ocamlc.byte-build-env;

 (* Carrier A -- functor two-instance, single file. *)
 module = "sibconf.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;

 (* Carrier A -- cross-unit declared functor. *)
 ocamlc_byte_exit_status = "0";
 module = "scf_prov.mli";
 ocamlc.byte;
 ocamlc_byte_exit_status = "2";
 module = "scf_cli.ml";
 ocamlc.byte;
 (* true control: same instance proves *)
 ocamlc_byte_exit_status = "0";
 module = "scf_true.ml";
 ocamlc.byte;
 (* reject control: contract genuinely unmet *)
 ocamlc_byte_exit_status = "2";
 module = "scf_reject.ml";
 ocamlc.byte;

 (* Carrier B -- first-class modules. *)
 ocamlc_byte_exit_status = "0";
 module = "scb_prov.mli";
 ocamlc.byte;
 ocamlc_byte_exit_status = "2";
 module = "scb_cli.ml";
 ocamlc.byte;

 (* Carrier C -- plain module R1/R2, no functor (minimal). *)
 ocamlc_byte_exit_status = "0";
 module = "scc_prov.mli";
 ocamlc.byte;
 ocamlc_byte_exit_status = "2";
 module = "scc_cli.ml";
 ocamlc.byte;
 ocamlc_byte_exit_status = "0";
 module = "scc_true.ml";
 ocamlc.byte;

 (* Carrier D -- two DIFFERENT sigs sharing only a sibling name (honest
    accidental-collision case). *)
 ocamlc_byte_exit_status = "0";
 module = "scd_prov.mli";
 ocamlc.byte;
 ocamlc_byte_exit_status = "2";
 module = "scd_cli.ml";
 ocamlc.byte;

 (* Control -- different sibling NAMES stay rejected (name-keying guard). *)
 ocamlc_byte_exit_status = "0";
 module = "scn_prov.mli";
 ocamlc.byte;
 ocamlc_byte_exit_status = "2";
 module = "scn_cli.ml";
 ocamlc.byte;
*)

(* Cross-module sibling-reference conflation regression (F1).  A refinement that
   mentions a sibling value is signature-relative and was lowered to a bare name;
   on projection under a module path it is requalified to that instance
   ([M.cap]), so two instances of one signature -- or two signatures sharing a
   value name -- no longer collapse their siblings into a single prover symbol.
   Carriers A (functor), B (first-class modules), C (plain module) and D
   (distinct signatures, shared name) each false-proved before the fix; the true
   and reject controls, and the different-name control, pin that the fix
   distinguishes instances without over- or under-rejecting. *)
