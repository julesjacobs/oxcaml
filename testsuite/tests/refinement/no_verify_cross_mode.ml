(* TEST
 readonly_files = "\
   nvcm_provider.mli nvcm_provider.ml nvcm_client.ml nvcm_client.reference \
   nvcm_plain.mli nvcm_plain.ml nvcm_plain_client.ml \
   nvcm_mixed.mli nvcm_mixed.ml nvcm_mixed.reference \
   nvcm_bare.mli nvcm_bare.ml nvcm_bare_client.ml nvcm_bare_client.reference \
   nvcm_solo.ml nvcm_solo_client.ml nvcm_solo_client.reference \
 ";
 setup-ocamlc.byte-build-env;

 (* An interface written without discharging obligations is marked, whatever
    it exports, because deciding that it mentions no refinement would mean
    expanding named module types and type constructors through other units. *)
 flags = "-vox-no-verify";
 module = "nvcm_provider.mli";
 ocamlc.byte;
 module = "nvcm_provider.ml";
 ocamlc.byte;

 (* Consuming it from a mode that also discharges nothing is allowed. *)
 module = "nvcm_client.ml";
 ocamlc.byte;
 flags = "-vox-type-only";
 module = "nvcm_client.ml";
 ocamlc.byte;

 (* A discharging compilation must refuse it rather than prove [_ = 0] of a
    value that is 1. *)
 flags = "-vox-backend z3 -vox-smt-solver false";
 module = "nvcm_client.ml";
 compiler_output = "nvcm_client.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/nvcm_client.reference";
 check-ocamlc.byte-output;

 (* The same refusal applies to an interface exporting no refinement: the
    artifact still went unchecked, and the mode boundary is what is enforced. *)
 ocamlc_byte_exit_status = "0";
 flags = "-vox-no-verify";
 module = "nvcm_bare.mli";
 ocamlc.byte;
 module = "nvcm_bare.ml";
 ocamlc.byte;
 flags = "-vox-backend z3 -vox-smt-solver false";
 module = "nvcm_bare_client.ml";
 compiler_output = "nvcm_bare_client.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/nvcm_bare_client.reference";
 check-ocamlc.byte-output;

 (* An interface compiled with verification owns an unmarked .cmi, so its
    implementation may not be compiled without verification: nothing would
    then check the implementation against it. *)
 ocamlc_byte_exit_status = "0";
 module = "nvcm_mixed.mli";
 ocamlc.byte;
 flags = "-vox-no-verify";
 module = "nvcm_mixed.ml";
 compiler_output = "nvcm_mixed.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/nvcm_mixed.reference";
 check-ocamlc.byte-output;

 (* A unit with no interface file writes its own .cmi, so the mark is applied
    by the implementation compilation rather than by a separate interface
    compilation. *)
 ocamlc_byte_exit_status = "0";
 flags = "-vox-no-verify";
 module = "nvcm_solo.ml";
 ocamlc.byte;
 flags = "-vox-backend z3 -vox-smt-solver false";
 module = "nvcm_solo_client.ml";
 compiler_output = "nvcm_solo_client.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/nvcm_solo_client.reference";
 check-ocamlc.byte-output;

 (* No false rejection of ordinary single-mode builds. *)
 ocamlc_byte_exit_status = "0";
 flags = "-vox-backend z3 -vox-smt-solver false";
 module = "nvcm_plain.mli";
 ocamlc.byte;
 module = "nvcm_plain.ml";
 ocamlc.byte;
 module = "nvcm_plain_client.ml";
 ocamlc.byte;
*)

(* [-vox-no-verify] accepts refinement claims without discharging them, so the
   artifacts it writes are consumable only by compilations that also discharge
   nothing.  An interface it writes is marked and a discharging compilation
   refuses to import it; an interface written by a discharging compilation may
   not have its implementation compiled this way.  Without both rules a
   verified client proves a false refinement about a provider value and links
   to a program that contradicts it. *)
