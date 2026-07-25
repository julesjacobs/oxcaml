(* TEST
 flags = "-vox-backend lean -vox-dump-vc-json reflexive-facts.json -c";
 readonly_files = "\
   reflexive_facts_source.ml reflexive_facts_check.py \
 ";
 setup-ocamlc.byte-build-env;

 (* Verification still succeeds, and no emitted fact has the same term on
    both sides of an equality. *)
 module = "reflexive_facts_source.ml";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/reflexive_facts_check.py \
           reflexive-facts.json";
 script;
*)

(* A fact whose two sides are the same term holds at every instantiation, so
   it constrains nothing while costing solver input, proof-pane lines and
   hydration payload.  The check compares emitted terms rather than their
   rendered form: a global reference and a local binder can print with the
   same name while denoting different terms, and those facts do carry content
   and are retained. *)
