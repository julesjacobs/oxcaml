(* TEST
 readonly_files = "refinement_alias_statement_provider.mli";
 setup-ocamlc.byte-build-env;

 module = "refinement_alias_statement_provider.mli";
 ocamlc.byte;

 module = "refinement_alias_statement.ml";
 flags = "-w +10";
 ocamlc.byte;

 flags = "-strict-sequence -w +10";
 ocamlc.byte;
*)

let use_aliased_law ()
    : unit{ Refinement_alias_statement_provider.p = true } =
  Refinement_alias_statement_provider.proof;
  ()

let use_composite_aliased_law ()
    : unit{ Refinement_alias_statement_provider.p = true } =
  let _items = Refinement_alias_statement_provider.list_proof in
  ()
