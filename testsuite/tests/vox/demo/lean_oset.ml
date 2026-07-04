(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/oset.mli ../lib/oset.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Client of the fully abstract sealed interface (lib/oset.mli): the
   representation of [Oset.t] is invisible -- its sort is the opaque
   Vox_Oset_t, not a datatype -- so every step below is justified by
   the interface's axioms alone (no_mem_spec, bst_insert, mem_insert).
   Compare demo/lean_seal.ml, where only the FUNCTIONS were opaque;
   here the type is too. *)

open Oset

let found : bool{ _ = true } =
  let t1 = insert 2 empty in
  let t2 = insert 1 t1 in
  member 1 t2

let absent : bool{ _ = false } =
  let t1 = insert 2 empty in
  let t2 = insert 1 t1 in
  member 3 t2
