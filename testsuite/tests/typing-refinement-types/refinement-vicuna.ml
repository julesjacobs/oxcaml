(* TEST
 flags = "-extension refinement_types -bin-annot-cms";
 compile_only = "true";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
*)

type refined = { x : int | true }

external refined_identity : refined -> refined = "refinement_identity"

type unboxed_refined = Unboxed of refined [@@unboxed]

external unboxed_refined_identity :
  unboxed_refined -> unboxed_refined = "unboxed_refinement_identity"

let make_unboxed x = Unboxed x
let unwrap_unboxed (Unboxed x) = x
