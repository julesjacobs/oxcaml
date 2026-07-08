(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/oset_infer.mli ../lib/oset_infer.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Cross-unit client of a set whose interface has NO [@@vox.sort opaque]
   attribute: the sort is INFERRED opaque because oset_infer.mli's block
   mentions the type's minted name [Vox_Oset_infer_t].  The client sorts
   [t] at that block-declared opaque (read from the imported artifact --
   NOT the shared VoxU) and the imported law [mem_insert] fires, exactly
   as it would with the explicit attribute (cf. lean_oset_opaque.ml). *)

open Oset_infer

let after : (x : int) -> (t : set) -> unit =
  fun x t ->
    let t' = insert x t in
    let _u : unit{ mem x t' } = () in
    ignore t'
