(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/oset_infer.mli ../lib/oset_infer.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The INFERRED opaque sort is a real abstraction barrier, identical to
   the explicit [@@vox.sort opaque] (cf. lean_oset_opaque.ml): this claim
   is true of the concrete representation but not decided by the
   interface axioms, so the client cannot establish it.  If the inferred
   type had degraded to VoxU instead of its own sort, the abstraction
   would still hold -- but the emitted opaque [Vox_Oset_infer_t] (checked
   by lean_oset_infer_client.ml) is what makes it a DISTINCT sealed
   sort. *)

open Oset_infer

let cheat : bool{ _ = true } =
  let t1 = insert 2 empty in
  member 5 t1
