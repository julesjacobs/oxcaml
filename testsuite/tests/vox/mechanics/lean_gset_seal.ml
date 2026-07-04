(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/gset.mli ../lib/gset.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A ghost sort declared in a SEALED interface travels: the client
   binds Gset.t at the Lean sort GSet (from the .cmi) and imports GSet
   plus its spec functions from the unit's VoxSig, so it proves through
   the ghost sort with no visibility into the representation.  Confirms
   Vs_lean composes with the sealed sig-module export. *)

open Gset

let roundtrip : (x : int) -> (s : t) -> bool{ _ = mem x (ins x s) } =
  fun x s ->
    let u = add x s in
    member x u
