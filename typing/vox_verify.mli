(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Verification-condition generation for refinements         *)
(*                                                                        *)
(**************************************************************************)

val verify_structure : ?toplevel:bool -> Typedtree.structure -> unit

val verify_seal_obligations :
  env:Env.t ->
  seal_location:Location.t ->
  Ctype.refinement_seal_obligation list ->
  unit
