(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Verification-condition generation for refinements         *)
(*                                                                        *)
(**************************************************************************)

val verify_structure : ?toplevel:bool -> Typedtree.structure -> unit

(** True only for a marked, saturated regular application.  Translcore may
    replace such an application with the unit value without translating its
    head or arguments. *)
val is_erased_proof_call : Typedtree.expression -> bool

val finish_dump : unit -> unit

val render_display :
  env:Env.t -> Types.refinement_expression -> string

val verify_seal_obligations :
  env:Env.t ->
  seal_location:Location.t ->
  Ctype.refinement_seal_obligation list ->
  unit
