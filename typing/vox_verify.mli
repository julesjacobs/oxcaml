(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Verification-condition generation for refinements         *)
(*                                                                        *)
(**************************************************************************)

val verify_structure : ?toplevel:bool -> Typedtree.structure -> unit

val finish_dump : unit -> unit

val render_display :
  env:Env.t -> Types.refinement_expression -> string

val verify_seal_obligations :
  env:Env.t ->
  seal_location:Location.t ->
  Ctype.refinement_seal_obligation list ->
  unit

(** Write out every obligation this unit admitted rather than proved.  Called
    for every compilation, including those that discharge nothing, since an
    admission is a fact about the source and not about the verification. *)
val report_admissions : unit -> unit

(** Run something and report its admissions however it leaves, so that a unit
    which admits and also fails still says what it admitted, and so that a
    failed phrase leaves nothing to be reported against a later one. *)
val reporting_admissions : (unit -> 'a) -> 'a
