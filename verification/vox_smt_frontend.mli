type t
exception Error of string

(** [clock] is monotonic; the default uses process CPU time.
    [require_enabled_retrievals] requires each SMT-LIB retrieval command's
    [produce-] option to be enabled. It defaults to false for embedded callers. *)
val create :
  ?clock:(unit -> float) -> ?require_enabled_retrievals:bool -> unit -> t

(** An optional shared resource covers elaboration, preprocessing, search and
    model construction. SMT-LIB check limits also cover assumption elaboration.
    Interrupted checks return [unknown]. Other operations raise
    [Vox_smt_resource.Exhausted] without committing logical state.
    Commands completed before an interrupted [run] remain committed.
    The legacy [cancelled] callback applies to checks; cancellation in a
    shared resource applies to all operations, including parsing in [run].
    Interrupted commands can leave unreachable terms in the append-only arena.
    A check may rebuild live assertions after a pop or reset-assertions; this
    reclamation shares the check resource.
    Callbacks must not mutate or reenter the frontend. *)
val execute :
  ?resource:Vox_smt_resource.t -> ?cancelled:(unit -> bool) ->
  t -> Vox_smt_sexp.t -> string option
val run :
  ?resource:Vox_smt_resource.t -> ?cancelled:(unit -> bool) ->
  t -> string -> string list
