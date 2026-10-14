module Term = Vox_smt_term
module Z = Vox_smt_integer.Z

type value =
  | Boolean of bool
  | Integer of Z.t
  | Bits of int * Z.t
  | Abstract of int * int
  | Constructor of Term.symbol * value array

type t

exception Unavailable of string

(** Reconstruct the current satisfying model, or raise [Unavailable].
    Successful assertions, scope changes, new registrations and new checks
    invalidate the handle, including checks that are cancelled or raise.
    Every query below rejects stale handles with [Unavailable]. Unsupported
    completions also raise [Unavailable]. *)
val create : terms:Term.t -> solver:Vox_smt_engine.t -> t
val create_with_resource :
  Vox_smt_resource.t -> terms:Term.t -> solver:Vox_smt_engine.t -> t

(** Values and interpretation arrays are borrowed from the model. Callers
    must not mutate constructor fields or interpretation argument arrays.
    Caller-created values supplied below must be acyclic and remain
    unchanged throughout the operation. *)
val value : ?resource:Vox_smt_resource.t -> t -> Term.term -> value

val interpretation :
  ?resource:Vox_smt_resource.t ->
  t -> Term.symbol -> (value array * value) list * value

val to_string :
  ?resource:Vox_smt_resource.t -> ?qualify:(Term.symbol -> bool) ->
  t -> sort_name:(int -> string) ->
  value -> string

val equal :
  ?resource:Vox_smt_resource.t -> t -> value -> value -> bool
