type t = Atom of string | String of string | List of t list
exception Error of string
val to_string : ?resource:Vox_smt_resource.t -> t -> string

(** Interrupted streaming reads consume part of the current expression.
    Close the channel after an interrupted read; its position is not
    restored. *)
val read : ?resource:Vox_smt_resource.t -> in_channel -> t option

(** When [stop] returns true, include that expression and ignore the suffix. *)
val parse :
  ?resource:Vox_smt_resource.t -> ?stop:(t -> bool) -> string -> t list
