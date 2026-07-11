(** The [(set-info :status ...)] verdict label shared by the {!Printer} and the test-only
    parser. *)

type t =
  | Sat
  | Unsat
  | Unknown

val to_string : t -> string
val of_string : string -> t option
val equal : t -> t -> bool
