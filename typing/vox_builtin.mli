type t =
  [ `Add
  | `And
  | `Bigint_abs
  | `Bigint_add
  | `Bigint_compare
  | `Bigint_ge
  | `Bigint_gt
  | `Bigint_is_zero
  | `Bigint_le
  | `Bigint_lt
  | `Bigint_mul
  | `Bigint_neg
  | `Bigint_of_int
  | `Bigint_one
  | `Bigint_sub
  | `Bigint_zero
  | `Equal
  | `Greater
  | `Greater_equal
  | `Less
  | `Less_equal
  | `Multiply
  | `Not
  | `Not_equal
  | `Or
  | `Subtract ]

val of_primitive : string -> t option
val of_path : Path.t -> t option
val is_bigint_type : Path.t -> bool

(** Every recognized operation is pure and deterministic.  The verifier's
    stable-call classification relies on this property, so additions to either
    classifier must preserve it.  [of_path] uses persistent identifier
    identity; printed or shadowed names are never sufficient. *)

(* A match arm that did not fire contributes the fact that its scrutinee is
   not that arm's constructor, carried as an application of a function with
   this prefix.  Both backends recognise it and neither depends on the
   other. *)
val constructor_mismatch_prefix : string
val constructor_mismatch_name : string -> string
val constructor_mismatch : string -> string option
