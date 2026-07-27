type t =
  [ `Add
  | `And
  | `Bit_and
  | `Bit_or
  | `Bit_xor
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
  | `Divide
  | `Equal
  | `Greater
  | `Greater_equal
  | `Identity
  | `Int_max
  | `Int_min
  | `Less
  | `Less_equal
  | `Multiply
  | `Negate
  | `Not
  | `Not_equal
  | `Or
  | `Pred
  | `Remainder
  | `Shift_left
  | `Shift_right_arithmetic
  | `Shift_right_logical
  | `Subtract
  | `Succ ]

val of_primitive : path:Path.t -> string -> t option
val of_path : Path.t -> t option
val is_bigint_type : Path.t -> bool

(* The carriers at which an operation can also be EXECUTED as part of a
   run-time refinement assumption.  Ordinary [int] is modelled as a signed
   63-bit bitvector, so machine arithmetic denotes exactly what the backends
   reason about; the carriers with no such correspondence are absent. *)
type carrier =
  | Int
  | Bool

val runtime_result : t -> operands:carrier list -> carrier option
(** [runtime_result builtin ~operands] is the carrier of [builtin]'s result
    when evaluating it at these operand carriers produces exactly the value
    its logical model denotes, and [None] when the two can differ or the
    operation has no executable meaning at these operands. *)

(** Every recognized operation is pure and deterministic.  The verifier's
    stable-call classification relies on this property, so additions to either
    classifier must preserve it.  [of_path] uses persistent identifier
    identity; printed or shadowed names are never sufficient.

    Division and remainder are pure and deterministic in that sense: the same
    operands always give the same outcome.  They are not total — a zero
    divisor raises — so each backend guards the zero case and hands it to an
    uninterpreted symbol rather than to a value.  Recognizing them here does
    not make them total anywhere else: totality is read off the typed mode,
    which this classification does not touch. *)

(* A match arm that did not fire contributes the fact that its scrutinee is
   not that arm's constructor, carried as an application of a function with
   this prefix.  Both backends recognise it and neither depends on the
   other. *)
val constructor_mismatch_prefix : string
val constructor_mismatch_name : string -> string
val constructor_mismatch : string -> string option
