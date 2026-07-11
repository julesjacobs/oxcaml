(** Sorts (ADR-0003 Decision 6). [private] variant: deep matching is allowed, construction
    goes through the smart constructors so [equal]/[hash] stay O(1). [int_kind] is the §1
    width hook; v1 has only [Mathematical] (unbounded ℤ). Uninterpreted sorts are 0-arity
    in v1. *)

type t = private
  | Bool
  | Int of int_kind
  | Uninterpreted of Symbol.t

and int_kind = Mathematical

val bool : t
val int : t
val uninterpreted : Symbol.t -> t
val equal : t -> t -> bool
val hash : t -> int
