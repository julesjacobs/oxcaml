open Oxsmt_core

(** A semantic value in the model domain (task board #74). Three kinds, one per {!Sort.t}
    family:

    - [Bool] for [Sort.bool];
    - [Int] for [Sort.int] (mathematical ℤ, held in a native [int]; arithmetic that would
      leave native range is a loud failure in {!Eval}, never a wraparound — I8 spirit);
    - [Uninterp (sort, id)] for an [Uninterpreted] sort of cardinality [k]: [id] is the
      element index in [0, k). Two uninterpreted values are equal iff they share the sort
      and the index. *)

type t =
  | Bool of bool
  | Int of int
  | Uninterp of Sort.t * int

(** Structural value equality. Distinct constructors compare unequal (well-sorted terms
    never mix them under a single [Eq], but the function is total). For [Uninterp] both
    the sort and the element index must match. *)
val equal : t -> t -> bool

val to_string : t -> string
