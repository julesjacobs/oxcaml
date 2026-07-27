(* [int_key] with one thing changed: neighbours are swapped.  Keys one
   apart compare the wrong way round and everything else compares as
   usual, so distinct keys are still separated and the relation is still
   antisymmetric, but [2 < 1] and [1 < 0] hold while [0 < 2] does, and
   [compare_negative_transitive] is false at that triple. *)
module M : Key_intf.ORDERED_KEY = struct
  type t = int

  let witness = 0

  external int_equal : int -> int -> bool @@ total = "%equal"
  external int_less : int -> int -> bool @@ total = "%lessthan"
  external int_add : int -> int -> int @@ total = "%addint"

  let[@vox.def] compare (left : int) (right : int) =
    if int_equal left right
    then 0
    else if int_equal right (int_add left 1)
    then 1
    else if int_equal left (int_add right 1)
    then -1
    else if int_less left right
    then -1
    else 1

  let compare_zero_iff_equal ~(left : int) ~(right : int)
      : unit{ (compare left right = 0) = (left = right) } =
    let _definition = compare_def left right in
    ()

  let compare_sign_reversal ~(left : int) ~(right : int)
      : unit{
        (compare left right < 0) = (compare right left > 0)
      } =
    let _forward = compare_def left right in
    let _reverse = compare_def right left in
    ()

  let compare_negative_transitive ~(first : int) ~(second : int)
      ~(third : int)
      : unit{
        not (compare first second < 0)
        || not (compare second third < 0)
        || compare first third < 0
      } =
    let _first = compare_def first second in
    let _second = compare_def second third in
    let _result = compare_def first third in
    ()
end
