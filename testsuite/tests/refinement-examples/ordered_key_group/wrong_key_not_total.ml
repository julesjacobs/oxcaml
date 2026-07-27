(* [int_key] with one thing changed: the comparison no longer separates
   distinct keys.  Everything compares equal to everything, so the relation
   it induces is a preorder rather than a total order, and
   [compare_zero_iff_equal] is false at any two distinct keys. *)
module M : Key_intf.ORDERED_KEY = struct
  type t = int

  let witness = 0

  let[@vox.def] compare (_left : int) (_right : int) = 0

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
