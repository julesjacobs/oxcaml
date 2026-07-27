(* The integer key.  It is byte-identical to [polyset/ordered_int.ml], and
   the copy is deliberate: this directory needs an integer instance to make
   the "what does abstracting the key cost" comparison against the integer
   family, and an ocamltest directory symlinks its own [readonly_files].
   Referring to the sibling directory's copy is possible -- [iarray_set] does
   it for one module -- but it makes two suites' fixtures move together for
   the sake of 58 lines, and this file has no reason to change.  If
   [ORDERED_KEY] is ever changed, both copies change with it. *)
type t = int

let witness = 0

external int_less : int -> int -> bool @@ total = "%lessthan"
external int_greater : int -> int -> bool @@ total = "%greaterthan"

let[@vox.def] compare (left : int) (right : int) =
  if int_less left right then -1 else if int_greater left right then 1 else 0

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
