(* A key that is not an [int]: a pair ordered lexicographically.

   Every other key in this directory is an [int], so without this one
   nothing checks that the functors are generic rather than accidentally
   int-shaped.  It is also the only comparison here that is not a modelled
   primitive in disguise: proving its laws needs the case analysis over both
   fields rather than one instantiation of [compare_def] at a pair of
   ints. *)
type t = { major : int; minor : int }

let witness = { major = 0; minor = 0 }

external int_less : int -> int -> bool @@ total = "%lessthan"
external int_greater : int -> int -> bool @@ total = "%greaterthan"

let[@vox.def] compare (left : t @ logical) (right : t @ logical) =
  if int_less left.major right.major
  then -1
  else if int_greater left.major right.major
  then 1
  else if int_less left.minor right.minor
  then -1
  else if int_greater left.minor right.minor
  then 1
  else 0

(* [compare_zero_iff_equal] is proved in two steps, and the split is
   necessary rather than tidy.  Written as one obligation -- from the four
   comparisons being false, conclude the two records are equal -- z3 4.8.5
   does not answer within the compiler's budget.  Split into "the four
   comparisons being false makes the fields equal" and "equal fields make
   the records equal", the same solver answers each in hundredths of a
   second.  Reduced to bare SMT the same split holds: the combined query
   times out at thirty seconds, the halves take 0.06s and 0.01s.  Two
   64-bit fields is where it breaks; one field, or two narrow fields, are
   both immediate.  So this is a limitation of the bitvector model against
   record extensionality, not of the interface, and the cost of working
   around it is these two extra lemmas. *)
let fields_agree_when_zero ~(left : t @ logical) ~(right : t @ logical)
    : unit{
      not (compare left right = 0)
      || (left.major = right.major && left.minor = right.minor)
    } =
  let _definition = compare_def left right in
  ()

let equal_when_fields_agree ~(left : t @ logical) ~(right : t @ logical)
    : unit{
      not (left.major = right.major && left.minor = right.minor)
      || left = right
    } =
  ()

let zero_when_equal ~(left : t @ logical) ~(right : t @ logical)
    : unit{ not (left = right) || compare left right = 0 } =
  let _definition = compare_def left right in
  ()

let compare_zero_iff_equal ~(left : t @ logical) ~(right : t @ logical)
    : unit{ (compare left right = 0) = (left = right) } =
  fields_agree_when_zero ~left ~right;
  equal_when_fields_agree ~left ~right;
  zero_when_equal ~left ~right;
  ()

let compare_sign_reversal ~(left : t @ logical) ~(right : t @ logical)
    : unit{
      (compare left right < 0) = (compare right left > 0)
    } =
  let _forward = compare_def left right in
  let _reverse = compare_def right left in
  ()

let compare_negative_transitive ~(first : t @ logical)
    ~(second : t @ logical) ~(third : t @ logical)
    : unit{
      not (compare first second < 0)
      || not (compare second third < 0)
      || compare first third < 0
    } =
  let _first = compare_def first second in
  let _second = compare_def second third in
  let _result = compare_def first third in
  ()
