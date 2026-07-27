(* [int_key] with neighbours swapped: keys one apart compare the wrong way
   round and everything else compares as usual.

   Named for the law it breaks.  This is the interesting one of the three,
   because the comparison is a genuine sign-coherent relation that separates
   distinct keys -- both of the other laws are true of it -- and it fails
   only because [2 < 1] and [1 < 0] hold while [0 < 2] does.  The solver
   refutes [compare_negative_transitive] without being shown that triple.

   The broken law is written last, as in the other two fixtures, so its
   recorded failure is also a record that the other two verified. *)
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

(* What the pinned reference establishes.  The recorded verdict is
   [not-proved], not [disproved], and the distinction is worth reading
   correctly.  The obligation has free key variables, so the law is false at
   some pairs and true at others: the solver reports the prove query
   satisfiable, which is a countermodel, and the disprove query satisfiable
   too, which is why it cannot be [disproved].  [not-proved] is the strongest
   verdict available for a contingent goal.

   It is also not a solver coverage gap dressed up as a rejection.  A gap
   reports [unknown], [solver-error] or [unavailable], each of which renders
   as a different token in the compiler message, and the reference pins the
   token -- so a fixture that stopped being refuted and started being merely
   unanswerable would fail this test rather than keep passing. *)
