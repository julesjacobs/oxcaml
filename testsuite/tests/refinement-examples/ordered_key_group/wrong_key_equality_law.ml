(* [int_key] with the comparison changed to the constant [0], so distinct
   keys are never separated.

   Named for the law it breaks rather than for the order property, because
   the two do not line up one to one: this comparison is not a total order
   -- distinct keys are incomparable -- but the refinement that catches it is
   [compare_zero_iff_equal], which is false at any two distinct keys.

   The broken law is written LAST on purpose.  Verification stops at the
   first non-proved verdict, so a fixture whose broken law comes first
   establishes nothing about the other two.  Written last, the recorded
   failure at [compare_zero_iff_equal] is also a record that the other two
   laws verified of this comparison: they hold vacuously, since nothing is
   ever below anything. *)
module M : Key_intf.ORDERED_KEY = struct
  type t = int

  let witness = 0

  let[@vox.def] compare (_left : int) (_right : int) = 0

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

  let compare_zero_iff_equal ~(left : int) ~(right : int)
      : unit{ (compare left right = 0) = (left = right) } =
    let _definition = compare_def left right in
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
