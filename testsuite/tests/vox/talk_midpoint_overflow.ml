(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { expect; }
*)

(* Talk, section 2 (claim 6, "the logic matches the machine"): the classic
   binary-search midpoint. [int] is exact 63-bit wrapping arithmetic in the
   logic, so [(left + right) / 2] is rejected with a counterexample in which
   [upper = max_int] and [left + right] wraps, while [left + half] is
   accepted. The code is [Binary] from sorted_array_proofs.ml; only the line
   computing [middle] differs between the two phrases. *)

module Overflowing = struct
  external divide : int -> {d : int | d <> 0} -> int @@ total = "%divint"

  let (search_midpoint @ total) :
      (p : (int -> bool)) ->
      (lower : int) -> (upper : int) ->
      {u : unit | -1 <= lower && lower < upper && 0 < upper - lower
        && not (p lower) && p upper} @ ghost ->
      {result : int * int | match result with left, right ->
        lower <= left && right <= upper && right = left + 1
        && not (p left) && p right} =
    fun p lower upper premise ->
    premise;
    let (evaluate @ total) :
        (index : {i : int | lower < i && i < upper}) ->
        {b : bool | let i = index in b = p i} =
      fun index ->
      let i = index in
      let b = p i in
      b
    in
    let rec (loop @ total) :
        (left : int) -> (right : int) ->
        {u : unit | -1 <= left && 0 < right - left
          && lower <= left && left < right && right <= upper
          && not (p left) && p right} @ ghost ->
        {result : int * int | match result with l, r ->
          left <= l && r <= right && r = l + 1
          && not (p l) && p r} =
      fun left right invariant ->
      let distance = right - left in
      if distance <= 1 then
        let result = left, right in
        result
      else
        let two = 2 in
        let half = divide distance (two) in
        let middle = divide (left + right) (two) in
        let index : {i : int | lower < i && i < upper} =
          middle in
        let yes = evaluate index in
        if yes then
          let result = loop left middle () in
          result
        else
          let result = loop middle right () in
          result
    [@@decreases right - left]
    in
    let result = loop lower upper () in
    result

end;;
[%%expect{|
Line 2, characters 2-73:
2 |   external divide : int -> {d : int | d <> 0} -> int @@ total = "%divint"
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 228 [trusted-external]: The verifier assumes this external's refinement;
  nothing checks it.

Line 37, characters 12-16:
37 |         let half = divide distance (two) in
                 ^^^^
Warning 26 [unused-var]: unused variable "half".

Line 40, characters 10-16:
40 |           middle in
               ^^^^^^
Error: Refinement could not be proved (counterexample: lower = 4611686018427326839, upper = 4611686018427387903, left = 4611686018427326839, right = 4611686018427369316)
Line 39, characters 31-40:
39 |         let index : {i : int | lower < i && i < upper} =
                                    ^^^^^^^^^
  The refinement is stated here.
|}]

(* The shipped midpoint, [left + half], is accepted. *)
module Safe = struct
  external divide : int -> {d : int | d <> 0} -> int @@ total = "%divint"

  let (search_midpoint @ total) :
      (p : (int -> bool)) ->
      (lower : int) -> (upper : int) ->
      {u : unit | -1 <= lower && lower < upper && 0 < upper - lower
        && not (p lower) && p upper} @ ghost ->
      {result : int * int | match result with left, right ->
        lower <= left && right <= upper && right = left + 1
        && not (p left) && p right} =
    fun p lower upper premise ->
    premise;
    let (evaluate @ total) :
        (index : {i : int | lower < i && i < upper}) ->
        {b : bool | let i = index in b = p i} =
      fun index ->
      let i = index in
      let b = p i in
      b
    in
    let rec (loop @ total) :
        (left : int) -> (right : int) ->
        {u : unit | -1 <= left && 0 < right - left
          && lower <= left && left < right && right <= upper
          && not (p left) && p right} @ ghost ->
        {result : int * int | match result with l, r ->
          left <= l && r <= right && r = l + 1
          && not (p l) && p r} =
      fun left right invariant ->
      let distance = right - left in
      if distance <= 1 then
        let result = left, right in
        result
      else
        let two = 2 in
        let half = divide distance (two) in
        let middle = left + half in
        let index : {i : int | lower < i && i < upper} =
          middle in
        let yes = evaluate index in
        if yes then
          let result = loop left middle () in
          result
        else
          let result = loop middle right () in
          result
    [@@decreases right - left]
    in
    let result = loop lower upper () in
    result

end;;
[%%expect{|
Line 2, characters 2-73:
2 |   external divide : int -> {d : int | d <> 0} -> int @@ total = "%divint"
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 228 [trusted-external]: The verifier assumes this external's refinement;
  nothing checks it.

module Safe :
  sig
    external divide : int -> {d : int | d <> 0} -> int = "%divint"
    val search_midpoint :
      (p : (int -> bool)) ->
      ((lower : int) ->
       (upper : int) ->
       {u : unit
         | ((-1) <= lower) &&
             ((lower < upper) &&
                ((0 < (upper - lower)) && ((not (p lower)) && (p upper))))} @ ghost ->
       {result : int * int
         | match result with
           | (left, right) ->
               (lower <= left) &&
                 ((right <= upper) &&
                    ((right = (left + 1)) && ((not (p left)) && (p right))))}) @ total
      stateful
  end
|}]
