(* TEST
 has-z3;
 flags = "-extension refinement_types -smt-resource-limit 150000";
 { expect; }
*)

(* A query with a bitwise operation is first proved with the operations
   abstracted, and only then with every integer as a 63-bit bitvector. The
   capacity's [land] fact alone used to make these linear proofs bit-blast,
   at 0.3M to 1M resource units, over this file's limit. *)

type capacity = {c : int | 16 <= c && c <= 1073741824 && c land (c - 1) = 0}

module Linear = struct
  let (middle @ total) (capacity : capacity)
      (lo : {i : int | 0 <= i && i < capacity})
      (hi : {j : int | lo <= j && j < capacity}) :
      {r : int | lo <= r && r <= hi && (r - lo) + (r - lo) <= hi - lo + 1} =
    lo + Int.Refined.((hi - lo) lsr 1)

  let (distance @ total) (capacity : capacity)
      (home : {i : int | 0 <= i && i < capacity})
      (slot : {i : int | 0 <= i && i < capacity}) :
      {r : int | 0 <= r && r < capacity &&
        (slot >= home || r = slot + capacity - home)} =
    if slot >= home then slot - home else slot + capacity - home
end;;
[%%expect{|
type capacity =
    {c : int | (16 <= c) && ((c <= 1073741824) && ((c land (c - 1)) = 0))}
module Linear :
  sig
    val middle :
      (capacity : capacity) ->
      (lo : {i : int | (0 <= i) && (i < capacity)}) ->
      (hi : {j : int | (lo <= j) && (j < capacity)}) ->
      {r : int
        | (lo <= r) &&
            ((r <= hi) && (((r - lo) + (r - lo)) <= ((hi - lo) + 1)))}
    val distance :
      (capacity : capacity) ->
      (home : {i : int | (0 <= i) && (i < capacity)}) ->
      (slot : {i : int | (0 <= i) && (i < capacity)}) ->
      {r : int
        | (0 <= r) &&
            ((r < capacity) &&
               ((slot >= home) || (r = ((slot + capacity) - home))))}
  end
|}]

(* Facts about the abstracted operations: masks, low masks, shifts by a
   constant and shifts by a count in range. *)
module Facts = struct
  let (wrap @ total) (capacity : capacity) (i : int) :
      {r : int | 0 <= r && r < capacity} = i land (capacity - 1)

  let (split @ total) (x : {x : int | 0 <= x}) :
      {q : int | 0 <= x - 16 * q && x - 16 * q < 16} = Int.Refined.(x lsr 4)

  let (low_negative @ total) (x : {x : int | x < 0}) :
      {r : int | 0 <= r && r < 8 && (x - r) land 7 = 0} = x land 7

  let (top @ total) (x : {x : int | x < 0}) :
      {r : int | 4 <= r && r <= 7} = Int.Refined.(x lsr 60)

  let (halve_by @ total) (x : {x : int | x >= 0})
      (k : {k : int | 0 <= k && k <= 63}) : {r : int | 0 <= r && r <= x} =
    Int.Refined.(x lsr k)

  let (sign @ total) (x : {x : int | x < 0}) (y : int) :
      {r : int | r < 0} = x lor y
end;;
[%%expect{|
module Facts :
  sig
    val wrap :
      (capacity : capacity) -> int -> {r : int | (0 <= r) && (r < capacity)}
    val split :
      (x : {x : int | 0 <= x}) ->
      {q : int | (0 <= (x - (16 * q))) && ((x - (16 * q)) < 16)}
    val low_negative :
      (x : {x : int | x < 0}) ->
      {r : int | (0 <= r) && ((r < 8) && (((x - r) land 7) = 0))}
    val top : {x : int | x < 0} -> {r : int | (4 <= r) && (r <= 7)}
    val halve_by :
      (x : {x : int | x >= 0}) ->
      {k : int | (0 <= k) && (k <= 63)} -> {r : int | (0 <= r) && (r <= x)}
    val sign : {x : int | x < 0} -> int -> {r : int | r < 0}
  end
|}]

(* Goals that depend on the bits are proved by the exact encoding, and false
   claims are refuted by it. *)
module Exact = struct
  let (disjoint @ total) (x : int) : {n : int | n = 0} = x land (x lxor (-1))
end;;
[%%expect{|
module Exact : sig val disjoint : int -> {n : int | n = 0} end
|}]

let wrong (x : int) : {r : int | r <= x} = x lor 1;;
[%%expect{|
Line 1, characters 43-50:
1 | let wrong (x : int) : {r : int | r <= x} = x lor 1;;
                                               ^^^^^^^
Error: Refinement could not be proved (counterexample: x = 0)
Line 1, characters 33-39:
1 | let wrong (x : int) : {r : int | r <= x} = x lor 1;;
                                     ^^^^^^
  The refinement is stated here.
|}]
