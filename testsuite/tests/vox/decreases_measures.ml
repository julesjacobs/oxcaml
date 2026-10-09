(* TEST
 flags = "-extension refinement_types";
 has-z3;
 { expect; }
*)

(* Measures may call total functions, and a tuple is a lexicographic
   measure. *)

module Lengths = struct
  let[@def] rec (length @ total) (l : int list) : Bigint.t =
    match l with [] -> 0Z | _ :: rest -> Bigint.add (length rest) 1Z

  let rec (length_nonnegative @ total) : (l : int list) ->
      {u : unit | length l >= 0Z} = fun l ->
    length_def l;
    match l with [] -> () | _ :: rest -> length_nonnegative rest
end;;
[%%expect{|
module Lengths :
  sig
    val length : int list -> Bigint.t
    val length_def :
      (l : int list) ->
      {u : unit
        | (length l) ===
            (match l with
             | [] -> Bigint.of_int 0
             | _::rest -> Bigint.add (length rest) (Bigint.of_int 1) :
            Bigint.t)}
    val length_nonnegative :
      (l : int list) -> {u : unit | (length l) >= (Bigint.of_int 0)}
  end
|}]

module Merge = struct
  open Lengths

  let rec (merge @ total) (left : int list) (right : int list) : int list =
    match left, right with
    | [], rest | rest, [] -> rest
    | x :: xs, y :: ys ->
      ghost_ (length_def left; length_def right;
        length_nonnegative xs; length_nonnegative ys;
        length_nonnegative left; length_nonnegative right);
      if x <= y then x :: merge xs right else y :: merge left ys
  [@@decreases Bigint.add (length left) (length right)]
end;;
[%%expect{|
module Merge : sig val merge : int list -> int list -> int list end
|}]

module Ackermann = struct
  let[@def] rec (ack @ total) (m : Bigint.t) (n : Bigint.t) : Bigint.t =
    if m <= 0Z then Bigint.add n 1Z
    else if n <= 0Z then ack (Bigint.sub m 1Z) 1Z
    else ack (Bigint.sub m 1Z) (ack m (Bigint.sub n 1Z))
  [@@decreases (m, n)]

  let rec (grid @ total) (row : int) (column : int) : int =
    if row > -3 then grid (row - 1) column
    else if column > 0 then grid row (column - 1)
    else 0
  [@@decreases (row, column)]
end;;
[%%expect{|
module Ackermann :
  sig
    val ack : Bigint.t -> Bigint.t -> Bigint.t
    val ack_def :
      (m : Bigint.t) ->
      (n : Bigint.t) ->
      {u : unit
        | (ack m n) ===
            (if m <= (Bigint.of_int 0)
             then Bigint.add n (Bigint.of_int 1)
             else
               if n <= (Bigint.of_int 0)
               then ack (Bigint.sub m (Bigint.of_int 1)) (Bigint.of_int 1)
               else
                 ack (Bigint.sub m (Bigint.of_int 1))
                   (ack m (Bigint.sub n (Bigint.of_int 1))) : Bigint.t)}
    val grid : int -> int -> int
  end
|}]

module Annotated = struct
  let rec (grid @ total) row column =
    if row > 0 then grid (row - 1) column
    else if column > 0 then grid row (column - 1)
    else 0
  [@@decreases (((row, column) : int * int) : int * int)]

  let rec (mixed @ total) (row : Bigint.t) column =
    if row > 0Z then mixed (Bigint.sub row 1Z) column
    else if column > 0 then mixed row (column - 1)
    else 0
  [@@decreases ((row, column) : Bigint.t * int)]
end;;
[%%expect{|
module Annotated :
  sig val grid : int -> int -> int val mixed : Bigint.t -> int -> int end
|}]

(* The first component must not grow when a later one decreases. *)
let rec swapped (m : Bigint.t) (n : Bigint.t) : Bigint.t =
  if m <= 0Z then Bigint.add n 1Z
  else if n <= 0Z then swapped (Bigint.sub m 1Z) 1Z
  else swapped (Bigint.sub m 1Z) (swapped m (Bigint.sub n 1Z))
[@@decreases (n, m)];;
[%%expect{|
Line 3, characters 23-51:
3 |   else if n <= 0Z then swapped (Bigint.sub m 1Z) 1Z
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample: m = 1Z, n = 0Z)
Line 5, characters 13-19:
5 | [@@decreases (n, m)];;
                 ^^^^^^
  Required by this decreases attribute
|}]

(* A decreasing Bigint component must stay nonnegative. *)
let rec below_zero (m : Bigint.t) (n : int) : int =
  if n > 0 then below_zero (Bigint.sub m 1Z) n else 0
[@@decreases ((m, n) : Bigint.t * int)];;
[%%expect{|
Line 2, characters 16-46:
2 |   if n > 0 then below_zero (Bigint.sub m 1Z) n else 0
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample: n = 1, m = 0Z)
Line 3, characters 13-38:
3 | [@@decreases ((m, n) : Bigint.t * int)];;
                 ^^^^^^^^^^^^^^^^^^^^^^^^^
Required by this decreases attribute
|}]

(* A total function without a definition says nothing about its result. *)
module Opaque = struct
  let (opaque @ total) (n : int) = n
  let rec unknown n = if n > 0 then unknown (n - 1) else 0
  [@@decreases opaque n]
end;;
[%%expect{|
Line 3, characters 36-51:
3 |   let rec unknown n = if n > 0 then unknown (n - 1) else 0
                                        ^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample: n = 1)
Line 4, characters 15-23:
4 |   [@@decreases opaque n]
                   ^^^^^^^^
  Required by this decreases attribute
|}]

(* A refinement introduction in a measure would not be checked. *)
module Introduction = struct
  let (positive @ total) (n : {n : int | n > 0}) = n
  let rec introduction n = if n > 0 then introduction (n - 1) else 0
  [@@decreases positive 1 + n]
end;;
[%%expect{|
Line 4, characters 24-25:
4 |   [@@decreases positive 1 + n]
                            ^
Error: Unsupported decreases expression: a measure cannot contain a refinement introduction
Line 4, characters 15-29:
4 |   [@@decreases positive 1 + n]
                   ^^^^^^^^^^^^^^
  Required by this decreases attribute
|}]

let rec boolean n = if n > 0 then boolean (n - 1) else 0
[@@decreases ((n, true) : int * bool)];;
[%%expect{|
Line 2, characters 18-22:
2 | [@@decreases ((n, true) : int * bool)];;
                      ^^^^
Error: The constructor "true" has type "bool"
       but an expression was expected of type "int"
|}]
