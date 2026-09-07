(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { expect; }
 { expect.opt; }
*)

List.map Bigint.to_string
  [0Z; -0Z; 1_234_567_890_123_456_789_012_345_678_901Z;
   -1234567890123456789012345678901Z];;
[%%expect{|
- : string list =
["0"; "0"; "1234567890123456789012345678901";
 "-1234567890123456789012345678901"]
|}]

module Shadow = struct
  module Bigint = struct let of_int _ = "shadowed" end
  let number = 12345678901234567890Z
end;;
[%%expect{|
module Shadow :
  sig
    module Bigint : sig val of_int : 'a -> string end
    val number : Bigint.t
  end
|}]

let _ = Bigint.to_string Shadow.number;;
[%%expect{|
- : string = "12345678901234567890"
|}]

type number = Bigint.t
type positive = {n : number | n > 0Z};;
[%%expect{|
type number = Bigint.t
type positive = {n : number | n > (Bigint.of_int 0)}
|}]

let next (x : number) : {r : number | r > x} =
  let r = Bigint.add x Bigint.one in refine_ r
let numeric_equal (x : number) : {b : bool | b} =
  let y = Bigint.add x Bigint.zero in
  let b = x = y in refine_ b
let compare_equal (x : number) : {r : int | r = 0} =
  let r = Bigint.compare x x in refine_ r;;
[%%expect{|
val next : (x : number) -> {r : number | r > x} = <fun>
val numeric_equal : number -> {b : bool | b} = <fun>
val compare_equal : number -> {r : int | r = 0} = <fun>
|}]

let physical_equal (x : number) : {b : bool | b} =
  let y = Bigint.add x Bigint.zero in
  let b = x == y in refine_ b;;
[%%expect{|
Line 3, characters 20-29:
3 |   let b = x == y in refine_ b;;
                        ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let signed_min () : {r : number | r = -4611686018427387904Z} =
  let r = Bigint.of_int (-4611686018427387904) in refine_ r
let nonnegative_int (x : int) : {r : number | r >= 0Z} =
  let r = if x >= 0 then Bigint.of_int x else Bigint.zero in refine_ r;;
[%%expect{|
val signed_min :
  unit ->
  {r : number
    | r =
        (Bigint.neg
           (Bigint.add
              (Bigint.mul
                 (Bigint.add
                    (Bigint.mul (Bigint.of_int 4) (Bigint.of_int 1000000000))
                    (Bigint.of_int 611686018)) (Bigint.of_int 1000000000))
              (Bigint.of_int 427387904)))} =
  <fun>
val nonnegative_int : int -> {r : number | r >= (Bigint.of_int 0)} = <fun>
|}]

let zero_divisor (x : number) : {r : number | r = x} =
  let q = Bigint.div x 0Z in
  let r = Bigint.modulo x 0Z in
  let result = Bigint.add q r in refine_ result
let euclidean (a : number) (b : number) : {r : number | r = a} =
  let q = Bigint.div a b in
  let r = Bigint.modulo a b in
  let result = Bigint.(b * q + r) in refine_ result;;
[%%expect{|
val zero_divisor : (x : number) -> {r : number | r = x} = <fun>
val euclidean : (a : number) -> number -> {r : number | r = a} = <fun>
|}]

module Shadow_ops = struct let ( + ) = Bigint.sub end;;
[%%expect{|
module Shadow_ops : sig val ( + ) : Bigint.t -> Bigint.t -> Bigint.t end
|}]

let shadowed_open () : {r : number | r = 2Z} =
  let r = Shadow_ops.(1Z + 1Z) in refine_ r;;
[%%expect{|
Line 2, characters 34-43:
2 |   let r = Shadow_ops.(1Z + 1Z) in refine_ r;;
                                      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Definitions = struct
  let[@def] next (x : number) = Bigint.add x 1Z
end;;
[%%expect{|
module Definitions :
  sig
    val next : number -> Bigint.t
    val next_def :
      (x : number) ->
      {u : unit | (next x) === (Bigint.add x (Bigint.of_int 1))}
  end
|}]

let unfolded (x : number) : {r : number | r > x} =
  let r = Definitions.next x in
  let refine_ proof = Definitions.next_def x in
  refine_ r;;
[%%expect{|
val unfolded : (x : number) -> {r : number | r > x} = <fun>
|}]

module Recursion = struct
  let rec (countdown @ total) (n : number) =
    if n > 0Z then countdown Bigint.(n - 1Z) else 0Z
  [@@decreases Bigint.(n + 0Z)]
  let rec (halve @ total) (n : number) =
    if n > 0Z then halve Bigint.(n / 2Z) else 0Z
  [@@decreases n]
end;;
[%%expect{|
module Recursion :
  sig val countdown : number -> Bigint.t val halve : number -> Bigint.t end
|}]

let rec inferred_measure n =
  if false then inferred_measure n else ()
[@@decreases n];;
[%%expect{|
val inferred_measure : int -> unit = <fun>
|}]

let rec negative_measure (n : number) =
  if n > -3Z then negative_measure Bigint.(n - 1Z) else 0Z
[@@decreases n];;
[%%expect{|
Line 2, characters 18-50:
2 |   if n > -3Z then negative_measure Bigint.(n - 1Z) else 0Z
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 3, characters 13-14:
3 | [@@decreases n];;
                 ^
  Required by this decreases attribute
|}]

let rec unchanged (n : number) = unchanged n [@@decreases n];;
[%%expect{|
Line 1, characters 33-44:
1 | let rec unchanged (n : number) = unchanged n [@@decreases n];;
                                     ^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 1, characters 58-59:
1 | let rec unchanged (n : number) = unchanged n [@@decreases n];;
                                                              ^
  Required by this decreases attribute
|}]

let rec refined_measure (bound : {n : number | n >= 0Z}) =
  let refine_ n = bound in
  if n > 0Z then
    let m = Bigint.(n - 1Z) in
    refined_measure (refine_ m)
  else Bigint.zero
[@@decreases let refine_ n = bound in n];;
[%%expect{|
val refined_measure : {n : number | n >= (Bigint.of_int 0)} -> Bigint.t =
  <fun>
|}]

let pattern = function 0Z -> true | _ -> false;;
[%%expect{|
Line 1, characters 23-25:
1 | let pattern = function 0Z -> true | _ -> false;;
                           ^^
Error: Bigint literal patterns are not supported
|}]

let _ = 0x123Z;;
[%%expect{|
Line 1, characters 8-14:
1 | let _ = 0x123Z;;
            ^^^^^^
Error: Bigint literals require decimal digits
|}]
