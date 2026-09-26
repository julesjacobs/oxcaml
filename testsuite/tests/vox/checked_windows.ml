(* TEST
 has-z3;
 {
   flags = "-extension refinement_types";
   { expect; }
 }
*)

module Window = struct
  external ( <= ) : int -> int -> bool @@ total = "%lessequal"
  let width :
      (length : int) -> (start : int) -> (stop : int) ->
      {width : int |
        width = stop - start && 0 <= width && width <= length} =
    fun length start stop ->
    let length : {n : int | 0 <= n} = assume_ length in

    let start : {n : int | 0 <= n && n <= length} = assume_ start in

    let stop : {n : int | start <= n && n <= length} = assume_ stop in

    let width = stop - start in
    width

  let remaining (length : int) (start : int) (stop : int) (count : int) =
    let width = width length start stop in
    let count : {n : int | 0 <= n && n <= width} = assume_ count in

    let remaining = width - count in
    let remaining : {n : int | 0 <= n && n <= length} = remaining in

    remaining
end
;;
[%%expect{|
module Window :
  sig
    external ( <= ) : int -> int -> bool = "%lessequal"
    val width :
      (length : int) ->
      (start : int) ->
      (stop : int) ->
      {width : int
        | (width = (stop - start)) && ((0 <= width) && (width <= length))}
    val remaining : int -> int -> int -> int -> int
  end
|}]

let () =
  let length = 10 in
  let start = 4 in
  let stop = 10 in
  let empty = Window.width length start start in
  let suffix = Window.width length start stop in
  Format.printf "empty=%d suffix=%d remaining=%d@."
    empty suffix (Window.remaining 10 4 10 2);
  try
    let bad_start = 8 in
    let _ = Window.width length bad_start start in
    Format.printf "accepted@."
  with Assert_failure _ -> Format.printf "invalid window@."
;;
[%%expect{|
empty=0 suffix=6 remaining=4
invalid window
|}]

let unchecked start stop : {width : int | 0 <= width} =
  let width = stop - start in
  width
;;
[%%expect{|
Line 3, characters 2-7:
3 |   width
      ^^^^^
Error: Refinement could not be proved (counterexample)
|}]
