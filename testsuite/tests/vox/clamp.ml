(* TEST
 has-z3;
 {
   flags = "-extension refinement_types";
   { expect; }
   { expect.opt; }
 }{
   flags = "-extension refinement_types -principal";
   { expect; }
   { expect.opt; }
 }
*)

module Clamp = struct
  let[@def] clamp (lo : int) (hi : int) (x : int) = if x < lo then lo else if hi < x then hi else x

  let (bounds @ total) :
      (lo : int) -> (hi : {hi : int | lo <= hi}) -> (x : int) ->
      {r : int | lo <= r && r <= (let refine_ h = hi in h)} =
    fun lo hi x ->
    let refine_ hi = hi in
    let result = clamp lo hi x in
    let refine_ equation = clamp_def lo hi x in
    refine_ result

  let (identity @ total) (lo : int) (hi : int) (x : int) :
      {u : unit |
        if lo <= x && x <= hi then clamp (lo : int) (hi : int) (x : int) === x else true} =
    let refine_ equation = clamp_def lo hi x in
    let u = () in
    refine_ u

  let (idempotent @ total) (lo : int) (hi : int) (x : int) :
      {u : unit |
        if lo <= hi then
          clamp lo hi (clamp lo hi x) === clamp lo hi x
        else true} =
    let first = clamp lo hi x in
    let refine_ first_equation = clamp_def lo hi x in
    let refine_ second_equation = clamp_def lo hi first in
    let u = () in
    refine_ u
end
;;
[%%expect{|
module Clamp :
  sig
    val clamp : int -> int -> int -> int
    val clamp_def :
      (lo : int) ->
      (hi : int) ->
      (x : int) ->
      {u : unit
        | (clamp lo hi x) ===
            (if x < lo then lo else if hi < x then hi else x)}
    val bounds :
      (lo : int) ->
      ((hi : {hi : int | lo <= hi}) ->
       int -> {r : int | (lo <= r) && (r <= (let refine_ h = hi in h))}) @ total
      stateful
    val identity :
      (lo : int) ->
      (hi : int) ->
      (x : int) ->
      {u : unit
        | if (lo <= x) && (x <= hi)
          then (clamp (lo : int) (hi : int) (x : int)) === x
          else true}
    val idempotent :
      (lo : int) ->
      (hi : int) ->
      (x : int) ->
      {u : unit
        | if lo <= hi
          then (clamp lo hi (clamp lo hi x)) === (clamp lo hi x)
          else true}
  end
|}]

let () =
  let lo = 0 in
  let hi = 10 in
  List.iter (fun x ->
    let refine_ identity = Clamp.identity lo hi x in
    let refine_ idempotent = Clamp.idempotent lo hi x in
    Format.printf "%d -> %d@." x (Clamp.clamp 0 10 x)) [-3; 4; 12]
;;
[%%expect{|
-3 -> 0
4 -> 4
12 -> 10
|}]

let opaque x : {r : int | 0 <= r && r <= 10} =
  let result = Clamp.clamp 0 10 x in
  refine_ result
;;
[%%expect{|
Line 3, characters 2-16:
3 |   refine_ result
      ^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let unordered (lo : int) (hi : int) (x : int) : {r : int | lo <= r && r <= hi} =
  let result = Clamp.clamp lo hi x in
  let refine_ equation = Clamp.clamp_def lo hi x in
  refine_ result
;;
[%%expect{|
Line 4, characters 2-16:
4 |   refine_ result
      ^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
