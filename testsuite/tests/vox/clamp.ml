(* TEST
 has-z3;
 modules = "clamp_api.ml";
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

module Clamp = Clamp_api
;;
[%%expect{|
module Clamp = Clamp_api
|}]

let () =
  let lo = 0 in
  let hi = 10 in
  List.iter (fun (x : int) ->
    ghost_ (Clamp.identity lo hi x);
    ghost_ (Clamp.idempotent lo hi x);
    let ordered : {hi : int | lo <= hi} = refine_ hi in
    let refine_ result = Clamp.bounds lo ordered x in
    Format.printf "%d -> %d@." x result) [-3; 4; 12]
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
  Clamp.clamp_def lo hi x;
  refine_ result
;;
[%%expect{|
Line 4, characters 2-16:
4 |   refine_ result
      ^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
