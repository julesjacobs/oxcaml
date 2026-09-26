(* TEST
 has-z3;
 flags = "-extension refinement_types";
 all_modules = "clamp_api.mli clamp_api.ml";
 readonly_files = "clamp.ml";
 compile_only = "true";
 {
   setup-ocamlc.opt-build-env;
   ocamlc.opt;
   binary_modules = "clamp_api";
   run-expect;
   check-program-output;
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
    let ordered : {hi : int | lo <= hi} = hi in
    let result = Clamp.bounds lo ordered x in
    Format.printf "%d -> %d@." x result) [-3; 4; 12]
;;
[%%expect{|
-3 -> 0
4 -> 4
12 -> 10
|}]

let opaque x : {r : int | 0 <= r && r <= 10} =
  let result = Clamp.clamp 0 10 x in
  result
;;
[%%expect{|
Line 3, characters 2-8:
3 |   result
      ^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let unordered (lo : int) (hi : int) (x : int) : {r : int | lo <= r && r <= hi} =
  let result = Clamp.clamp lo hi x in
  Clamp.clamp_def lo hi x;
  result
;;
[%%expect{|
Line 4, characters 2-8:
4 |   result
      ^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
