(* TEST
 has-z3;
 flags = "-extension refinement_types";
 timeout = "10";
 { expect; }
 { expect.opt; }
*)

let conditional_fact : (x : int) -> (b : bool) ->
    {u : unit | if b then x = 0 && true else x = 1 || false}
      @ immutable contended =
  fun x b ->
  let u = () in
  assume_ u

let collapsed_paths : (x : int) -> (b : bool) ->
    {n : int | if b then n = 0 else n = 1} @ immutable contended =
  fun x b ->
  let p1 = conditional_fact x b in
  let p2 = conditional_fact x b in
  let p3 = conditional_fact x b in
  let p4 = conditional_fact x b in
  let p5 = conditional_fact x b in
  let p6 = conditional_fact x b in
  let p7 = conditional_fact x b in
  let p8 = conditional_fact x b in
  let p9 = conditional_fact x b in
  let p10 = conditional_fact x b in
  let refine_ p1 = p1 in
  let refine_ p2 = p2 in
  let refine_ p3 = p3 in
  let refine_ p4 = p4 in
  let refine_ p5 = p5 in
  let refine_ p6 = p6 in
  let refine_ p7 = p7 in
  let refine_ p8 = p8 in
  let refine_ p9 = p9 in
  let refine_ p10 = p10 in
  refine_ x;;
[%%expect{|
val conditional_fact :
  (x : int) ->
  (b : bool) ->
  {u : unit | if b then (x = 0) && true else (x = 1) || false} @ immutable =
  <fun>
val collapsed_paths :
  int -> (b : bool) -> {n : int | if b then n = 0 else n = 1} @ immutable =
  <fun>
|}]
