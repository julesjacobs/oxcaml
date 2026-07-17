(* Load-bearing CHC consumer proof for the public interpolation seam. The default CHC
   scoreboard keeps its legacy equality splitting; this executable runs with
   OXSMT_CHC_INTERP_EQ=1 and requires the direct-equality path to produce and admit a
   checked Farkas interpolant. *)

module Engine = Oxsmt_chc.Chc_engine
module Parse = Oxsmt_chc.Chc_parse

let source =
  {|(set-logic HORN)
    (declare-fun P (Int Int) Bool)
    (assert (forall ((x Int)(y Int)) (=> (and (= x 0)(= y 0)) (P x y))))
    (assert (forall ((x Int)(y Int)(a Int)(b Int))
      (=> (and (P x y)(= a (+ x 1))(= b (+ y 1))) (P a b))))
    (assert (forall ((x Int)(y Int)) (=> (and (P x y)(not (= x y))) false)))|}
;;

let () =
  let system = Parse.parse source in
  let result = Engine.solve ~budget:800 ~max_frames:20 system in
  let attempts, farkas, verified, used = Engine.interp_stats () in
  let eq_farkas = Engine.interp_eq_farkas_count () in
  let safe =
    match result.Engine.verdict with
    | Engine.Safe -> true
    | Engine.Unsafe | Engine.Unknown _ -> false
  in
  if safe && attempts > 0 && farkas > 0 && eq_farkas > 0 && verified > 0 && used > 0
  then
    Printf.printf
      "chc_interp_test: safe; attempts=%d farkas=%d eq-farkas=%d verified=%d used=%d\n"
      attempts
      farkas
      eq_farkas
      verified
      used
  else (
    Printf.eprintf
      "chc_interp_test: FAIL safe=%b attempts=%d farkas=%d eq-farkas=%d verified=%d used=%d\n"
      safe
      attempts
      farkas
      eq_farkas
      verified
      used;
    exit 1)
;;
