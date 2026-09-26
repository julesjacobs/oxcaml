(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { expect; }
*)

let fingerprint x : {n : int | 0 <= n && n <= 127} =
  refine_ (x land 127);;
[%%expect{|
val fingerprint : int -> {n : int | (0 <= n) && (n <= 127)} = <fun>
|}]

let disjoint x : {n : int | n = 0} = refine_ (x land (x lxor (-1)));;
[%%expect{|
val disjoint : int -> {n : int | n = 0} = <fun>
|}]

let combine x : {n : int | n = -1} = refine_ (x lor (x lxor (-1)));;
[%%expect{|
val combine : int -> {n : int | n = (-1)} = <fun>
|}]

let cancel x : {n : int | n = 0} = refine_ (x lxor x);;
[%%expect{|
val cancel : int -> {n : int | n = 0} = <fun>
|}]

let wrong x : {n : int | n = 0} = refine_ (x lor 1);;
[%%expect{|
Line 1, characters 34-51:
1 | let wrong x : {n : int | n = 0} = refine_ (x lor 1);;
                                      ^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let clear_first (m : {m : int | 0 <= m && m <= 65535}) :
    {r : int | r = (m land (m - 1)) && 0 <= r && r <= 65535
      && (m = 0 || r < m)} =
  refine_ (m land (m - 1));;
[%%expect{|
val clear_first :
  (m : {m : int | (0 <= m) && (m <= 65535)}) ->
  {r : int
    | (r = (m land (m - 1))) &&
        ((0 <= r) && ((r <= 65535) && ((m = 0) || (r < m))))} =
  <fun>
|}]
