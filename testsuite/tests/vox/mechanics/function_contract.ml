(* TEST
 flags = "-dump-vc -vox-dry-run";
 expect;
*)

(* vox: [function]-case parameters are CONTRACTS, like the other
   parameter spellings: the case patterns type against the skeleton of
   a refined domain, the domain's predicate is assumed at the
   anonymous parameter, a variable case aliases that parameter, and
   constructor cases get match facts and negations against it. *)

let vpos : int{ _ > 0 } -> {r:int | r > 0} = function
  | y -> refine_ y
[%%expect{|
Line 2, characters 17-18: vox VC:
  goal: y > 0
  hypotheses:
  y > 0
val vpos : int{ _ > 0 } -> int{ _ > 0 } = <fun>
|}]

type t =
  | K of int
  | L
[%%expect{|
type t = K of int | L
|}]

(* The contract plus the arm's match fact make the [L] arm's
   hypotheses contradictory: the dead arm is provable. *)
let kk : t{ not (_ = L) } -> {r:int | r >= 0} = function
  | K n -> refine_ 0
  | L -> refine_ (-1)
[%%expect{|
Line 2, characters 19-20: vox VC:
  goal: 0 >= 0
  hypotheses:
  param = K n
  not (param = L)
Line 3, characters 17-21: vox VC:
  goal: -1 >= 0
  hypotheses:
  param = L
  not (param = L)
val kk : t{ not (_ = L) } -> int{ _ >= 0 } = <fun>
|}]
