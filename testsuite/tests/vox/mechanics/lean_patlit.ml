(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* vox: literal constructor patterns propagate their equality, and a
   ground earlier arm contributes a negative equality to later arms.
   Before: [C 0] named a fresh unknown for the payload (the [0] was
   lost), and only a shallow single-constructor-over-variables arm
   contributed a "that arm failed" negative -- a literal or deep arm
   contributed nothing. *)

type t =
  | C of int
  | D
[%%expect{|
type t = C of int | D
|}]

(* WART (b): matching [C 0] propagates [x = C 0], so reconstructing
   [C 0] proves the result equals the scrutinee. *)
let idem (x : t) : t{ _ = x } =
  match x with
  | C 0 -> refine_ (C 0)
  | C n -> refine_ (C n)
  | D -> refine_ D
[%%expect{|
val idem : (x : t) -> t{ _ = x } = <fun>
|}]

(* SOUNDNESS (b): the [C 0] arm knows [x = C 0], so returning [C 1]
   cannot prove [_ = x] -- DISPROVED. *)
let bad (x : t) : t{ _ = x } =
  match x with
  | C 0 -> refine_ (C 1)
  | C n -> refine_ (C n)
  | D -> refine_ D
[%%expect{|
Line 3, characters 19-24:
3 |   | C 0 -> refine_ (C 1)
                       ^^^^^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: C 1 = x
Hypotheses:
  x = C 0
Counterexample (validated -- every hypothesis holds and the goal fails here):
  x = C 0
|}]

(* WART (c): the [C n] arm knows [x = C n] and, from the earlier ground
   arm [C 0] failing, [not (x = C 0)] -- hence [n <> 0]. *)
let nz (x : t) : int{ _ <> 0 } =
  match x with
  | C 0 -> refine_ 1
  | C n -> refine_ n
  | D -> refine_ 1
[%%expect{|
val nz : t -> int{ _ <> 0 } = <fun>
|}]

(* WART (c), bare int literal: the catch-all learns [not (m = 0)]. *)
let nzi (m : int) : int{ _ <> 0 } =
  match m with
  | 0 -> refine_ 1
  | n -> refine_ n
[%%expect{|
val nzi : int -> int{ _ <> 0 } = <fun>
|}]

(* SOUNDNESS (c): with NO earlier ground arm, [C n] has no negative
   fact forcing [n <> 0] -- DISPROVED (x could be C 0). *)
let cn (x : t) : int{ _ <> 0 } =
  match x with
  | C n -> refine_ n
  | D -> refine_ 1
[%%expect{|
Line 3, characters 19-20:
3 |   | C n -> refine_ n
                       ^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: n <> 0
Hypotheses:
  x = C n
Counterexample (validated -- every hypothesis holds and the goal fails here):
  n = 0
  x = C 0
|}]
