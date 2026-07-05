(* TEST
 flags = "-dump-vc -vox-dry-run";
 expect;
*)

(* vox: PARAMETERIZED simple datatypes.  A parameterized variant/record
   is declared to the solver once, generically (its type parameters
   become explicit [Type] binders); each USE instantiates it at its
   argument sorts.  Constructors and fields work structurally at every
   instantiation, exactly as for a monomorphic simple datatype. *)

type 'a mylist =
  | Nil
  | Cons of 'a * 'a mylist

(* Match facts at the [int] instantiation: injectivity of [Cons] proves
   [h = 3]. *)
let get_i (s : (int mylist){ _ = Cons (3, Nil) }) : {r:int | r = 3} =
  match s with
  | Cons (h, t) -> refine_ h
  | Nil -> assume_ 0
[%%expect{|
type 'a mylist = Nil | Cons of 'a * 'a mylist
Line 9, characters 27-28: vox VC:
  goal: h = 3
  hypotheses:
  s = (Cons (h, t))
  s = (Cons (3, Nil))
Line 10, characters 19-20: vox VC (RUNTIME CHECKED):
  goal: 0 = 3
  hypotheses:
  s = Nil
  s = (Cons (3, Nil))
val get_i : int mylist{ _ = (Cons (3, Nil)) } -> int{ _ = 3 } = <fun>
|}]

(* The SAME declaration reused at the [bool] instantiation (the solver
   sees one generic [mylist]); a wildcard field names a fresh unknown. *)
let hd_or (s : bool mylist) (d : bool) : bool =
  match s with
  | Cons (h, _) -> h
  | Nil -> d
[%%expect{|
val hd_or : bool mylist -> bool -> bool = <fun>
|}]

(* A parameterized RECORD: its projection works at [int box]. *)
type 'a box = { v : 'a }

let getv (b : (int box){ _.v = 5 }) : {r:int | r = 5} = refine_ b.v
[%%expect{|
type 'a box = { v : 'a; }
Line 3, characters 64-67: vox VC:
  goal: b.v = 5
  hypotheses:
  b.v = 5
val getv : int box{ _.v = 5 } -> int{ _ = 5 } = <fun>
|}]

(* Escape probe: at a type VARIABLE the argument sort has no parameter
   in scope, so it degrades to the uninterpreted sort (VoxU in the
   solver) -- SOUND, never leaking an [S_param].  The match facts are
   still emitted (structure of [mylist] at an opaque element). *)
let poly (s : 'a mylist) : {r:int | r = 0} =
  match s with
  | Cons (h, t) -> refine_ 0
  | Nil -> refine_ 0
[%%expect{|
Line 3, characters 27-28: vox VC:
  goal: 0 = 0
  hypotheses:
  s = (Cons (h, t))
Line 4, characters 19-20: vox VC:
  goal: 0 = 0
  hypotheses:
  s = Nil
val poly : 'a mylist -> int{ _ = 0 } = <fun>
|}]
