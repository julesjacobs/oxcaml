(* TEST
 flags = "-dump-vc -vox-dry-run";
 expect;
*)

(* vox: simple records (monomorphic, all fields immutable) are
   single-constructor datatypes with named selectors.  Field projection
   [_.px] appears in predicates; construction names the constructor
   term; a functional update projects kept fields out of the base;
   destructuring (by [let] or [match]) contributes per-field facts. *)

type point =
  { px : int
  ; py : int
  }

let origin : point{ _.px = 0 && _.py = 0 } = refine_ { px = 0; py = 0 }
[%%expect{|
type point = { px : int; py : int; }
Line 6, characters 53-71: vox VC:
  goal: ((mk (0, 0)).px = 0) && ((mk (0, 0)).py = 0)
  hypotheses: <none>
val origin : point{ (_.px = 0) && (_.py = 0) } = {px = 0; py = 0}
|}]

(* Dependent spec over projections; the record literal names itself. *)
let swap : (p : point) -> point{ _.px = p.py && _.py = p.px } =
  fun p -> refine_ { px = p.py; py = p.px }
[%%expect{|
Line 2, characters 19-43: vox VC:
  goal: ((mk (p.py, p.px)).px = p.py) && ((mk (p.py, p.px)).py = p.px)
  hypotheses:
  (origin.px = 0) && (origin.py = 0)
val swap : (p : point) -> point{ (_.px = p.py) && (_.py = p.px) } = <fun>
|}]

(* Functional update: kept fields project out of the base. *)
let setx : (p : point) -> point{ _.px = 3 && _.py = p.py } =
  fun p -> refine_ { p with px = 3 }
[%%expect{|
Line 2, characters 19-36: vox VC:
  goal: ((mk (3, p.py)).px = 3) && ((mk (3, p.py)).py = p.py)
  hypotheses:
  (origin.px = 0) && (origin.py = 0)
val setx : (p : point) -> point{ (_.px = 3) && (_.py = p.py) } = <fun>
|}]

(* Destructuring let: per-field facts (partial patterns are fine). *)
let getx (p : point{ _.px = 7 }) : {r:int | r = 7} =
  let refine_ q = p in
  let { px; py = _ } = q in
  refine_ px
[%%expect{|
Line 4, characters 10-12: vox VC:
  goal: px = 7
  hypotheses:
  px = q.px
  q.px = 7
  p.px = 7
  (origin.px = 0) && (origin.py = 0)
val getx : point{ _.px = 7 } -> int{ _ = 7 } = <fun>
|}]

(* Same through a match. *)
let getx2 (p : point{ _.px = 7 }) : {r:int | r = 7} =
  let refine_ q = p in
  match q with
  | { px; _ } -> refine_ px
[%%expect{|
Line 4, characters 25-27: vox VC:
  goal: px = 7
  hypotheses:
  px = q.px
  q.px = 7
  p.px = 7
  (origin.px = 0) && (origin.py = 0)
val getx2 : point{ _.px = 7 } -> int{ _ = 7 } = <fun>
|}]

(* ADTs and records compose: injectivity of Pt links the payloads. *)
type shape =
  | Pt of point
  | Nothing

let compose (v : point{ _.px = 1 }) : {r:int | r = 1} =
  let refine_ q = v in
  let refine_ s = (refine_ (Pt q) : shape{ _ = Pt q }) in
  match s with
  | Pt w -> let { px; _ } = w in refine_ px
  | Nothing -> assume_ 0
[%%expect{|
type shape = Pt of point | Nothing
Line 7, characters 27-33: vox VC:
  goal: (Pt q) = (Pt q)
  hypotheses:
  q.px = 1
  v.px = 1
  (origin.px = 0) && (origin.py = 0)
Line 9, characters 41-43: vox VC:
  goal: px = 1
  hypotheses:
  px = w.px
  s = (Pt w)
  s = (Pt q)
  q.px = 1
  v.px = 1
  (origin.px = 0) && (origin.py = 0)
Line 10, characters 23-24: vox VC (RUNTIME CHECKED):
  goal: 0 = 1
  hypotheses:
  s = Nothing
  not (s is Pt)
  s = (Pt q)
  q.px = 1
  v.px = 1
  (origin.px = 0) && (origin.py = 0)
val compose : point{ _.px = 1 } -> int{ _ = 1 } = <fun>
|}]

(* A mutable field disqualifies the record from precise tracking:
   its fields may not appear in predicates... *)
type mrec = { mutable m : int }

let bad : mrec{ _.m = 3 } = assume_ { m = 3 }
[%%expect{|
type mrec = { mutable m : int; }
Line 3, characters 16-19:
3 | let bad : mrec{ _.m = 3 } = assume_ { m = 3 }
                    ^^^
Error: vox: only fields of simple records (monomorphic, no mutable fields) may appear in refinement predicates
|}]

(* ... and reads of mutable fields stay fresh unknowns (sound). *)
let opaque (r : mrec) : {v:int | v = 3} =
  r.m <- 3;
  refine_ (r.m)
[%%expect{|
Line 3, characters 10-15: vox VC:
  goal: *unknown1* = 3
  hypotheses:
  (origin.px = 0) && (origin.py = 0)
val opaque : mrec -> int{ _ = 3 } = <fun>
|}]

(* Parameterized records are not simple. *)
type 'a box = { contents : 'a }

let badbox : int box{ _.contents = 3 } = assume_ { contents = 3 }
[%%expect{|
type 'a box = { contents : 'a; }
Line 3, characters 22-32:
3 | let badbox : int box{ _.contents = 3 } = assume_ { contents = 3 }
                          ^^^^^^^^^^
Error: vox: only fields of simple records (monomorphic, no mutable fields) may appear in refinement predicates
|}]
