(* TEST
 flags = "-dump-vc -vox-dry-run";
 expect;
*)

(* vox: implicit subsumption across positions (see also infer.ml).
   An expression at a refined expected type is typed at the skeleton
   and the refinement becomes a proof obligation at its logical name
   -- no keyword -- at let annotations, constructor arguments, record
   fields (construction and update), and function results; a value
   already carrying the same refinement flows through with no
   obligation. *)

(* A let annotation: the obligation comes from the annotation alone. *)
let three : int{ _ = 3 } = 3
[%%expect{|
Line 1, characters 27-28: vox VC:
  goal: 3 = 3
  hypotheses: <none>
val three : int{ _ = 3 } = 3
|}]

(* Flow-through stays rigid: [three] is already refined, so no
   obligation is generated for the alias. *)
let keep : int{ _ = 3 } = three
[%%expect{|
Line 1, characters 26-31: vox VC:
  goal: three = 3
  hypotheses:
  three = 3
val keep : int{ _ = 3 } = 3
|}]

(* A bare variable at a refined annotation gets the obligation. *)
let promote (x : int) =
  let refine_ p = (x : int{ _ = x }) in
  p
[%%expect{|
Line 2, characters 19-20: vox VC:
  goal: x = x
  hypotheses:
  keep = three
  keep = 3
  three = 3
val promote : int -> int = <fun>
|}]

(* Arithmetic at a refined result type: the body needs no keyword. *)
let inc : (x : int) -> int{ _ = x + 1 } = fun x -> x + 1
[%%expect{|
Line 1, characters 51-56: vox VC:
  goal: (x + 1) = (x + 1)
  hypotheses:
  keep = three
  keep = 3
  three = 3
val inc : (x : int) -> int{ _ = (x + 1) } = <fun>
|}]

(* A refined constructor argument. *)
type w =
  | W of {v:int | v > 0}
  | Z
[%%expect{|
type w = W of int{ _ > 0 } | Z
|}]

let w1 : w = W 3
[%%expect{|
Line 1, characters 15-16: vox VC:
  goal: 3 > 0
  hypotheses:
  keep = three
  keep = 3
  three = 3
val w1 : w = W 3
|}]

(* A refined record field, both at construction and at update. *)
type point =
  { px : int{ _ >= 0 }
  ; py : int
  }
[%%expect{|
type point = { px : int{ _ >= 0 }; py : int; }
|}]

let origin = { px = 0; py = 0 }
[%%expect{|
Line 1, characters 20-21: vox VC:
  goal: 0 >= 0
  hypotheses:
  w1 = (W 3)
  keep = three
  keep = 3
  three = 3
val origin : point = {px = 0; py = 0}
|}]

let shift (p : point) (d : int) = { p with px = d * d }
[%%expect{|
Line 1, characters 48-53: vox VC:
  goal: (d * d) >= 0
  hypotheses:
  origin = (mk (0, 0))
  w1 = (W 3)
  keep = three
  keep = 3
  three = 3
val shift : point -> int -> point = <fun>
|}]

(* R1: a LOCAL binder at a visibly refined type binds at the skeleton
   -- the refinement is a fact, and the value is an ordinary int.  (A
   MODULE-LEVEL binder keeps the refined type: the signature is the
   module's contract, see [three] above.) *)
let local_payload () =
  let x : int{ _ > 0 } = 3 in
  x + 1
[%%expect{|
Line 2, characters 25-26: vox VC:
  goal: 3 > 0
  hypotheses:
  origin = (mk (0, 0))
  w1 = (W 3)
  keep = three
  keep = 3
  three = 3
val local_payload : unit -> int = <fun>
|}]

(* A match binder against a refined field binds at the skeleton too:
   [y] is an int, and [y > 0] is a fact. *)
let field_payload (v : w) =
  match v with
  | W y -> y + 1
  | Z -> 0
[%%expect{|
val field_payload : w -> int = <fun>
|}]

(* The fact still discharges obligations. *)
let field_fact (v : w) : int =
  match v with
  | W y ->
    let refine_ ok = (y : int{ _ > 0 }) in
    ok
  | Z -> 1
[%%expect{|
Line 4, characters 22-23: vox VC:
  goal: y > 0
  hypotheses:
  v = (W y)
  y > 0
  origin = (mk (0, 0))
  w1 = (W 3)
  keep = three
  keep = 3
  three = 3
val field_fact : w -> int = <fun>
|}]
